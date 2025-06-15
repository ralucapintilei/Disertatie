#!/bin/bash

DB="tpch0_01__schema_431"
COLLECTION_ORIGINAL="nation_region"
COLLECTION_FLAT="nation_region_flat"
SHARD_KEY="n_nationkey"
PORT=26061
INPUT_JSON="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___nation_region.json"
FLAT_JSON="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___nation_region_flat.json"
MONGO_HOST="127.0.0.1"
SHARDS=(shard1 shard2 shard3)

# === [1] Flatten original nation documents ===
echo "[1] Flatten JSON into individual nation documents..."
python3 <<EOF
import json

infile = "$INPUT_JSON"
outfile = "$FLAT_JSON"

with open(infile, "r") as f_in, open(outfile, "w") as f_out:
    for line in f_in:
        doc = json.loads(line)
        region_fields = {k: v for k, v in doc.items() if k != "nation"}
        for nation in doc.get("nation", []):
            flat_doc = dict(region_fields)
            flat_doc.update(nation)
            f_out.write(json.dumps(flat_doc) + "\n")
EOF

# === [2] Drop and cleanup ===
echo "[2] Drop DB and clean config metadata..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
use $DB
db.dropDatabase()
use config
db.collections.deleteOne({ _id: "$DB.$COLLECTION_FLAT" })
db.chunks.deleteMany({ ns: "$DB.$COLLECTION_FLAT" })
EOF

# === [3] Shard setup ===
echo "[3] Enable sharding and create collection/index..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
sh.enableSharding("$DB")
use $DB
db.createCollection("$COLLECTION_FLAT")
db.$COLLECTION_FLAT.createIndex({ $SHARD_KEY: "hashed" })
sh.shardCollection("$DB.$COLLECTION_FLAT", { $SHARD_KEY: "hashed" })
EOF

# === [4] Import data ===
echo "[4] Import flattened data..."
mongoimport --host $MONGO_HOST --port $PORT -d $DB -c $COLLECTION_FLAT --file "$FLAT_JSON"

# === [5] Manual chunk splits ===
echo "[5] Apply fixed split points..."
CHUNK_KEYS=(2 5 8 11 14)
for key in "${CHUNK_KEYS[@]}"; do
  mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt($key) })
EOF
done

# === [6] Balanced chunk movement ===
echo "[6] Move chunks to 3 shards..."
MOVE_MAP=(
  "0 shard1"
  "2 shard1"
  "5 shard2"
  "8 shard2"
  "11 shard3"
  "14 shard3"
)
for entry in "${MOVE_MAP[@]}"; do
  KEY=$(echo $entry | cut -d' ' -f1)
  SHARD=$(echo $entry | cut -d' ' -f2)
  echo "→ Moving chunk starting at $KEY → $SHARD"
  mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt($KEY) }, "$SHARD", { _waitForDelete: false })
EOF
done

# === [7] Final check ===
echo "[7] Final shard distribution check..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.getShardDistribution()
EOF