#!/bin/bash

DB="tpch0_01__schema_431"
COLLECTION_ORIGINAL="partsupp_part"
COLLECTION_FLAT="partsupp_part_flat"
SHARD_KEY="ps_partkey"
PORT=26061
INPUT_JSON="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___partsupp_part.json"
FLAT_JSON="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___partsupp_part_flat.json"
MONGO_HOST="127.0.0.1"
SHARDS=(shard1 shard2 shard3)

# === [1] Flatten original documents ===
echo "[1] Flatten JSON into individual documents..."
python3 <<EOF
import json
with open("$INPUT_JSON") as fin, open("$FLAT_JSON", "w") as fout:
    for line in fin:
        doc = json.loads(line)
        part_info = {k: v for k, v in doc.items() if k != "partsupp"}
        for item in doc.get("partsupp", []):
            flat_doc = dict(part_info)
            flat_doc.update(item)
            fout.write(json.dumps(flat_doc) + "\n")
EOF

# === [2] Drop and cleanup ===
echo "[2] Drop collection and clean configs..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.drop()
use config
db.collections.deleteOne({ _id: "$DB.$COLLECTION_FLAT" })
db.chunks.deleteMany({ ns: "$DB.$COLLECTION_FLAT" })
EOF

# === [3] Enable sharding ===
echo "[3] Enable sharding and prepare collection..."
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

# === [5] Split ===
echo "[5] Manual chunk splits..."
CHUNK_KEYS=(
  -7378697629483820646
  -5534023222112865482
  -3689348814741910318
  -1844674407370955154
  0
  1844674407370955154
  3689348814741910318
  5534023222112865482
  7378697629483820646
)
for key in "${CHUNK_KEYS[@]}"; do
  mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong($key) })
EOF
done

# === [6] Distribute chunks ===
echo "[6] Distribute chunks to 3 shards..."
MOVE_MAP=(
  "-9223372036854775808 shard1"
  "-7378697629483820646 shard1"
  "-5534023222112865482 shard1"
  "-3689348814741910318 shard2"
  "-1844674407370955154 shard2"
  "0 shard2"
  "1844674407370955154 shard3"
  "3689348814741910318 shard3"
  "5534023222112865482 shard3"
  "7378697629483820646 shard3"
)

for entry in "${MOVE_MAP[@]}"; do
  KEY=$(echo $entry | cut -d' ' -f1)
  SHARD=$(echo $entry | cut -d' ' -f2)
  echo "→ Moving chunk starting at $KEY → $SHARD"
  mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong($KEY) }, "$SHARD", { _waitForDelete: false })
EOF
done

# === [7] Final check ===
echo "[7] Final shard distribution check..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.getShardDistribution()
EOF