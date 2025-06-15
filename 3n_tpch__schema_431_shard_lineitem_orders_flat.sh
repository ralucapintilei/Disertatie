#!/bin/bash

DB="tpch0_01__schema_431"
COLLECTION_ORIGINAL="lineitem_orders"
COLLECTION_FLAT="lineitem_orders_flat"
SHARD_KEY="l_orderkey"
PORT=26061
INPUT_JSON="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___lineitem_orders.json"
FLAT_JSON="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___lineitem_orders_flat.json"
MONGO_HOST="127.0.0.1"
SHARDS=(shard1 shard2 shard3)

# === [1] Flatten original nested documents ===
echo "[1] Flatten JSON into individual documents..."
python3 <<EOF
import json
with open("$INPUT_JSON", "r") as f_in, open("$FLAT_JSON", "w") as f_out:
    for line in f_in:
        doc = json.loads(line)
        order_info = {k: v for k, v in doc.items() if k != "lineitem"}
        for item in doc.get("lineitem", []):
            flat_doc = dict(order_info)
            flat_doc.update(item)
            f_out.write(json.dumps(flat_doc) + "\n")
EOF

# === [2] Drop and cleanup ===
echo "[2] Drop and clean config metadata..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.drop()
use config
db.collections.deleteOne({ _id: "$DB.$COLLECTION_FLAT" })
db.chunks.deleteMany({ ns: "$DB.$COLLECTION_FLAT" })
EOF

# === [3] Shard setup ===
echo "[3] Enable sharding, create collection and index..."
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

# === [5] Manual balanced splits ===
echo "[5] Apply 5 fixed split points..."
CHUNK_KEYS=(-6148914691236517203 -3074457345618258602 0 3074457345618258602 6148914691236517203)
for key in "${CHUNK_KEYS[@]}"; do
  mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("$key") })
EOF
done

# === [6] Balanced chunk moves ===
echo "[6] Move chunks evenly to shards..."
MOVE_MAP=(
  "-9223372036854775808 shard1"
  "-6148914691236517203 shard1"
  "-3074457345618258602 shard2"
  "0 shard2"
  "3074457345618258602 shard3"
  "6148914691236517203 shard3"
)
for entry in "${MOVE_MAP[@]}"; do
  KEY=$(echo $entry | cut -d' ' -f1)
  SHARD=$(echo $entry | cut -d' ' -f2)
  echo "→ Moving chunk starting at $KEY → $SHARD"
  mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("$KEY") }, "$SHARD", { _waitForDelete: false })
EOF
done

# === [7] Final verification ===
echo "[7] Final shard distribution check..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.getShardDistribution()
EOF
