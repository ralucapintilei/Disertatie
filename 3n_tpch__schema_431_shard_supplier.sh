#!/bin/bash

DB="tpch0_01__schema_431"
COLLECTION="supplier"
SHARD_KEY="s_suppkey"
PORT=26061
JSON_FILE="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___supplier.json"
MONGO_HOST="127.0.0.1"
SHARDS=(shard1 shard2 shard3)

# === [1] Drop and cleanup ===
echo "[1] Drop & cleanup configs..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
use $DB
db.$COLLECTION.drop()
use config
db.collections.deleteOne({ _id: "$DB.$COLLECTION" })
db.chunks.deleteMany({ ns: "$DB.$COLLECTION" })
EOF

# === [2] Create collection + index ===
echo "[2] Create collection and hashed index..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
use $DB
db.createCollection("$COLLECTION")
db.$COLLECTION.createIndex({ $SHARD_KEY: "hashed" })
EOF

# === [3] Enable sharding ===
echo "[3] Enable sharding and shard the collection..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
sh.enableSharding("$DB")
sh.shardCollection("$DB.$COLLECTION", { $SHARD_KEY: "hashed" })
EOF

# === [4] Import data ===
echo "[4] Import actual data..."
mongoimport --host $MONGO_HOST --port $PORT -d $DB -c $COLLECTION --file "$JSON_FILE"

# === [5] Manual chunk splits ===
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
sh.splitAt("$DB.$COLLECTION", { $SHARD_KEY: NumberLong($key) })
EOF
done

# === [6] Distribute chunks to 3 shards ===
echo "[6] Move chunks to all 3 shards..."
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
sh.moveChunk("$DB.$COLLECTION", { $SHARD_KEY: NumberLong($KEY) }, "$SHARD", { _waitForDelete: false })
EOF
done

# === [7] Final check ===
echo "[7] Final shard distribution check..."
mongosh --quiet --host $MONGO_HOST --port $PORT <<EOF
use $DB
db.$COLLECTION.getShardDistribution()
db.$COLLECTION.count()
EOF
