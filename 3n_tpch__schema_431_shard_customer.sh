#!/bin/bash

# === CONFIG ===
MONGO_HOST="127.0.0.1"
MONGO_PORT=26061
DB_NAME="tpch0_01__schema_431"
COLLECTION="customer"
SHARD_KEY="c_custkey"
JSON_FILE="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___customer.json"
SHARDS=(shard1 shard2 shard3)

# === Cleanup ===
echo "[1] Drop collection and clean config metadata..."
mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.drop()
use config
db.collections.deleteOne({ _id: "$DB_NAME.$COLLECTION" })
db.chunks.deleteMany({ ns: "$DB_NAME.$COLLECTION" })
EOF

# === Create and shard collection ===
echo "[2] Enable sharding and create collection..."
mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
sh.enableSharding("$DB_NAME")
use $DB_NAME
db.createCollection("$COLLECTION")
db.$COLLECTION.createIndex({ $SHARD_KEY: "hashed" })
sh.shardCollection("$DB_NAME.$COLLECTION", { $SHARD_KEY: "hashed" })
EOF

# === Import data ===
echo "[3] Import data..."
if [ ! -f "$JSON_FILE" ]; then
  echo "[ERROR] JSON file not found at $JSON_FILE"
  exit 1
fi

mongoimport --host $MONGO_HOST --port $MONGO_PORT -d $DB_NAME -c $COLLECTION --file "$JSON_FILE"

# === Manual split points for 6 chunks ===
echo "[4] Apply 5 fixed split points..."
CHUNK_KEYS=(-6148914691236517203 -3074457345618258602 0 3074457345618258602 6148914691236517203)

for key in "${CHUNK_KEYS[@]}"; do
  mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
sh.splitAt("$DB_NAME.$COLLECTION", { $SHARD_KEY: NumberLong("$key") })
EOF
done

# === Move chunks: 2 to each shard ===
echo "[5] Move chunks evenly to shards..."
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
  mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
sh.moveChunk("$DB_NAME.$COLLECTION", { $SHARD_KEY: NumberLong("$KEY") }, "$SHARD", { _waitForDelete: false })
EOF
done

# === Final check ===
echo "[6] Final distribution check:"
mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.getShardDistribution()
EOF
