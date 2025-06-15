#!/bin/bash

# === CONFIG ===
MONGO_HOST="127.0.0.1"
MONGO_PORT=26061
DB_NAME="tpch0_01__schema_431"
COLLECTION="customer"
SHARD_KEY="c_custkey"
JSON_FILE="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___customer.json"
SHARDS=(shard1 shard2 shard3 shard4 shard5 shard6)

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

# === Generate and apply splits ===
echo "[4] Generate split points with splitVector..."
SPLIT_KEYS=$(mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
var result = db.adminCommand({
  splitVector: "$DB_NAME.$COLLECTION",
  keyPattern: { $SHARD_KEY: "hashed" },
  maxChunkSize: 1
});
print(result.splitKeys.map(k => k.$SHARD_KEY).join(","));
EOF
)

IFS=',' read -ra SPLIT_ARRAY <<< "$SPLIT_KEYS"

echo "[5] Manual splits..."
for key in "${SPLIT_ARRAY[@]}"; do
  mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
sh.splitAt("$DB_NAME.$COLLECTION", { $SHARD_KEY: NumberLong("$key") })
EOF
done

# === Distribute chunks to shards ===
echo "[6] Move chunks to all shards..."
START_VAL="-9223372036854775808"
i=0
for shard in "${SHARDS[@]}"; do
  mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
sh.moveChunk("$DB_NAME.$COLLECTION", { $SHARD_KEY: NumberLong("$START_VAL") }, "$shard")
EOF
  if [ $i -lt ${#SPLIT_ARRAY[@]} ]; then
    START_VAL=${SPLIT_ARRAY[$i]}
    ((i++))
  fi
done

# === Final check ===
echo "[7] Final distribution check:"
mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.getShardDistribution()
EOF
