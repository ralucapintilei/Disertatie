#!/bin/bash

MONGO_PORT=26061
DB_NAME="tpch0_01__schema_431"
COLLECTION="lineitem"
SHARD_KEY="l_orderkey"
JSON_FILE="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___lineitem.json"
MONGO_HOST="127.0.0.1"
SHARDS=(shard1 shard2 shard3)

# [1] Drop + cleanup
echo "[1] Drop and clean metadata for $COLLECTION..."
mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.drop()
use config
db.collections.deleteOne({ _id: "$DB_NAME.$COLLECTION" })
db.chunks.deleteMany({ ns: "$DB_NAME.$COLLECTION" })
EOF

# [2] Create collection + index
echo "[2] Create collection and hashed index..."
mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
use $DB_NAME
db.createCollection("$COLLECTION")
db.$COLLECTION.createIndex({ $SHARD_KEY: "hashed" })
EOF

# [3] Enable sharding + shard collection
echo "[3] Enable sharding and shard the collection..."
mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
sh.enableSharding("$DB_NAME")
sh.shardCollection("$DB_NAME.$COLLECTION", { $SHARD_KEY: "hashed" })
EOF

# [4] Import actual data
echo "[4] Import data..."
mongoimport --host $MONGO_HOST --port $MONGO_PORT -d $DB_NAME -c $COLLECTION --file "$JSON_FILE"

# [5] Manual splits for balanced distribution
echo "[5] Apply 5 fixed split points..."
CHUNK_KEYS=(-6148914691236517203 -3074457345618258602 0 3074457345618258602 6148914691236517203)
for key in "${CHUNK_KEYS[@]}"; do
  mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
sh.splitAt("$DB_NAME.$COLLECTION", { $SHARD_KEY: NumberLong("$key") })
EOF
done

# [6] Move chunks evenly to shards
echo "[6] Move chunks to target shards..."
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
  mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
sh.moveChunk("$DB_NAME.$COLLECTION", { $SHARD_KEY: NumberLong("$KEY") }, "$SHARD", { _waitForDelete: false })
EOF
done

# [7] Final distribution
echo "[7] Final distribution overview:"
mongosh --quiet --host $MONGO_HOST --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.getShardDistribution()
EOF
