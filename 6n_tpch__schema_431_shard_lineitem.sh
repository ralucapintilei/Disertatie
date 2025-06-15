#!/bin/bash

MONGO_PORT=26061
DB_NAME=tpch0_01__schema_431
COLLECTION="lineitem"
SHARD_KEY="l_orderkey"
JSON_FILE="/home/ubuntu/Data/${DB_NAME}___${COLLECTION}.json"

# [1] Drop + cleanup
echo "[1] Drop and clean metadata for $COLLECTION..."
mongosh --quiet --host 172.16.31.200 --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.drop()
use config
db.collections.deleteOne({ _id: "$DB_NAME.$COLLECTION" })
db.chunks.deleteMany({ ns: "$DB_NAME.$COLLECTION" })
EOF

# [2] Create collection + index
echo "[2] Create collection and hashed index..."
mongosh --quiet --host 172.16.31.200 --port $MONGO_PORT <<EOF
use $DB_NAME
db.createCollection("$COLLECTION")
db.$COLLECTION.createIndex({ $SHARD_KEY: "hashed" })
EOF

# [3] Enable sharding + shard collection
echo "[3] Enable sharding and shard the collection..."
mongosh --quiet --host 172.16.31.200 --port $MONGO_PORT <<EOF
sh.enableSharding("$DB_NAME")
sh.shardCollection("$DB_NAME.$COLLECTION", { $SHARD_KEY: "hashed" })
EOF

# [4] Insert dummy document to force chunk creation
echo "[4] Insert dummy to force chunk creation..."
mongosh --quiet --host 172.16.31.200 --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.insertOne({ $SHARD_KEY: NumberLong(-999999999), dummy: true })
EOF

# [5] Delete dummy and import real data
echo "[5] Remove dummy and import data..."
mongosh --quiet --host 172.16.31.200 --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.deleteOne({ dummy: true })
EOF

mongoimport --host 172.16.31.200 --port $MONGO_PORT -d $DB_NAME -c $COLLECTION --file "$JSON_FILE"

# [6] Final distribution check
echo "[6] Final shard distribution check..."
mongosh --quiet --host 172.16.31.200 --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.getShardDistribution()
EOF
