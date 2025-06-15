#!/bin/bash

MONGO_PORT=26061
DB_NAME="tpch0_01__schema_431"
COLLECTION="supplier"
SHARD_KEY="s_suppkey"
JSON_FILE="/home/ubuntu/Data/tpch0_01__schema_431/tpch0_01__schema_431___supplier.json"

echo "[1] Drop & cleanup configs..."
mongosh --quiet --host 172.16.31.200 --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.drop()
use config
db.collections.deleteOne({ _id: "$DB_NAME.$COLLECTION" })
db.chunks.deleteMany({ ns: "$DB_NAME.$COLLECTION" })
EOF

echo "[2] Create collection and hashed index..."
mongosh --quiet --host 172.16.31.200 --port $MONGO_PORT <<EOF
use $DB_NAME
db.createCollection("$COLLECTION")
db.$COLLECTION.createIndex({ $SHARD_KEY: "hashed" })
EOF

echo "[3] Enable sharding and shard collection..."
mongosh --quiet --host 172.16.31.200 --port $MONGO_PORT <<EOF
sh.enableSharding("$DB_NAME")
sh.shardCollection("$DB_NAME.$COLLECTION", { $SHARD_KEY: "hashed" })
EOF

echo "[4] Import actual data..."
mongoimport --host 172.16.31.200 --port $MONGO_PORT -d $DB_NAME -c $COLLECTION --file "$JSON_FILE"

echo "[5] Final distribution check..."
mongosh --quiet --host 172.16.31.200 --port $MONGO_PORT <<EOF
use $DB_NAME
db.$COLLECTION.getShardDistribution()
db.$COLLECTION.count()
EOF
