#!/bin/bash

DB="tpch0_01__schema_431"
COLLECTION="partsupp"
SHARD_KEY="ps_partkey"
PORT=26061
JSON_FILE="/home/ubuntu/Data/${DB}___${COLLECTION}.json"

echo "[1] Drop & cleanup configs..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.$COLLECTION.drop()
use config
db.collections.deleteOne({ _id: "$DB.$COLLECTION" })
db.chunks.deleteMany({ ns: "$DB.$COLLECTION" })
EOF

echo "[2] Create collection and hashed index..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.createCollection("$COLLECTION")
db.$COLLECTION.createIndex({ $SHARD_KEY: "hashed" })
EOF

echo "[3] Enable sharding and shard the collection..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
sh.enableSharding("$DB")
sh.shardCollection("$DB.$COLLECTION", { $SHARD_KEY: "hashed" })
EOF

echo "[4] Import actual data..."
mongoimport --host 172.16.31.200 --port $PORT -d $DB -c $COLLECTION --file "$JSON_FILE"

echo "[5] Final shard distribution:"
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.$COLLECTION.getShardDistribution()
EOF
