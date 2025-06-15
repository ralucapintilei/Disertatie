#!/bin/bash

DB="tpch0_01__schema_431"
COLLECTION_ORIGINAL="lineitem_orders"
COLLECTION_FLAT="lineitem_orders_flat"
SHARD_KEY="l_orderkey"
PORT=26061
INPUT_JSON="/home/ubuntu/Data/${DB}___${COLLECTION_ORIGINAL}.json"
FLAT_JSON="/home/ubuntu/Data/${DB}___${COLLECTION_FLAT}.json"

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
            f_out.write(json.dumps(flat_doc) + "\\n")
EOF

echo "[2] Drop and clean metadata..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.drop()
use config
db.collections.deleteOne({ _id: "$DB.$COLLECTION_FLAT" })
db.chunks.deleteMany({ ns: "$DB.$COLLECTION_FLAT" })
EOF

echo "[3] Enable sharding, create collection and index..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
sh.enableSharding("$DB")
use $DB
db.createCollection("$COLLECTION_FLAT")
db.$COLLECTION_FLAT.createIndex({ $SHARD_KEY: "hashed" })
sh.shardCollection("$DB.$COLLECTION_FLAT", { $SHARD_KEY: "hashed" })
EOF

echo "[4] Insert dummy to trigger chunk creation..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.insertOne({ $SHARD_KEY: NumberLong(-999999999), dummy: true })
EOF

echo "[5] Delete dummy and import real data..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.deleteOne({ dummy: true })
EOF

mongoimport --host 172.16.31.200 --port $PORT -d $DB -c $COLLECTION_FLAT --file "$FLAT_JSON"

echo "[6] View final distribution"
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.getShardDistribution()
EOF
