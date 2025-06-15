#!/bin/bash

DB="tpch0_01__schema_431"
COLLECTION_ORIGINAL="partsupp_part"
COLLECTION_FLAT="partsupp_part_flat"
SHARD_KEY="ps_partkey"
PORT=26061
INPUT_JSON="/home/ubuntu/Data/${DB}___${COLLECTION_ORIGINAL}.json"
FLAT_JSON="/home/ubuntu/Data/${DB}___${COLLECTION_FLAT}.json"

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
            fout.write(json.dumps(flat_doc) + "\\n")
EOF

echo "[2] Drop collection and clean configs..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.drop()
use config
db.collections.deleteOne({ _id: "$DB.$COLLECTION_FLAT" })
db.chunks.deleteMany({ ns: "$DB.$COLLECTION_FLAT" })
EOF

echo "[3] Enable sharding and prepare collection..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
sh.enableSharding("$DB")
use $DB
db.createCollection("$COLLECTION_FLAT")
db.$COLLECTION_FLAT.createIndex({ $SHARD_KEY: "hashed" })
sh.shardCollection("$DB.$COLLECTION_FLAT", { $SHARD_KEY: "hashed" })
EOF

echo "[4] Import flattened data..."
mongoimport --host 172.16.31.200 --port $PORT -d $DB -c $COLLECTION_FLAT --file "$FLAT_JSON"

echo "[5] Manual split into 6 chunks..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("-6917529027641081856") })
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("-4611686018427387904") })
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("-2305843009213693952") })
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("2305843009213693952") })
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("4611686018427387904") })
EOF

echo "[6] Move chunks to all shards..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("-9223372036854775808") }, "shard1")
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("-6917529027641081856") }, "shard2")
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("-4611686018427387904") }, "shard3")
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("-2305843009213693952") }, "shard4")
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("2305843009213693952") }, "shard5")
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberLong("4611686018427387904") }, "shard6")
EOF

echo "[7] Final distribution check..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.getShardDistribution()
EOF
