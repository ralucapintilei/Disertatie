#!/bin/bash

DB="tpch0_01__schema_431"
COLLECTION_ORIGINAL="nation_region"
COLLECTION_FLAT="nation_region_flat"
SHARD_KEY="n_nationkey"
PORT=26061
INPUT_JSON="/home/ubuntu/Data/${DB}___${COLLECTION_ORIGINAL}.json"
FLAT_JSON="/home/ubuntu/Data/${DB}___${COLLECTION_FLAT}.json"

echo "[1] Flatten JSON into individual nation documents..."
python3 <<EOF
import json

infile = "$INPUT_JSON"
outfile = "$FLAT_JSON"

with open(infile, "r") as f_in, open(outfile, "w") as f_out:
    for line in f_in:
        doc = json.loads(line)
        region_fields = {k: v for k, v in doc.items() if k != "nation"}
        for nation in doc.get("nation", []):
            flat_doc = dict(region_fields)
            flat_doc.update(nation)
            f_out.write(json.dumps(flat_doc) + "\n")
EOF

echo "[2] Drop DB and cleanup config metadata..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.dropDatabase()
use config
db.collections.deleteOne({ _id: "$DB.$COLLECTION_FLAT" })
db.chunks.deleteMany({ ns: "$DB.$COLLECTION_FLAT" })
EOF

echo "[3] Enable sharding and create collection/index..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
sh.enableSharding("$DB")
use $DB
db.createCollection("$COLLECTION_FLAT")
db.$COLLECTION_FLAT.createIndex({ $SHARD_KEY: "hashed" })
sh.shardCollection("$DB.$COLLECTION_FLAT", { $SHARD_KEY: "hashed" })
EOF

echo "[4] Import flattened data..."
mongoimport --host 172.16.31.200 --port $PORT -d $DB -c $COLLECTION_FLAT --file "$FLAT_JSON"

echo "[5] Manual chunk splits (optional for better control)..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(2) })
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(5) })
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(8) })
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(11) })
sh.splitAt("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(14) })
EOF

echo "[6] Move chunks to different shards..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(0) }, "shard1")
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(2) }, "shard2")
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(5) }, "shard3")
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(8) }, "shard4")
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(11) }, "shard5")
sh.moveChunk("$DB.$COLLECTION_FLAT", { $SHARD_KEY: NumberInt(14) }, "shard6")
EOF

echo "[7] Final distribution check..."
mongosh --quiet --host 172.16.31.200 --port $PORT <<EOF
use $DB
db.$COLLECTION_FLAT.getShardDistribution()
EOF
