#!/bin/bash -e

cd problems
zip -r -X solutions.zip *.sol
curl -F "private_id=$ICFPC_KEY" -F "file=@solutions.zip" https://monadic-lab.org/submit
cd ..