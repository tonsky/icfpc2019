#!/bin/bash -e

cd client/lambda-client

block=$(./lambda-cli.py getmininginfo block)
excluded=$(./lambda-cli.py getmininginfo excluded)
puzzle=$(./lambda-cli.py getmininginfo puzzle)
task=$(./lambda-cli.py getmininginfo task)

lein run -m icfpc.main/mine-coins $block $excluded $puzzle $task
