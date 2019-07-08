#!/bin/bash -xe

export JAVA_HOME=~/work/graalvm-ee-19.1.0/Contents/Home
export GRAAL_HOME=~/work/graalvm-ee-19.1.0/Contents/Home
export PATH=$JAVA_HOME/bin:$PATH

lein trampoline run