#!/bin/sh

cd ..
cd src
javac -d ../classes/ recycling/*.java
cd ..

java -cp classes/ recycling.RunOriginalModel "test" "exp1" 6 250000 100 30 10 10 0.5 2 0
