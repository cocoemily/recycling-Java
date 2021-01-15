#!/bin/sh

cd src
javac -d ../classes/ recycling/*.java
cd ..
java -cp classes/ recycling.RunOriginalModel "test" 6 250000 100 30 10 10 0.5 2 0

