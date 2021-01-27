#!/bin/sh

cd ..
cd src
javac -d ../classes/ recycling/*.java
cd ..

java -cp classes/ recycling.RunOriginalModel "run1" "exp1" 6 250000 100 30 10 10 0.5 2 0
java -cp classes/ recycling.RunOriginalModel "run1" "exp2" 6 250000 100 30 10 10 0.5 2 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp3" 6 250000 100 30 10 10 0.5 2 1
java -cp classes/ recycling.RunOriginalModel "run1" "exp4" 6 250000 100 30 10 18 0.5 2 0
java -cp classes/ recycling.RunOriginalModel "run1" "exp5" 6 250000 100 30 10 18 0.5 2 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp6" 6 250000 100 30 10 18 0.5 2 1
java -cp classes/ recycling.RunOriginalModel "run1" "exp7" 6 250000 100 30 10 36 0.5 2 0  
java -cp classes/ recycling.RunOriginalModel "run1" "exp8" 6 250000 100 30 10 36 0.5 2 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp9" 6 250000 100 30 10 36 0.5 2 1
java -cp classes/ recycling.RunOriginalModel "run1" "exp10" 6 250000 100 30 10 10 0.5 10 0
java -cp classes/ recycling.RunOriginalModel "run1" "exp11" 6 250000 100 30 10 10 0.5 10 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp12" 6 250000 100 30 10 10 0.5 10 1 
java -cp classes/ recycling.RunOriginalModel "run1" "exp13" 6 250000 100 30 10 18 0.5 10 0  
java -cp classes/ recycling.RunOriginalModel "run1" "exp14" 6 250000 100 30 10 18 0.5 10 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp15" 6 250000 100 30 10 18 0.5 10 1
java -cp classes/ recycling.RunOriginalModel "run1" "exp16" 6 250000 100 30 10 36 0.5 10 0
java -cp classes/ recycling.RunOriginalModel "run1" "exp17" 6 250000 100 30 10 36 0.5 10 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp18" 6 250000 100 30 10 36 0.5 10 1
java -cp classes/ recycling.RunOriginalModel "run1" "exp19" 6 250000 100 30 10 10 0.5 20 0
java -cp classes/ recycling.RunOriginalModel "run1" "exp20" 6 250000 100 30 10 10 0.5 20 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp21" 6 250000 100 30 10 10 0.5 20 1 
java -cp classes/ recycling.RunOriginalModel "run1" "exp22" 6 250000 100 30 10 18 0.5 20 0  
java -cp classes/ recycling.RunOriginalModel "run1" "exp23" 6 250000 100 30 10 18 0.5 20 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp24" 6 250000 100 30 10 18 0.5 20 1
java -cp classes/ recycling.RunOriginalModel "run1" "exp25" 6 250000 100 30 10 36 0.5 20 0
java -cp classes/ recycling.RunOriginalModel "run1" "exp26" 6 250000 100 30 10 36 0.5 20 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp27" 6 250000 100 30 10 36 0.5 20 1
java -cp classes/ recycling.RunOriginalModel "run1" "exp28" 6 250000 100 30 10 18 0.25 10 0
java -cp classes/ recycling.RunOriginalModel "run1" "exp29" 6 250000 100 30 10 18 0.25 10 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp30" 6 250000 100 30 10 18 0.25 10 1
java -cp classes/ recycling.RunOriginalModel "run1" "exp28" 6 250000 100 30 10 18 0.75 10 0
java -cp classes/ recycling.RunOriginalModel "run1" "exp29" 6 250000 100 30 10 18 0.75 10 0.5
java -cp classes/ recycling.RunOriginalModel "run1" "exp30" 6 250000 100 30 10 18 0.75 10 1
