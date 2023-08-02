#!/bin/bash

module purge 
module load jdk/11.0.9 

#cd ..
#cd ..
#cd src
#javac -d ../classes/ recycling/*.java
#cd ..


while IFS=, read -r line; do
  #echo $line
  if [[ $line != \"of* ]]
  then
    echo $line
    sbatch /scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/occupation/run-param.sbatch $line
  fi

  #while IFS=, read -r of name size startYear timestep maxUI maxAC maxFS maxNS bProb sProb overlap mu sizePref flakePref minFS minNS strict ED GF totalSteps
  #do
  #  echo "exp$of" "run$name" $size $startYear $timestep $maxUI $maxAC $maxFS $maxNS $bProb $sProb $overlap $mu $sPref $fPref $minFS $minNS $strict $ED $GF $totalSteps
  #done
done < /scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/occupation/parameters4.csv
