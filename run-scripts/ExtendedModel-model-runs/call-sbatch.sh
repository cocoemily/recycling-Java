#!/bin/bash

echo >> /scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv
while IFS=, read -r line; do
  #echo $line
  if [[ $line != \"of* ]]
  then
    echo $line
    sbatch /scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/run-param.sbatch $line
  fi

  #while IFS=, read -r of name size startYear timestep maxUI maxAC maxFS maxNS bProb sProb overlap mu sizePref flakePref minFS minNS strict ED GF totalSteps
  #do
  #  echo "exp$of" "run$name" $size $startYear $timestep $maxUI $maxAC $maxFS $maxNS $bProb $sProb $overlap $mu $sPref $fPref $minFS $minNS $strict $ED $GF $totalSteps
  #done
done < test.csv
