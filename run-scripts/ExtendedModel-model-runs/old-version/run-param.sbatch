#!/bin/sh

#SBATCH --job-name=params
#SBATCH --nodes=1 
#SBATCH --cpus-per-task=1 
#SBATCH --mem=2GB 
#SBATCH --time=24:00:00 
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge 
module load jdk/11.0.9 

cd ..
cd ..
#cd src
#javac -d ../classes/ recycling/*.java
#cd ..

##run experiment 50 times
while IFS=, read -r of name size startYear timestep maxUI maxAC maxFS maxNS bProb sProb overlap mu sizePref flakePref minFS minNS strict ED GF totalSteps;
do
  of=$(echo $of | tr -d \")
  name=$(echo $name | tr -d \")
  size=$(echo $size | tr -d \")
  startYear=$(echo $startYear | tr -d \")
  timestep=$(echo $timestep | tr -d \")
  maxUI=$(echo $maxUI | tr -d \")
  maxAC=$(echo $maxAC | tr -d \")
  maxFS=$(echo $maxFS | tr -d \")
  maxNS=$(echo $maxNS | tr -d \")
  bProb=$(echo $bProb | tr -d \")
  sProb=$(echo $sProb | tr -d \")
  overlap=$(echo $overlap | tr -d \")
  mu=$(echo $mu | tr -d \")
  sPref=$(echo $sizePref | tr -d \")
  fPref=$(echo $flakePref | tr -d \")
  minFS=$(echo $minFS | tr -d \")
  minNS=$(echo $minNS | tr -d \")
  strict=$(echo $strict | tr -d \")
  ED=$(echo $ED | tr -d \")
  GF=$(echo $GF | tr -d \")
  totalSteps=$(echo $totalSteps | tr -d \")
  echo "exp$of" "run$name" $size $startYear $timestep $maxUI $maxAC $maxFS $maxNS $bProb $sProb $overlap $mu $sPref $fPref $minFS $minNS $strict $ED $GF $totalSteps

##run experiment 50 times
  for i in {1..50}
  do
    runnum=$(echo "run$i")
    java -jar recycling-model.jar "exp$of" $runnum $size $startYear $timestep $maxUI $maxAC $maxFS $maxNS $bProb $sProb $overlap $mu $sPref $fPref $minFS $minNS $strict $ED $GF $totalSteps
  done

##collect data into one spreadsheet
  cd output
  cd "exp$of"

  #concatenate all model files
  awk '(NR == 1) || (FNR > 1)' *model*.csv > model-data.csv
  #concatenate all layer files
  awk '(NR == 1) || (FNR > 1)' *layers*.csv > layers-data.csv  
  #concatenate all artifact files
  awk '(NR == 1) || (FNR > 1)' *artifacts*.csv > artifacts-data.csv

  rm run*.csv

done <<< $1 

