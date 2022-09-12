#!/bin/bash
#
#SBATCH --job-name=ts-creation
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=2000GB
#SBATCH --time=167:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.1.0

R --no-save -q -f time-series-dissimilarity.R
#R --no-save -q -f test-time-series.R
