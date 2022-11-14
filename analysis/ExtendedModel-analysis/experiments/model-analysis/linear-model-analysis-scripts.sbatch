#!/bin/bash
#
#SBATCH --job-name=m-analysis
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=500GB
#SBATCH --time=48:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.1.0
module load pandoc/2.11.3.2

R --no-save -q -f model-mobility-analysis.R
#R --no-save -q -f model-selection-analysis.R
