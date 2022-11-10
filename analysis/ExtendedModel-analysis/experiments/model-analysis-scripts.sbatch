#!/bin/bash
#
#SBATCH --job-name=test-m-analysis
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=1500GB
#SBATCH --time=127:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.1.0
module load pandoc/2.11.3.2

R --no-save -q -f variation-analysis.R
