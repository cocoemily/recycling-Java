#!/bin/bash
#
#SBATCH --job-name=bregs
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=500GB
#SBATCH --time=120:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.1.0
module load pandoc/2.11.3.2

#R --no-save -q -f test-beta-reg.R
R --no-save -q -f full-beta-models.R
