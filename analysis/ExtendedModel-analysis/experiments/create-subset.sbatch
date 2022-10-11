#!/bin/bash
#
#SBATCH --job-name=render
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=2500GB
#SBATCH --time=47:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.2.0

R --no-save -q -f create-subset-for-script-dev.R
