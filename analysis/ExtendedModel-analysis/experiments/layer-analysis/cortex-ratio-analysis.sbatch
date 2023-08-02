#!/bin/bash
#
#SBATCH --job-name=l-analysis
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=100GB
#SBATCH --time=6:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.2.0

R --no-save -q -f cortex-ratio-CI.R
