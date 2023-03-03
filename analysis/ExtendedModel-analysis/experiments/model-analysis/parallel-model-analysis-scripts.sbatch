#!/bin/bash
#
#SBATCH --job-name=ll-m-analysis
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=100GB
#SBATCH --time=2:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.1.0
module load pandoc/2.11.3.2

R --no-save -q -f model-variation-analysis.R

