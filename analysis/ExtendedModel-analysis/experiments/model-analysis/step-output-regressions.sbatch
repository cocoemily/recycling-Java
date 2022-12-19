#!/bin/bash
#
#SBATCH --job-name=m-analysis
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=2000GB
#SBATCH --time=48:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.2.0


R --no-save -q -f scavenging-events-analysis.R

