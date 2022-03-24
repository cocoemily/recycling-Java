#!/bin/bash
#
#SBATCH --job-name=regs
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=1000GB
#SBATCH --time=72:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.1.0

R --no-save -q -f total-RI-beta-reg.R
