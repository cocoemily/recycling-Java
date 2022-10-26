#!/bin/bash
#
#SBATCH --job-name=l-data-test
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=100GB
#SBATCH --time=47:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.2.0
module load pandoc/2.11.3.2

R --no-save -q -f layer-data-req-test.R
