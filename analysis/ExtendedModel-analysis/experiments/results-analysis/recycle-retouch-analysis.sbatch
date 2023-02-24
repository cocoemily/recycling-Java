#!/bin/bash
#
#SBATCH --job-name=rr-over
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=500GB
#SBATCH --time=72:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.2.0

#R --no-save -q -f recycled-object-count-results.R
R --no-save -q -f retouch-encounter-overlap.R
