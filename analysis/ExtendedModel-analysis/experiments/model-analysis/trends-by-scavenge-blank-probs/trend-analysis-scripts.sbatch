#!/bin/bash
#
#SBATCH --job-name=m-analysis
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=400GB
#SBATCH --time=4:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.2.0

#R --no-save -q -f overlap-trends.R
#R --no-save -q -f mu-trends.R
#R --no-save -q -f flake-pref-trends.R
R --no-save -q -f size-pref-trends.R
#R --no-save -q -f strict-trends.R

