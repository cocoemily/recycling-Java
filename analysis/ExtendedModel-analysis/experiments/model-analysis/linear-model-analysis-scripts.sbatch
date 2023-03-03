#!/bin/bash
#
#SBATCH --job-name=m-analysis
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=100GB
#SBATCH --time=4:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.2.0

#R --no-save -q -f model-mobility-analysis.R
#R --no-save -q -f model-overlap-analysis.R
#R --no-save -q -f model-selection-analysis.R

#R --no-save -q -f change-over-time-RI.R
#R --no-save -q -f change-over-time-scavenging.R
#R --no-save -q -f change-over-time-discards.R
#R --no-save -q -f change-over-time-recycling-events.R
#R --no-save -q -f change-over-time-retouches.R
#R --no-save -q -f change-over-time-blanks.R

R --no-save -q -f trends-by-blank-scavenging.R
