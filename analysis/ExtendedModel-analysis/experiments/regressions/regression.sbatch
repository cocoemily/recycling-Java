#!/bin/bash
#
#SBATCH --job-name=regs
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=2750GB
#SBATCH --time=47:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.1.0

#R --no-save -q -f total-RI-beta-reg.R
#R --no-save -q -f total-CR-beta-reg.R
#R --no-save -q -f num-deposits-poisson-reg.R
R --no-save -q -f manu-events-poisson-reg.R
#R --no-save -q -f total-retouches-reg.R
#R --no-save -q -f total-encounters-linear-reg.R
#R --no-save -q -f total-discards-reg.R
#R --no-save -q -f scav-events-reg.R
#R --no-save -q -f total-recycled-reg.R
