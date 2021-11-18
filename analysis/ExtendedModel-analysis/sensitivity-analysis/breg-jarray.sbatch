#!/bin/bash
#
#SBATCH --job-name=br-array2
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --array=1-13
#SBATCH --mem=500GB
#SBATCH --time=72:00:00
#SBATCH --output=regression_output/loo-regression_%A_%a.out
#SBATCH --mail-type=END
#SBATCH --mail-user=ec3307@nyu.edu

module purge
module load r/gcc/4.1.0
module load pandoc/2.11.3.2

#parameters=("max_use_intensity" "max_artifact_carry" "max_flake_size" 
#"max_nodules_size" "blank_prob" "scavenge_prob" "overlap" "mu" 
#"size_preference" "flake_preference" "min_suitable_flake_size" 
#"min_suitable_nodule_size" "strict_selection")

Rscript --vanilla full-beta-models.R $SLURM_ARRAY_TASK_ID
