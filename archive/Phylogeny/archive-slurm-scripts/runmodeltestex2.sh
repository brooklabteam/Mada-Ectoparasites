#!/bin/bash
#SBATCH --job-name=trim-modtest-ecto
#SBATCH --account=pi-cbrook
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --time=24:00:00

module load python
source /home/cbrook/anaconda3/etc/profile.d/conda.sh
conda activate /project2/cbrook/software/modeltest_env

modeltest-ng -i /home/cbrook/scratch-midway2/modeltestecto/AllEctoAligned_Trimmed.fasta -t ml -p 1