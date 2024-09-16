#!/bin/bash
#SBATCH --job-name=modtest-ecto
#SBATCH --account=pi-cbrook
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --time=24:00:00

module load flex/2.6.4
module load vim/8.1  
module load openmpi/3.1.2
module load cmake/3.15 
module load python/cpython-3.7.0
module load gcc/10.2.0
module load emacs/26
module load java/1.8


~/project2/cbrook/modeltest-ng-static -i ~/scratch-midway2/modeltestecto/AllEctoAligned_Trimmed.fasta -t ml -p 1