#!/bin/bash
#SBATCH --job-name=full-raxml
#SBATCH --account=pi-cbrook
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=5
#SBATCH --time=24:00:00

module load flex/2.6.4
module load vim/8.1  
module load openmpi/3.1.2
module load cmake/3.15 
module load python/cpython-3.7.0
module load gcc/10.2.0
module load emacs/26
module load java/1.8

/project2/cbrook/software/raxml-ng/bin/raxml-ng-mpi --all --msa 18Salign.fasta --model TIM2+I+G4 --prefix T3  --seed 12 --threads 5  --bs-metric fbp,tbe --blopt nr_safe