#!/bin/bash
#SBATCH --job-name=modtest-ecto
#SBATCH --account=pi-cbrook
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --time=24:00:00

module load python
conda init bash
conda activate /project2/cbrook/software/modeltest_env

modeltest-ng -i /home/andrianiaina/Phylogen2024/Ecto_Background_Sequences.fasta -t ml -p 1