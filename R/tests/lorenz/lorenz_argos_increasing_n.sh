#!/bin/bash

#SBATCH -p shared
#SBATCH -c 1
#SBATCH -n 32
##SBATCH --mem-per-cpu=6G
#SBATCH -t 72:00:00

# Load Application
module purge
module load r/4.1.2
module load python/3.9.9

# try to control automatic multithreading
ompthreads=1
export OMP_NUM_THREADS=$ompthreads
echo 'OMP_NUM_THREADS is ' $OMP_NUM_THREADS

# Run application
R CMD BATCH lorenz_argos_increasing_n.R lorenz_argos_${LASSO_METHOD}_${ALASSO_WEIGHTS}_snr_${SNR}_increasing_n_${START}_${END}_${SLURM_JOB_ID}.Rout
