#!/bin/bash

#SBATCH -p shared
#SBATCH -c 1
#SBATCH -n 40
#SBATCH --mem-per-cpu=6G
#SBATCH -t 72:00:00
#SBATCH -e kegan.%A.err

# Load Application
module purge
module load python/3.9.9

# try to control automatic multithreading
ompthreads=1
export OMP_NUM_THREADS=$ompthreads
echo 'OMP_NUM_THREADS is ' $OMP_NUM_THREADS

# Run application
nohup python3 sindy_aic_lorenz_increasing_n.py > sindy_aic_lorenz_increasing_n_${START}_${END}_${SNR}_${SLURM_JOB_ID}.log 2>&1