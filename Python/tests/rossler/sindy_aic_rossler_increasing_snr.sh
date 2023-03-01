#!/bin/bash

#SBATCH -p shared
#SBATCH -c 1
#SBATCH -n 32
#SBATCH -t 72:00:00

# Load Application
module purge
module load python/3.9.9

# try to control automatic multithreading
ompthreads=1
export OMP_NUM_THREADS=$ompthreads
echo 'OMP_NUM_THREADS is ' $OMP_NUM_THREADS


# Run application
nohup python3 sindy_aic_rossler_increasing_snr.py > sindy_aic_rossler_increasing_snr_${START}_${END}_${SLURM_JOB_ID}.log 2>&1