#!/bin/bash

#SBATCH -p shared
#SBATCH -c 20
#SBATCH --mem=60G
#SBATCH -t 72:00:00

# Load Application
module purge
module load python/3.9.9
unset OMP_PLACES
unset OMP_PROC_BIND

# try to control automatic multithreading
ompthreads=1
export OMP_NUM_THREADS=$ompthreads
echo 'OMP_NUM_THREADS is ' $OMP_NUM_THREADS
export OPENBLAS_NUM_THREADS=$ompthreads
echo 'OPENBLAS_NUM_THREADS is ' $OPENBLAS_NUM_THREADS
export MKL_NUM_THREADS=$ompthreads
echo 'MKL_NUM_THREADS is ' $MKL_NUM_THREADS
export BLIS_NUM_THREADS=$ompthreads
echo 'BLIS_NUM_THREADS is ' $BLIS_NUM_THREADS


# Run application
#./lorenz_sindy_increasing_n.py
time nohup python3 lorenz_ensemble_sindy_increasing_n.py > lorenz_${LIBRARY_ENS}_${METHOD}_ensemble_sindy_increasing_n_${START}_${END}_num_init_${NUM_INIT_CONDITIONS}_${SLURM_JOB_ID}.log 2>&1