library(magrittr) ## used for extract function
### Increasing n ----
by <- 1
start <- 2
end <- 5 - by
s1 <- seq(start, end, by = by)
cl_ncpus <- 32
cl_mem <- 90
by_file <- 0.1
num_samples <- 2000
num_init <- 100
snr <- 49
seed_new_num <- 100
for (i in seq_along(s1)) {
  system(sprintf("sbatch --mem=%iG --export=ALL,SEED=%i,START=%.2f,END=%.2f,BY=%.2f,SNR=%i,NUM_INIT=%i,LASSO_METHOD=alasso,ALASSO_WEIGHTS=ridge,CL_CPU=%i,NUM_SAMPLES=%i lorenz_argos_increasing_n.sh",
                 cl_mem, seed_new_num, s1[i], s1[i] + by, by_file, snr, num_init, cl_ncpus, num_samples))
  system(sprintf("sbatch --mem=%iG --export=ALL,SEED=%i,START=%.2f,END=%.2f,BY=%.2f,SNR=%i,NUM_INIT=%i,LASSO_METHOD=lasso,ALASSO_WEIGHTS=NULL,CL_CPU=%i,NUM_SAMPLES=%i lorenz_argos_increasing_n.sh",
                 cl_mem, seed_new_num, s1[i], s1[i] + by, by_file, snr, num_init, cl_ncpus, num_samples))
}

### Increasing SNR ----
by <- 61 - 1
start <- 1
end <- 61 - by
s1 <- seq(start, end, by = by)
cl_ncpus <- 32
by_file <- 3
n_obs <- 7000
num_samples <- 2000
num_init <- 100
cl_mem <- 60
seed_new_num <- 100
for (i in seq_along(s1)) {
  system(sprintf("sbatch --mem=%iG --export=ALL,SEED=%i,START=%.2f,END=%.2f,BY=%.4f,NUM_INIT=100,N_OBS=%i,LASSO_METHOD=alasso,ALASSO_WEIGHTS=ridge,CL_CPU=%i,NUM_SAMPLES=%i lorenz_argos_increasing_snr.sh",
                 cl_mem, seed_new_num, s1[i], s1[i] + by, by_file, n_obs, cl_ncpus, num_samples))
  system(sprintf("sbatch --mem=%iG --export=ALL,SEED=%i,START=%.2f,END=%.2f,BY=%.4f,NUM_INIT=100,N_OBS=%i,LASSO_METHOD=lasso,ALASSO_WEIGHTS=NULL,CL_CPU=%i,NUM_SAMPLES=%i lorenz_argos_increasing_snr.sh",
                 cl_mem, seed_new_num, s1[i], s1[i] + by, by_file, n_obs, cl_ncpus, num_samples))
}
