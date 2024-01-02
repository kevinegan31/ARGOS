system(sprintf("chmod 700 lorenz_ensemble_sindy_increasing_n.py lorenz_ensemble_sindy_increasing_snr.py"))


methods <- c("bragging", "bagging")  # Define methods
n_models <- c(100, 1000)  # Define number of models
noise_levels <- c(0.1, 0.5, 1.0)  # Define noise levels

by <- 3
start <- 1
end <- 62
n_obs <- 5000
s1 <- seq(start, end, by = by)
for (method in methods) {
  for (n_model in n_models) {
        system(
          sprintf(
            "sbatch --export=ALL,N_OBS=%i,START=1,END=62,BY=3,SEED=123,LIBRARY_DEGREE=5,NUM_INIT_CONDITIONS=100,LIBRARY_TYPE=poly,SPARSITY_THRESHOLD=0.2,N_MODELS=%i,INCLUSION_PROBA=0.4,METHOD=%s,LIBRARY_ENS=library lorenz_ensemble_sindy_increasing_snr.sh",
            n_obs, n_model, method
          )
        )
    system(
      sprintf(
        "sbatch --export=ALL,N_OBS=%i,START=1,END=62,BY=3,SEED=123,LIBRARY_DEGREE=5,NUM_INIT_CONDITIONS=100,LIBRARY_TYPE=poly,SPARSITY_THRESHOLD=0.2,N_MODELS=%i,INCLUSION_PROBA=0.6,METHOD=%s,LIBRARY_ENS=normal lorenz_ensemble_sindy_increasing_snr.sh",
        n_obs, n_model, method
      )
    )
  }
}
for (method in methods) {
  for (n_model in n_models) {
    print(
      sprintf(
        "sbatch --export=ALL,N_OBS=%i,START=1,END=62,BY=3,SEED=123,LIBRARY_DEGREE=5,NUM_INIT_CONDITIONS=100,LIBRARY_TYPE=poly,SPARSITY_THRESHOLD=0.2,N_MODELS=%i,INCLUSION_PROBA=0.4,METHOD=%s,LIBRARY_ENS=library lorenz_ensemble_sindy_increasing_snr.sh",
        n_obs, n_model, method
      )
    )
    print(
      sprintf(
        "sbatch --export=ALL,N_OBS=%i,START=1,END=62,BY=3,SEED=123,LIBRARY_DEGREE=5,NUM_INIT_CONDITIONS=100,LIBRARY_TYPE=poly,SPARSITY_THRESHOLD=0.2,N_MODELS=%i,INCLUSION_PROBA=0.6,METHOD=%s,LIBRARY_ENS=normal lorenz_ensemble_sindy_increasing_snr.sh",
        n_obs, n_model, method
      )
    )
  }
}


by <- 0.1
start <- 2
end <- 5 - by
s1 <- seq(start, end, by = by)
eta <- 49
start <- 2
end <- 5
eta <- 49
methods <- c("bragging", "bagging")  # Define an array containing the methods you want to use
n_models <- c(100, 1000)  # Define an array containing the number of models you want to use
for (method in methods) {  # Loop through the array of methods
  for (n_model in n_models) {  # Loop through the array of number of models
    system(
      sprintf(
        "sbatch --export=ALL,SNR=49,START=2,END=5,SEED=123,LIBRARY_DEGREE=5,NUM_INIT_CONDITIONS=100,LIBRARY_TYPE=poly,SPARSITY_THRESHOLD=0.2,N_MODELS=%i,INCLUSION_PROBA=0.4,METHOD=%s,LIBRARY_ENS=library lorenz_ensemble_sindy_increasing_n.sh",
        n_model, method
      )
    )
    system(
      sprintf(
        "sbatch --export=ALL,SNR=49,START=2,END=5,SEED=123,LIBRARY_DEGREE=5,NUM_INIT_CONDITIONS=100,LIBRARY_TYPE=poly,SPARSITY_THRESHOLD=0.2,N_MODELS=%i,INCLUSION_PROBA=0.6,METHOD=%s,LIBRARY_ENS=normal lorenz_ensemble_sindy_increasing_n.sh",
        n_model, method
      )
    )
  }
}
