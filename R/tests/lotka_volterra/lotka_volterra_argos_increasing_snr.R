rm(list = ls())
###############################################################################
################################# Load Packages ###############################
###############################################################################
### Seed for reproducibility
seed <- as.numeric(Sys.getenv("SEED"))
set.seed(seed)
### For regression
library(glmnet)
### Used for tsboot bootstrap
library(boot)
### Determines blocklength with b.star
library(np)
### Dplyr as well as purrr
library(plyr)
library(tidyverse)
### ODE solver
library(deSolve)
### Smoothing
library(signal)
### Parallel processing
library(doParallel)
### Use Python for ODE solver
library(reticulate)
###############################################################################
################################# Load Functions ##############################
###############################################################################
# Change file path for each function
source("~/alasso.R")
source("~/lasso.R")
source("~/sg_optimal_combination.R")
source("~/argos_3d.R")
source_python("~/lotka_volterra_ode.py")
###############################################################################
################################ Generate Data ################################
###############################################################################
start <- Sys.getenv("START")
end <- Sys.getenv("END")
by <- Sys.getenv("BY")
num_init <- as.numeric(Sys.getenv("NUM_INIT"))
s <- rep(c(seq(
  as.numeric(start),
  as.numeric(end),
  by = as.numeric(by)
), Inf),
each = num_init)
num_state_var <- 2 # state variables
init_sequence <- list()
for (i in 1:num_init) {
  init_sequence[[i]] <- runif(num_state_var, min=1, max=10)
}
init_sequence <-
  rep(init_sequence,
      times = (length(s) / num_init),
      each = 1)
n_obs <- Sys.getenv("N_OBS") # n = 5000
monomial_degree <- 5 # theta matrix
dt <- 0.01 # time-step
mu <- 1.2
systems <- list()
for (i in seq_along(init_sequence)) {
  systems[[i]] <-
    lotka_volterra_ode(
      n = as.numeric(n_obs),
      dt = dt,
      init_conditions = init_sequence[[i]],
      snr = s[[i]]
    )
}
x_t <- systems
# 80/20 split
x_t_train <- list()
for (i in seq_along(x_t)) {
  x_t_train[[i]] <- x_t[[i]][1:round((nrow(x_t[[i]]) * 0.8), 0),]
}
###############################################################################
############################### Generate Combinations #########################
################################# Define methods ##############################
###############################################################################
### SG Filter for each x(t)
xt_1 <- list()
for (i in seq_along(x_t_train)) {
  xt_1[[i]] <- as.matrix(x_t_train[[i]][, 1])
}
xt_2 <- list()
for (i in seq_along(x_t_train)) {
  xt_2[[i]] <- as.matrix(x_t_train[[i]][, 2])
}
lotka_volterra_sg_combinations_x1 <- lapply(X = xt_1,
                                 FUN = sg_optimal_combination,
                                 dt = dt)
lotka_volterra_porder_wl_combos_x1 <- sapply(lotka_volterra_sg_combinations_x1,
                                  function(x)
                                    x[2])
lotka_volterra_sg_combinations_x2 <- lapply(X = xt_2,
                                 FUN = sg_optimal_combination,
                                 dt = dt)
lotka_volterra_porder_wl_combos_x2 <- sapply(lotka_volterra_sg_combinations_x2,
                                  function(x)
                                    x[2])
###############################################################################
### Methods for Bootstrap
### Need to order the methods correctly for bootstrap call
lasso_method <- Sys.getenv("LASSO_METHOD") # lasso or alasso
cl_cpu <- Sys.getenv("CL_CPU") # 32 for High-Power Computing
if (lasso_method == "lasso") {
  bs_method <- list(
    statistic = match.fun(lasso_method),
    parallel = "multicore",
    ncpus = as.numeric(cl_cpu),
    ols_ps = TRUE
  )
} else {
  alasso_weights_method <- Sys.getenv("ALASSO_WEIGHTS")
  bs_method <- list(
    statistic = match.fun(lasso_method),
    parallel = "multicore",
    ncpus = as.numeric(cl_cpu),
    weights_method = alasso_weights_method,
    ols_ps = TRUE
  )
}
alpha <- 0.05 # 95% confidence intervals
num_samples <-
  as.numeric(Sys.getenv("NUM_SAMPLES")) # 2000 bootstrap samples
###############################################################################
################################ Run Functions ################################
###############################################################################
### xdot
xdot_isr <- list()
for (i in seq_along(x_t_train)) {
  xdot_isr[[i]] <- boot_lasso(
    x_t = x_t_train[[i]],
    monomial_degree = monomial_degree,
    dt = dt,
    alpha = alpha,
    num_samples = num_samples,
    sg_combinations_x1 = lotka_volterra_porder_wl_combos_x1[[i]],
    sg_combinations_x2 = lotka_volterra_porder_wl_combos_x2[[i]],
    sr_method = lasso_method,
    bs_method = bs_method,
    derivative = "xdot"
  )
}
xdot_isr_pe <- purrr::map(xdot_isr, `[[`, 1)
xdot_isr_ci_normal <- purrr::map(xdot_isr, `[[`, 2)
### We change the length of each element
### to the max length from the 'indx' (length<-)
### and thereby pad NA values at the end of the list elements
### with length less than the max length.
xdot_pe_df <- data.frame(lapply(xdot_isr_pe, "length<-",
                                max(lengths(xdot_isr_pe))))
colnames(xdot_pe_df) <- paste0("pe_", s)
### Normal CIs
xdot_ci_normal_max_length <-
  max(unlist(lapply(xdot_isr_ci_normal, length)))
xdot_ci_df <- sapply(xdot_isr_ci_normal, function(x) {
  length(x) <- xdot_ci_normal_max_length
  return(x)
})
colnames(xdot_ci_df) <- paste0("ci_", s)
### Write to CSV
if (lasso_method == "lasso") {
  write.csv(
    xdot_pe_df,
    sprintf(
      "~/xdot_lotka_volterra_increasing_snr_%s_N%s_B%s_snr_%s_%s_SEED_%s_pe_df.csv",
      lasso_method,
      n_obs,
      num_samples,
      start,
      end,
      seed
    )
  )
  write.csv(
    xdot_ci_df,
    sprintf(
      "~/xdot_lotka_volterra_increasing_snr_%s_N%s_B%s_snr_%s_%s_SEED_%s_ci_df.csv",
      lasso_method,
      n_obs,
      num_samples,
      start,
      end,
      seed
    )
  )
} else {
  write.csv(
    xdot_pe_df,
    sprintf(
      "~/xdot_lotka_volterra_increasing_snr_%s_%s_N%s_B%s_snr_%s_%s_SEED_%s_pe_df.csv",
      lasso_method,
      alasso_weights_method,
      n_obs,
      num_samples,
      start,
      end,
      seed
    )
  )
  write.csv(
    xdot_ci_df,
    sprintf(
      "~/xdot_lotka_volterra_increasing_snr_%s_%s_N%s_B%s_snr_%s_%s_SEED_%s_ci_df.csv",
      lasso_method,
      alasso_weights_method,
      n_obs,
      num_samples,
      start,
      end,
      seed
    )
  )
}
rm(list = ls(pattern = "xdot"))
### ydot
ydot_isr <- list()
for (i in seq_along(x_t_train)) {
  ydot_isr[[i]] <- boot_lasso(
    x_t = x_t_train[[i]],
    monomial_degree = monomial_degree,
    dt = dt,
    alpha = alpha,
    num_samples = num_samples,
    sg_combinations_x1 = lotka_volterra_porder_wl_combos_x1[[i]],
    sg_combinations_x2 = lotka_volterra_porder_wl_combos_x2[[i]],
    sr_method = lasso_method,
    bs_method = bs_method,
    derivative = "ydot"
  )
}
ydot_isr_pe <- purrr::map(ydot_isr, `[[`, 1)
ydot_isr_ci_normal <- purrr::map(ydot_isr, `[[`, 2)
### We change the length of each element
### to the max length from the 'indx' (length<-)
### and thereby pad NA values at the end of the list elements
### with length less than the max length.
ydot_pe_df <- data.frame(lapply(ydot_isr_pe, "length<-",
                                max(lengths(ydot_isr_pe))))
colnames(ydot_pe_df) <- paste0("pe_", s)
### Normal CIs
ydot_ci_normal_max_length <-
  max(unlist(lapply(ydot_isr_ci_normal, length)))
ydot_ci_df <- sapply(ydot_isr_ci_normal, function(x) {
  length(x) <- ydot_ci_normal_max_length
  return(x)
})
colnames(ydot_ci_df) <- paste0("ci_", s)
### Write to CSV
if (lasso_method == "lasso") {
  write.csv(
    ydot_pe_df,
    sprintf(
      "~/ydot_lotka_volterra_increasing_snr_%s_N%s_B%s_snr_%s_%s_SEED_%s_pe_df.csv",
      lasso_method,
      n_obs,
      num_samples,
      start,
      end,
      seed
    )
  )
  write.csv(
    ydot_ci_df,
    sprintf(
      "~/ydot_lotka_volterra_increasing_snr_%s_N%s_B%s_snr_%s_%s_SEED_%s_ci_df.csv",
      lasso_method,
      n_obs,
      num_samples,
      start,
      end,
      seed
    )
  )
} else {
  write.csv(
    ydot_pe_df,
    sprintf(
      "~/ydot_lotka_volterra_increasing_snr_%s_%s_N%s_B%s_snr_%s_%s_SEED_%s_pe_df.csv",
      lasso_method,
      alasso_weights_method,
      n_obs,
      num_samples,
      start,
      end,
      seed
    )
  )
  write.csv(
    ydot_ci_df,
    sprintf(
      "~/ydot_lotka_volterra_increasing_snr_%s_%s_N%s_B%s_snr_%s_%s_SEED_%s_ci_df.csv",
      lasso_method,
      alasso_weights_method,
      n_obs,
      num_samples,
      start,
      end,
      seed
    )
  )
}
rm(list = ls(pattern = "ydot"))
