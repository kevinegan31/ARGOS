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
# Provide filepath for each item
source("~/alasso.R")
source("~/lasso.R")
source("~/sg_optimal_combination.R")
source("~/argos_2d.R")
source_python("~/vdp_ode.py")
###############################################################################
################################ Generate Data ################################
###############################################################################
start <- as.numeric(Sys.getenv("START")) # 2 -- n0 = 10^2
end <- as.numeric(Sys.getenv("END")) # 5 -- n_final = 10^5
num_init <- as.numeric(Sys.getenv("NUM_INIT")) # 100 - number of initial conditions
k <- round(rep(seq(start,
                   end,
                   by = 0.1),
               each = num_init), 1) # create 100 instances for each value of n
s <- 10 ^ k # power transformation of k
num_state_var <- 2 # 2 state variables
init_sequence <- list()
for (i in 1:num_init) {
  init_sequence[[i]] <- runif(num_state_var, min=-4, max=4) # x1,x2 random initial conditions
}
init_sequence <- rep(init_sequence, times = (length(s) / num_init), each = 1)
monomial_degree <- 5 # transform theta
dt <- 0.01 # time-step
snr <- as.numeric(Sys.getenv("SNR")) # 49 -- signal-to-noise ratio (SNR)
mu <- 1.2
systems <- list()
for (i in seq_along(init_sequence)) {
  systems[[i]] <-
    vdp_ode(
      n = as.numeric(s[i]),
      dt = dt,
      init_conditions = init_sequence[[i]],
      mu = mu,
      snr = snr
    )
}
# 80/20 split
x_t <- systems
x_t_train <- list()
for (i in seq_along(x_t)) {
  x_t_train[[i]] <- x_t[[i]][1:round((nrow(x_t[[i]])*0.8), 0), ]
}
###############################################################################
############################### SG filter Parameters ##########################
###############################################################################
xt_1 <- list()
for (i in seq_along(x_t_train)) {
  xt_1[[i]] <- as.matrix(x_t_train[[i]][, 1])
}
xt_2 <- list()
for (i in seq_along(x_t_train)) {
  xt_2[[i]] <- as.matrix(x_t_train[[i]][, 2])
}
vdp_sg_combinations_x1 <- lapply(X = xt_1,
                                    FUN = sg_optimal_combination,
                                    dt = dt)
vdp_porder_wl_combos_x1 <- sapply(vdp_sg_combinations_x1,
                                     function(x)
                                       x[2])
vdp_sg_combinations_x2 <- lapply(X = xt_2,
                                    FUN = sg_optimal_combination,
                                    dt = dt)
vdp_porder_wl_combos_x2 <- sapply(vdp_sg_combinations_x2,
                                     function(x)
                                       x[2])
###############################################################################
############################### Generate Combinations #########################
################################# Define methods ##############################
###############################################################################
### Methods for Bootstrap
### Need to order the methods correctly for bootstrap call
lasso_method <- Sys.getenv("LASSO_METHOD") # lasso or alasso
cl_cpu <- Sys.getenv("CL_CPU") # 32 for High-Power Computing
if (lasso_method == "lasso") {
  bs_method <- list(statistic = match.fun(lasso_method),
                    parallel = "multicore",
                    ncpus = as.numeric(cl_cpu),
                    ols_ps = TRUE)
} else {
  alasso_weights_method <- Sys.getenv("ALASSO_WEIGHTS")
  bs_method <- list(statistic = match.fun(lasso_method),
                    parallel = "multicore",
                    ncpus = as.numeric(cl_cpu),
                    weights_method = alasso_weights_method,
                    ols_ps = TRUE)
}
alpha <- 0.05 # 95% confidence intervals
num_samples <- as.numeric(Sys.getenv("NUM_SAMPLES")) # 2000 BS samples
###############################################################################
################################ Run Functions ################################
###############################################################################
### xdot
xdot_argos <- list()
for (i in seq_along(x_t_train)) {
  xdot_argos[[i]] <- boot_lasso(x_t = x_t_train[[i]],
                                monomial_degree = monomial_degree,
                                dt = dt,
                                alpha = alpha,
                                num_samples = num_samples,
                                sg_combinations_x1 = vdp_porder_wl_combos_x1[[i]],
                                sg_combinations_x2 = vdp_porder_wl_combos_x2[[i]],
                                sr_method = lasso_method,
                                bs_method = bs_method,
                                derivative = "xdot")
}
xdot_argos_pe <- purrr::map(xdot_argos, `[[`, 1)
xdot_argos_ci_normal <- purrr::map(xdot_argos, `[[`, 2)
### We change the length of each element
### to the max length from the 'indx' (length<-)
### and thereby pad NA values at the end of the list elements
### with length less than the max length.
xdot_pe_df <- data.frame(lapply(xdot_argos_pe, "length<-",
                                max(lengths(xdot_argos_pe))))
colnames(xdot_pe_df) <- paste0("pe_", s)
### Normal CIs
xdot_ci_normal_max_length <-
  max(unlist(lapply(xdot_argos_ci_normal, length)))
xdot_ci_normal_df <- sapply(xdot_argos_ci_normal, function(x) {
  length(x) <- xdot_ci_normal_max_length
  return(x)
})
colnames(xdot_ci_normal_df) <- paste0("ci_", s)
### Write to CSV
if (lasso_method == "lasso") {
  write.csv(
    xdot_pe_df,
    sprintf(
      "~/xdot_vdp_increasing_n_%s_N%s_%s_B%s_snr_%s_SEED_%s_pe_df.csv",
      lasso_method,
      start,
      end,
      num_samples,
      snr,
      seed
    )
  )
  write.csv(
    xdot_ci_normal_df,
    sprintf(
      "~/xdot_vdp_increasing_n_%s_N%s_%s_B%s_snr_%s_SEED_%s_ci_normal_df.csv",
      lasso_method,
      start,
      end,
      num_samples,
      snr,
      seed
    )
  )
} else {
  write.csv(
    xdot_pe_df,
    sprintf(
      "~/xdot_vdp_increasing_n_%s_%s_N%s_%s_B%s_snr_%s_SEED_%s_pe_df.csv",
      lasso_method,
      alasso_weights_method,
      start,
      end,
      num_samples,
      snr,
      seed
    )
  )
  write.csv(
    xdot_ci_normal_df,
    sprintf(
      "~/xdot_vdp_increasing_n_%s_%s_N%s_%s_B%s_snr_%s_SEED_%s_ci_normal_df.csv",
      lasso_method,
      alasso_weights_method,
      start,
      end,
      num_samples,
      snr,
      seed
    )
  )
}
### ydot
ydot_argos <- list()
for (i in seq_along(x_t_train)) {
  ydot_argos[[i]] <- boot_lasso(x_t = x_t_train[[i]],
                                monomial_degree = monomial_degree,
                                dt = dt,
                                alpha = alpha,
                                num_samples = num_samples,
                                sg_combinations_x1 = vdp_porder_wl_combos_x1[[i]],
                                sg_combinations_x2 = vdp_porder_wl_combos_x2[[i]],
                                sr_method = lasso_method,
                                bs_method = bs_method,
                                derivative = "ydot")
}
ydot_argos_pe <- purrr::map(ydot_argos, `[[`, 1)
ydot_argos_ci_normal <- purrr::map(ydot_argos, `[[`, 2)
### We change the length of each element
### to the max length from the 'indx' (length<-)
### and thereby pad NA values at the end of the list elements
### with length less than the max length.
ydot_pe_df <- data.frame(lapply(ydot_argos_pe, "length<-",
                                max(lengths(ydot_argos_pe))))
colnames(ydot_pe_df) <- paste0("pe_", s)
### Normal CIs
ydot_ci_normal_max_length <-
  max(unlist(lapply(ydot_argos_ci_normal, length)))
ydot_ci_normal_df <- sapply(ydot_argos_ci_normal, function(x) {
  length(x) <- ydot_ci_normal_max_length
  return(x)
})
colnames(ydot_ci_normal_df) <- paste0("ci_", s)
### Write to CSV
if (lasso_method == "lasso") {
  write.csv(
    ydot_pe_df,
    sprintf(
      "~/ydot_vdp_increasing_n_%s_N%s_%s_B%s_snr_%s_SEED_%s_pe_df.csv",
      lasso_method,
      start,
      end,
      num_samples,
      snr,
      seed
    )
  )
  write.csv(
    ydot_ci_normal_df,
    sprintf(
      "~/ydot_vdp_increasing_n_%s_N%s_%s_B%s_snr_%s_SEED_%s_ci_normal_df.csv",
      lasso_method,
      start,
      end,
      num_samples,
      snr,
      seed
    )
  )
} else {
  write.csv(
    ydot_pe_df,
    sprintf(
      "~/ydot_vdp_increasing_n_%s_%s_N%s_%s_B%s_snr_%s_SEED_%s_pe_df.csv",
      lasso_method,
      alasso_weights_method,
      start,
      end,
      num_samples,
      snr,
      seed
    )
  )
  write.csv(
    ydot_ci_normal_df,
    sprintf(
      "~/ydot_vdp_increasing_n_%s_%s_N%s_%s_B%s_snr_%s_SEED_%s_ci_normal_df.csv",
      lasso_method,
      alasso_weights_method,
      start,
      end,
      num_samples,
      snr,
      seed
    )
  )
}
