# dplyr from tidyverse is used for Centering and Standardizing
library(tidyverse)
library(deSolve) # ODE
library(signal)
# source("finite_difference.R")
### Last updated 18.02.2021
### Linear 3D System
linear3d_system <- function(n, init_conditions, dt, snr = Inf)  {
  n <- round(n, 0)
  # eta <- eta
  dt <- dt
  # n = number of time points rounded to nearest integer noise = noise to
  # be added monomial_degree = degree of monomial
  times <- seq(0, ((n) - 1) * dt, by = dt)
  # Matrix values taken from pysindy github
  # https://github.com/dynamicslab/pysindy/blob/master/examples/3_original_paper.ipynb
  matrix_a <- matrix(c(-0.1, -2, 0,
                       2, -0.1, 0,
                       0, 0, -0.3), 3, 3)
  # init_conditions <- c(X = 2, Y = 0, Z = 1) ### Original Initial Conditions
  init_conditions <- init_conditions
  linear3d <- function(t, init_conditions, parameters) {
    with(as.list(c(init_conditions, parameters)), {
      dx <- matrix_a[1, 1] * init_conditions[1] + matrix_a[1, 2] * init_conditions[2]
      dy <- matrix_a[2, 1] * init_conditions[1] + matrix_a[2, 2] * init_conditions[2]
      dz <- matrix_a[3, 3] * init_conditions[3]
      list(c(dx, dy, dz))
    })
  }
  out <- ode(y = init_conditions, times = times,
             func = linear3d, parms = matrix_a,
             atol = 1.49012e-8, rtol = 1.49012e-8)[, -1]
  if (!is.infinite(snr)) { # change to allow for noise
    # Add Noise if noise = "init_conditions"
    length <- nrow(out) * ncol(out)
    # out <- out + eta * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
    snr_volt <- 10 ^ -(snr / 20)
    # e_sym <- (sum(abs(out) ^ 2)) / length # symbolic energy (power / length of signal)
    # N0 <- e_sym / snr_lin
    # noise_sigma <- sqrt(N0)
    # noise_matrix <- noise_sigma * matrix(rnorm(length, mean = 0, sd = 1), nrow(out))
    noise_matrix <- snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
    out <- out + noise_matrix
  }
  # Return data frames, column names, and polynomial degrees
  return(x_t = out)
}


# linear3d_measured <- linear3d_system(10 ^ 2, 0, 5, "measured")
# linear3d_centered <- linear3d_system(10 ^ 2, 0, 5, "findiff", "centered")

