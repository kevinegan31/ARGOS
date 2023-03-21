# dplyr from tidyverse is used for Centering and Standardizing
library(tidyverse)
library(deSolve) # ODE
library(signal) # smoothing
# source("finite_difference.R")
### Last updated 18.02.2021
### Removed 1's column
# source("finite_difference.R")
### Lotka Volterra
lotka_volterra <- function(n, init_conditions, dt, snr = Inf) {
  # Could potentially add option between SG filter and findiff if we want
  # to keep findiff in function.
  n <- round(n, 0)
  # eta <- eta
  dt <- dt
  # snr <- snr # provided in dB
  # n = number of time points rounded to nearest integer
  # noise = noise to be added
  # monomial_degree = degree of polynomial
  # Lorenz Parameters: sigma, rho, beta
  parameters <- c(c0 = 1, c1 = -1, c2 = -1, c3 = 1)
  # init_conditions <- c(X = -8, Y = 7, Z = 27) # Original Initial Conditions
  init_conditions <- init_conditions
  # Lorenz Function used to generate Lorenz Derivatives
  lv <- function(t, init_conditions, parameters) {
    with(as.list(c(init_conditions, parameters)), {
      dx <- (parameters[1] * init_conditions[1]) + (parameters[2] * (init_conditions[1] * init_conditions[2]))
      dy <- (parameters[3] * init_conditions[2]) + (parameters[4] * (init_conditions[1] * init_conditions[2]))
      list(c(dx, dy))
    })
  }
  # Create time span to provide to ODE function,
  # n * 0.001 creates length n given sequence incremented by 0.001
  times <- seq(0, ((n) - 1) * dt, by = dt)
  # ODE45 used to determine Lorenz Matrix
  out <- ode(y = init_conditions, times = times,
             func = lv, parms = parameters,
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

