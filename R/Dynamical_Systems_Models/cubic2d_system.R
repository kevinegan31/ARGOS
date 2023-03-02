# dplyr from tidyverse is used for Centering and Standardizing
library(tidyverse)
library(deSolve) # ODE
# source("finite_difference.R")
# Last updated 18.12.2020
### Cubic 2D system
cubic2d_system <- function(n, init_conditions, dt, snr = Inf) {
  n <- round(n, 0)
  # eta <- eta
  dt <- dt
  # n = number of time points rounded to nearest integer
  # noise = noise to be added
  # monomial_degree = degree of polynomial
  # times: n - 1 to round off total n given that we start at 0
  times <- seq(0, ((n) - 1) * dt, by = dt)
  # init_conditions <- c(X = 2, Y = 0) # Original Initial Conditions
  init_conditions <- init_conditions
  matrix_a <- matrix(c(-0.1, -2,
                       2, -0.1), 2, 2)
  cubic2d <- function(t, init_conditions, parameters) {
    with(as.list(c(init_conditions, parameters)), {
      dx <- matrix_a[1, 1] * init_conditions[1] ** 3 + matrix_a[1, 2] * init_conditions[2] ** 3
      dy <- matrix_a[2, 1] * init_conditions[1] ** 3 + matrix_a[2, 2] * init_conditions[2] ** 3
      list(c(dx, dy))
    })
  }
  out <- ode(y = init_conditions, times = times,
             func = cubic2d, parms = matrix_a,
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
