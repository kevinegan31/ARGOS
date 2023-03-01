# dplyr from tidyverse is used for Centering and Standardizing
library(tidyverse)
library(deSolve) # ODE
# source("finite_difference.R")
### Last updated 11.01.2021
### Van der Pol Oscillator
### this is a test
vdp_oscillator <- function(n, dt, init_conditions, mu, snr = Inf) {
  n <- round(n, 0)
  # eta <- eta
  dt <- dt
  mu <- mu
  # n = number of time points rounded to nearest integer
  # noise = noise to be added
  # monomial_degree = degree of polynomial
  # mu = "negative" resistance of triode passing a small current
  # init_conditions vector corresponds to
  # https://github.com/luckystarufo/pySINDy/blob/master/examples/example-1-sindy-vanderpol.ipynb
  # init_conditions <- c(y1 = 0.1, y2 = 5) ### Original Initial Conditions
  init_conditions <- init_conditions
  times <- seq(0, ((n) - 1) * dt, by = dt)
  mu <- mu
  # higher values of mu lead to a stiff oscillator
  # (Solving Differential Equations in R)
  # (Princeton Companion to Applied Mathematics)
  # Van der Pol Oscillator
  # Solving Differential Equations in R paper
  vdpol <- function(t, init_conditions, mu) {
    with(as.list(c(init_conditions, mu)), {
      dx <- init_conditions[2]
      dy <- mu * (1 - ((init_conditions[1]) ^ 2)) * init_conditions[2] - init_conditions[1]
      list(c(dx, dy))
    })
  }
  # Oscillator
  out <- ode(y = init_conditions, func = vdpol,
             times = times, parms = mu,
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
  return(x_t = out)
}


# vdpol_measured <- vdp_oscillator(10 ^ 2, eta = 0, monomial_degree = 5, mu = 1.2, derivative = "measured")
# vdpol_centered <- vdp_oscillator(10 ^ 2, 0, 5, mu = 1.2, "findiff")
# 

