# dplyr from tidyverse is used for Centering and Standardizing
library(tidyverse)
library(deSolve) # ODE
library(signal) # smoothing
# source("finite_difference.R")
### Last updated 18.02.2021
### Removed 1's column
# source("finite_difference.R")
### Lorenz Chaotic System
lorenz_system <- function(n, init_conditions, dt, snr = Inf) {
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
  parameters <- c(s = 10, r = 28, b = 8 / 3)
  # init_conditions <- c(X = -8, Y = 7, Z = 27) # Original Initial Conditions
  init_conditions <- init_conditions
  # Lorenz Function used to generate Lorenz Derivatives
  lorenz <- function(t, init_conditions, parameters) {
    with(as.list(c(init_conditions, parameters)), {
      dx <- parameters[1] * (init_conditions[2] - init_conditions[1])
      dy <- init_conditions[1] * (parameters[2] - init_conditions[3]) - init_conditions[2]
      dz <- init_conditions[1] * init_conditions[2] - parameters[3] * init_conditions[3]
      list(c(dx, dy, dz))
    })
  }
  # Create time span to provide to ODE function,
  # n * 0.001 creates length n given sequence incremented by 0.001
  times <- seq(0, ((n) - 1) * dt, by = dt)
  # ODE45 used to determine Lorenz Matrix
  out <- ode(y = init_conditions, times = times,
             func = lorenz, parms = parameters,
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


# n <- 10^7
# eta <- 0
# dt <- 0.001
# init_conditions <- c(-8, 7, 27)
# lorenz_test <- lorenz_system(n, eta, dt, init_conditions)
# colnames(lorenz_test)[1] <- "X"
# colnames(lorenz_test)[2] <- "Y"
# colnames(lorenz_test)[3] <- "Z"
# write.csv(lorenz_test, "/Users/kevinegan/Library/CloudStorage/OneDrive-DurhamUniversity/Proc_Royal_Soc_A/Data/Lorenz/Lorenz_Attractor/lorenz_attractor.csv")

# noise_mag <- seq(1, 100, 10)
# # sapply(noise_mag, sd(out)/noise_mag)
# for (i in seq_along(noise_mag)) {
#   # print(sd(out) / noise_mag[i])
#   print(10^(noise_mag[i]/20))
# }

# snr <- 40
# out <-
#   out + ((sd(out) / (10 ^ (snr / 20))) * matrix(rnorm(length, mean = 0, sd = 1), nrow(out)))
# noise_mag <- seq(0, 100, length = 101)
# sapply(noise_mag, FUN = function(x) { sd(out) /  (10 ^ (x / 20))})
# 
# L <- length(out)
# e_sym <- (sum(abs(out) ** 2)) / L # symbolic energy
# snr_db <- seq(0, 100, by = 1)
# sapply(snr_db, FUN = function(x) {
#   snr <- 10 ** (x / 10)
#   N0 <- (e_sym / L) / snr
#   sqrt(N0)})
# 
# 
# out <- lorenz_system(100, c(-8,7,27), dt = 0.001, snr = Inf)
# 
# L <- length(out)
# e_sym <- (sum(abs(out) ** 2)) / L # symbolic energy
# snr_db <- 6
# snr_lin <- 10 ^ (snr_db / 10)
# snr_volt <- 10 ^ (snr_db / 20)
# N0 <- e_sym / snr_lin
# noise_sigma <- sqrt(N0)
# noise_sigma
# noise_matrix <- noise_sigma * matrix(rnorm(length, mean = 0, sd = 1), nrow(out))
# head(out + noise_matrix)
# sd(out) / sqrt(snr_lin)
# 
# snr_db <- seq(-40, -20, by = 1)
# sapply(snr_db, FUN = function(x) {
#   snr_volt <- 10 ^ -(x / 20)
#   sd(out) * snr_volt})
# sapply(snr_db, FUN = function(x) {
#   snr_volt <- 10 ^ -(x / 20)
#   round(snr_volt, 4)})
# snr_db <- seq(10, 75, by = 1)
# snr_db <- Inf
# snr_volt <- 10 ^ -(snr_db / 20)
# sd(out) * (snr_volt)
# 
# eta <- seq(0, 0.02, by = 0.001)
# snr_db <- 20*log10(eta)
# eta
# sd(out) * eta
# sd(out) * (10**(snr_db/20))
# 
# snr_db <- 60
# snr_volt <- 10 ^ -(snr_db / 20)
# sd(out) * (snr_volt)
# ### Get snr_db from previous eta values
# eta <- seq(0, 0.02, by = 0.001)
# eta * sd(out)
# sd(out) / (eta * sd(out))
# snr_db <- 20*log10(sd(out) / (eta * sd(out)))
# snr_db <- 20*log10(1 / (eta))
# snr_volt <- 10 ^ -(snr_db / 20)
# snr_db
# snr_volt
# sd(out) * (snr_volt)

# seq(10, 60, by = 5)
# 
# snr_db <- 0
# snr <- 10 ** (snr_db / 10)
# N0 <- e_sym / snr
# sqrt(N0)
# n <- out + (sqrt(N0) * matrix(rnorm(length, mean = 0, sd = 1), nrow(out)))
# 
# snr_db <- seq(0, 100, length = 101)
# sapply(snr_db, FUN = function(x) {
#   snr <- 10 ** (x / 10)
#   N0 <- (e_sym / L) / snr
#   sqrt(N0)})

