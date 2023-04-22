library(deSolve) # ODE
### Lotka Volterra
lotka_volterra <- function(n, init_conditions, dt, snr = Inf) {
  n <- round(n, 0)
  dt <- dt
  # n = number of time points rounded to nearest integer
  # snr = added noise to system (dB)
  # times: n - 1 to round off total n given to start at t_init = 0
  parameters <- c(c0 = 1, c1 = -1, c2 = -1, c3 = 1)
  init_conditions <- init_conditions
  lv <- function(t, init_conditions, parameters) {
    with(as.list(c(init_conditions, parameters)), {
      dx <- (parameters[1] * init_conditions[1]) + (parameters[2] * (init_conditions[1] * init_conditions[2]))
      dy <- (parameters[3] * init_conditions[2]) + (parameters[4] * (init_conditions[1] * init_conditions[2]))
      list(c(dx, dy))
    })
  }
  times <- seq(0, ((n) - 1) * dt, by = dt)
  out <- ode(y = init_conditions, times = times,
             func = lv, parms = parameters,
             atol = 1.49012e-8, rtol = 1.49012e-8)[, -1]
  # Add Noise
  if (!is.infinite(snr)) {
    length <- nrow(out) * ncol(out)
    # Convert to snr voltage (dB)
    snr_volt <- 10 ^ -(snr / 20)
    noise_matrix <- snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
    out <- out + noise_matrix
  }
  # Return x_t
  return(x_t = out)
}
