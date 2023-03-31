library(deSolve) # ODE
### Cubic 2D system
cubic2d_system <- function(n, init_conditions, dt, snr = Inf) {
  n <- round(n, 0)
  dt <- dt
  # n = number of time points rounded to nearest integer
  # snr = added noise to system (dB)
  # times: n - 1 to round off total n given to start at t_init = 0
  times <- seq(0, ((n) - 1) * dt, by = dt)
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
