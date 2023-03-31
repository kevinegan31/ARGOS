library(deSolve) # ODE
### Van der Pol Oscillator
vdp_oscillator <- function(n, dt, init_conditions, mu, snr = Inf) {
  n <- round(n, 0)
  dt <- dt
  mu <- mu
  # mu = "negative" resistance of triode passing a small current
  # n = number of time points rounded to nearest integer
  # snr = added noise to system (dB)
  # times: n - 1 to round off total n given to start at t_init = 0
  init_conditions <- init_conditions
  times <- seq(0, ((n) - 1) * dt, by = dt)
  mu <- mu
  vdpol <- function(t, init_conditions, mu) {
    with(as.list(c(init_conditions, mu)), {
      dx <- init_conditions[2]
      dy <-
        mu * (1 - ((init_conditions[1]) ^ 2)) * init_conditions[2] - init_conditions[1]
      list(c(dx, dy))
    })
  }
  out <- ode(
    y = init_conditions,
    func = vdpol,
    times = times,
    parms = mu,
    atol = 1.49012e-8,
    rtol = 1.49012e-8
  )[,-1]
  # Add Noise
  if (!is.infinite(snr)) {
    length <- nrow(out) * ncol(out)
    # Convert to snr voltage (dB)
    snr_volt <- 10 ^ -(snr / 20)
    noise_matrix <-
      snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
    out <- out + noise_matrix
  }
  return(x_t = out)
}
