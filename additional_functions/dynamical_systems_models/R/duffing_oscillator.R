library(deSolve) # ODE
### Duffing Oscillator
duffing_oscillator <-
  function(n,
           dt,
           init_conditions,
           gamma_value,
           kappa_value,
           epsilon_value,
           snr = Inf,
           ...) {
    n <- round(n, 0)
    dt <- dt
    # n = number of time points rounded to nearest integer
    # snr = added noise to system (dB)
    # times: n - 1 to round off total n given to start at t_init = 0
    init_conditions <- init_conditions
    times <- seq(0, ((n) - 1) * dt, by = dt)
    duff_parameters <- c(gamma_value, kappa_value, epsilon_value)
    duff_osc <- function(t,
                         init_conditions,
                         duff_parameters) {
      with(as.list(c(init_conditions,
                     duff_parameters)), {
                       dx <- init_conditions[2]
                       dy <-
                         (-duff_parameters[1] * init_conditions[2]) - (duff_parameters[2] * init_conditions[1]) - (duff_parameters[3] * (init_conditions[1] ^ 3))
                       list(c(dx, dy))
                     })
    }
    # Oscillator
    out <- ode(
      y = init_conditions,
      func = duff_osc,
      times = times,
      parms = duff_parameters,
      atol = 1.49012e-8,
      rtol = 1.49012e-8,
      ...
    )[, -1]
    # Add Noise
    if (!is.infinite(snr)) {
      length <- nrow(out) * ncol(out)
      # Convert to snr voltage (dB)
      snr_volt <- 10 ^ -(snr / 20)
      noise_matrix <-
        snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
      out <- out + noise_matrix
    }
    # Return x_t
    return(x_t = out)
  }
