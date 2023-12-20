library(deSolve) # ODE
### Rossler System
rossler_system <-
  function(n,
           dt,
           init_conditions,
           a, b, c,
           snr = Inf) {
    n <- round(n, 0)
    dt <- dt
    # n = number of time points rounded to nearest integer
    # snr = added noise to system (dB)
    # times: n - 1 to round off total n given to start at t_init = 0
    init_conditions <- init_conditions
    times <- seq(0, ((n) - 1) * dt, by = dt)
    rossler_parameters <- c(a, b, c)
    rossler <- function(t,
                        init_conditions,
                        rossler_parameters) {
      with(as.list(c(init_conditions,
                     rossler_parameters)), {
                       dx <- -init_conditions[2] - init_conditions[3]
                       dy <-
                         init_conditions[1] + (rossler_parameters[1] * init_conditions[2])
                       dz <-
                         rossler_parameters[2] + (init_conditions[3] * (init_conditions[1] - rossler_parameters[3]))
                       list(c(dx, dy, dz))
                     })
    }
    out <- ode(
      y = init_conditions,
      func = rossler,
      times = times,
      parms = rossler_parameters,
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
    # Return x_t
    return(x_t = out)
  }
