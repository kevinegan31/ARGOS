"""rossler_ode.py"""
import numpy as np
from scipy.integrate import odeint

# Rossler parameters
a = 0.2
b = 0.2
c = 5.7
true_matrix_a = np.array([a, b, c])


def rossler_eq(x_t, time, parameters):
    """Rossler system"""
    return [
        -x_t[1] - x_t[2],
        x_t[0] + (parameters[0] * x_t[1]),
        parameters[1] + (x_t[2] * (x_t[0] - parameters[2])),
    ]


def rossler_ode(n_obs, dt, init_conditions, snr):
    """Expand system"""
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(rossler_eq, init_conditions, t_span, args=(true_matrix_a,))
    # Convert snr (dB) to voltage
    snr_volt = 10 ** -(snr / 20)
    # Add noise (dB)
    if snr_volt != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:, i] = x_total[:, i] + snr_volt * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )

    return x_total
