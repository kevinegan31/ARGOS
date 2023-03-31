"""lorenz_ode.py"""
import numpy as np
from scipy.integrate import odeint

# Lorenz parameters
true_matrix_a = np.array([10, 28, -8 / 3])


def lorenz_eq(x_t, t, parameters):
    """Lorenz system"""
    return [
        parameters[0] * (x_t[1] - x_t[0]),
        x_t[0] * (parameters[1] - x_t[2]) - x_t[1],
        x_t[0] * x_t[1] + parameters[2] * x_t[2],
    ]


def lorenz_ode(n_obs, dt, init_conditions, snr):
    """Expand system"""
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(lorenz_eq, init_conditions, t_span, args=(true_matrix_a,))
    snr_db = 10 ** -(snr / 20)
    # Add noise (dB)
    if snr_db != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:, i] = x_total[:, i] + snr_db * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )

    return x_total
