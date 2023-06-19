"""linear3d_ode.py"""
import numpy as np
from scipy.integrate import odeint

true_matrix_a = np.array([-0.1, 2, -2, -0.1, -0.3])


def linear3d(x_t, time, parameters):
    """Three-dimensional linear system"""
    return [
        parameters[0] * x_t[0] + parameters[1] * x_t[1],
        parameters[2] * x_t[0] + parameters[3] * x_t[1],
        parameters[4] * x_t[2],
    ]


def linear3d_ode(n_obs, dt, init_conditions, snr):
    """Expand system"""
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(linear3d, init_conditions, t_span, args=(true_matrix_a,))
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
