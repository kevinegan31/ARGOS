"""vdp_ode.py"""
import numpy as np
from scipy.integrate import odeint


def vdp_osc(x_t, time, mu):
    """Van der Pol Oscillator"""
    return [x_t[1], mu * (1 - x_t[0] ** 2) * x_t[1] - x_t[0]]


def vdp_ode(n_obs, dt, init_conditions, mu, snr):
    """Expand system"""
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(vdp_osc, init_conditions, t_span, args=(mu,))
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
