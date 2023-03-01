import numpy as np
import pandas as pd
import math
from scipy import signal
from scipy.integrate import odeint

true_matrix_a = np.array([-0.1, 2, -2, -0.1])
def f(x, t, a):
    return [
        a[0] * x[0] + a[1] * x[1],
        a[2] * x[0] + a[3] * x[1]
    ]

def linear2d_ode(n, dt, init_conditions, snr):
    t_span = np.arange(0, float(n)*dt, dt) 
    x_total = odeint(f, init_conditions, t_span, args=(true_matrix_a,)) # noiseless data
    eps = 10 ** -(snr / 20)
    if eps != 0:        
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:,i] = x_total[:, i] + eps * np.random.normal(scale=np.std(x_init[:,i]), size=x_init[:,i].shape)
    
    return(x_total)