import numpy as np
import pandas as pd
import math
from scipy import signal
from scipy.integrate import odeint

a = 0.2
b = 0.2
c = 5.7
true_matrix_a = np.array([a, b, c])
def f(x, t, a):
    return [
        -x[1] - x[2],
        x[0] + (a[0]*x[1]),
        a[1] + (x[2]*(x[0] -  a[2]))
    ]

def rossler_ode(n, dt, init_conditions, snr):
    t_span = np.arange(0, float(n)*dt, dt) 
    x_total = odeint(f, init_conditions, t_span, args=(true_matrix_a,)) # noiseless data
    eps = 10 ** -(snr / 20)
    if eps != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:,i] = x_total[:, i] + eps * np.random.normal(scale=np.std(x_init[:,i]), size=x_init[:,i].shape)
    
    return(x_total)
    