import itertools
import numpy as np
import sys
import os
# sys.path.append("../") # must at the file's location sys.path.append("../")
import multiprocessing
import pandas as pd
sys.path.append("/Users/kevinegan/Documents/GitHub.nosync/PrivateAutomaticSparseRegression/Python_Code/")
from functions_mangan_code import *
###################################################################################
###################################################################################
###################################################################################
###################################################################################
true_matrix_a = np.array([10, 28, -8/3])
def f(x, t, a):
    return [
        a[0] * (x[1] - x[0]),
        x[0] * (a[1] - x[2]) - x[1],
        x[0] * x[1] + a[2] * x[2],
    ]

dt = 0.001
n_obs = 5000 #int(os.getenv('N_OBS')) 
max_iter_num = 10 #os.getenv('MAX_ITER') # 10
num_validation_sets = 100
noise_levels = [49]
### create threshold sequence
lambda_min = -6 #int(os.getenv('LAMBDA_MIN')) # -6
lambda_max = 2 #int(os.getenv('LAMBDA_MAX')) # 2
num_lambda = 30 #int(os.getenv('NUM_LAMBDA')) # 30
threshold_sequence = np.logspace(lambda_min, lambda_max, num = num_lambda)
### Seed for reproducibility
seed = 100
np.random.seed(seed)
poly_order = 5
### Create Random Initial Conditions
n=int(len(f(true_matrix_a,0,true_matrix_a)))
x_orig_init_conditions = np.random.uniform(-15,15,100,)
y_orig_init_conditions = np.random.uniform(-15,15,100,)
z_orig_init_conditions = np.random.uniform(10,40,100,)
orig_init_condition_vector = [i for tup in zip(x_orig_init_conditions,y_orig_init_conditions,z_orig_init_conditions) for i in tup]
orig_init_condition_vector_matrix = np.split(np.array(orig_init_condition_vector), 100)
orig_init_condition_vector_matrix_new = orig_init_condition_vector_matrix * int(len(noise_levels))
x_val_init_conditions = np.random.uniform(-15,15,100,)
y_val_init_conditions = np.random.uniform(-15,15,100,)
z_val_init_conditions = np.random.uniform(10,40,100,)
validation_init_condition_vector = [i for tup in zip(x_val_init_conditions,y_val_init_conditions,z_val_init_conditions) for i in tup]
validation_init_condition_matrix = np.split(np.array(validation_init_condition_vector), 100)
validation_init_condition_matrix_new = validation_init_condition_matrix * int(len(noise_levels))

t_total = np.arange(0, float(n_obs)*dt, dt) 

noise_levels = np.array((noise_levels))
snr_volt = 10 ** -(noise_levels / 20)

def get_results(i):
    stls_model = []
    validation_data = validation_data_generate(validation_init_condition_matrix, t_total, f, dt, snr_volt[i], true_matrix_a)
    for j in range(len(orig_init_condition_vector_matrix)):
        training_data = training_data_generate(orig_init_condition_vector_matrix_new[j], t_total, f, dt, snr_volt[i], true_matrix_a)
        stls = perform_STLS_3D(threshold_sequence, poly_order, t_total, max_iter_num, training_data, validation_init_condition_matrix_new, validation_data)
        stls_model.append(stls)
    return(stls_model)

import time
start = time.time()
stls_models = []
for i in range(len(snr_volt)):
    stls_new = get_results(i)
    stls_models.append(stls_new)

end = time.time()
print(end - start)