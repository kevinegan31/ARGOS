#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# install packages
import itertools
import numpy as np
import sys
import os
sys.path.append("../") # must at the file's location sys.path.append("../")
from functions import *
import multiprocessing
import pandas as pd
###################################################################################
###################################################################################
###################################################################################
###################################################################################
true_matrix_a = np.array([0.1, 1, 5])
def f(x, t, a):
  return [
          x[1],
          ((-a[0] * x[1]) - (a[1]*x[0]) - (a[2]*(x[0]**3)))
  ]

dt = 0.01
n_obs = int(os.getenv('N_OBS')) # 5000
max_iter_num = os.getenv('MAX_ITER') # 10
num_validation_sets = 100
noise_start = os.getenv('START') # 1
noise_end = os.getenv('END') # 62
by = os.getenv('BY')
noise_levels = np.arange(float(noise_start), float(noise_end), float(by))
noise_levels = np.append(noise_levels, math.inf)
noise_total = np.repeat(noise_levels, 100)
### create threshold sequence
lambda_min = int(os.getenv('LAMBDA_MIN')) # -6
lambda_max = int(os.getenv('LAMBDA_MAX')) # 2
num_lambda = int(os.getenv('NUM_LAMBDA')) # 30
threshold_sequence = np.logspace(lambda_min, lambda_max, num = num_lambda)
### Seed for reproducibility
seed = 100
np.random.seed(seed)
poly_order = 5
### Create Random Initial Conditions
n=int(len(f(true_matrix_a,0,true_matrix_a)))
x_orig_init_conditions = np.random.uniform(-2,2,100,)
y_orig_init_conditions = np.random.uniform(-6,6,100,)
orig_init_condition_vector = [i for tup in zip(x_orig_init_conditions,y_orig_init_conditions) for i in tup]
orig_init_condition_vector_matrix = np.split(orig_init_condition_vector, 100)
orig_init_condition_vector_matrix_new = orig_init_condition_vector_matrix * int(len(noise_levels))
x_init_conditions = np.random.uniform(-2,2,100,)
y_init_conditions = np.random.uniform(-6,6,100,)
validation_init_condition_vector = [i for tup in zip(x_init_conditions,y_init_conditions) for i in tup]
validation_init_condition_matrix = np.split(np.array(validation_init_condition_vector), 100)
validation_init_condition_matrix_new = validation_init_condition_matrix * int(len(noise_levels))

t_total = np.arange(0, float(n_obs)*dt, dt) 
### SNR
snr_volt = 10 ** -(noise_levels / 20)

def get_results(i):
    stls_model = []
    validation_data = validation_data_generate(validation_init_condition_matrix, t_total, f, dt, snr_volt[i], true_matrix_a)
    for j in range(len(orig_init_condition_vector_matrix)):
        training_data = training_data_generate(orig_init_condition_vector_matrix_new[j], t_total, f, dt, snr_volt[i], true_matrix_a)
        stls = perform_STLS_2D(threshold_sequence, poly_order, t_total, max_iter_num, training_data, validation_init_condition_matrix_new, validation_data)
        stls_model.append(stls)
    return(stls_model)

stls_models = []
for i in range(len(snr_volt)):
    stls_new = get_results(i)
    stls_models.append(stls_new)

stls_models_new = list(itertools.chain(*stls_models))

xdot_models = pd.DataFrame()
ydot_models = pd.DataFrame()

for i in range(0,len(stls_models_new)):
    xdot_models[i,] = stls_models_new[i][:,0]
    ydot_models[i,] = stls_models_new[i][:,1]


xdot_models.to_csv('./sindy_aic_duffing_increasing_snr_%s_%s_N%s_max_iter_%s_SEED_%s_lambda_%s_%s_num_lambda_%s_xdot.csv' % (noise_start, noise_end, n_obs, max_iter_num, seed, lambda_min, lambda_max, num_lambda))
ydot_models.to_csv('./sindy_aic_duffing_increasing_snr_%s_%s_N%s_max_iter_%s_SEED_%s_lambda_%s_%s_num_lambda_%s_ydot.csv' % (noise_start, noise_end, n_obs, max_iter_num, seed, lambda_min, lambda_max, num_lambda))
