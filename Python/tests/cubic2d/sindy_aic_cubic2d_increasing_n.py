#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# import packages
import itertools
import sys
import os
import pandas as pd
import numpy as np


sys.path.append("../")  # must at the file's location sys.path.append("../")
from functions import *

###################################################################################
###################################################################################
###################################################################################
###################################################################################
true_matrix_a = np.array([-0.1, 2, -2, -0.1])


def f(x, t, a):
    return [
        a[0] * x[0] ** 3 + a[1] * x[1] ** 3,
        a[2] * x[0] ** 3 + a[3] * x[1] ** 3,
    ]


dt = 0.01
snr = float(os.getenv("SNR"))  # 49
max_iter_num = os.getenv("MAX_ITER")  # 10
num_validation_sets = 100
START = float(os.getenv("START"))  # n_0 = 2
END = float(os.getenv("END"))  # n_final = 5
LEN = round(float(((END - START) * 10) + 1), 0)  # os.getenv('LEN') # here would be 301
n_exp = np.logspace(float(START), float(END), num=int(LEN)).round(0)
n_exp_total = np.repeat(n_exp, num_validation_sets)
### create threshold sequence
lambda_min = int(os.getenv("LAMBDA_MIN"))  # -4
lambda_max = int(os.getenv("LAMBDA_MAX"))  # 1
num_lambda = int(os.getenv("NUM_LAMBDA"))  # 20
threshold_sequence = np.logspace(lambda_min, lambda_max, num=num_lambda)

### Seed for reproducibility
np.random.seed(100)
poly_order = 5
### Create Random Initial Conditions
n = int(len(f(true_matrix_a, 0, true_matrix_a)))
orig_init_condition_vector = 10 ** (
    np.random.uniform(
        -2,
        2,
        n * 100,
    )
)
orig_init_condition_vector_matrix = np.split(orig_init_condition_vector, 100)
orig_init_condition_vector_matrix_new = orig_init_condition_vector_matrix * int(LEN)
validation_init_condition_vector = np.random.uniform(
    -2,
    2,
    n * 100,
)
validation_init_condition_matrix = np.split(validation_init_condition_vector, 100)
validation_init_condition_matrix_new = validation_init_condition_matrix * int(LEN)

t_total_list = []
for i in np.arange(0, len(n_exp)):
    t_total = np.arange(0, float(n_exp[i]) * dt, dt)
    t_total_list.append(t_total)

snr_volt = 10 ** -(snr / 20)


def get_results(i):
    stls_model = []
    validation_data = validation_data_generate(
        validation_init_condition_matrix,
        t_total_list[i],
        f,
        dt,
        snr_volt,
        true_matrix_a,
    )
    for j in range(len(orig_init_condition_vector_matrix)):
        training_data = training_data_generate(
            orig_init_condition_vector_matrix[j],
            t_total_list[i],
            f,
            dt,
            snr_volt,
            true_matrix_a,
        )
        stls = perform_STLS_2D(
            threshold_sequence,
            poly_order,
            t_total,
            max_iter_num,
            training_data,
            validation_init_condition_matrix_new,
            validation_data,
        )
        stls_model.append(stls)
    return stls_model


stls_models = []
for i in range(len(t_total_list)):
    stls_new = get_results(i)
    stls_models.append(stls_new)

stls_models_new = list(itertools.chain(*stls_models))

xdot_models = pd.DataFrame()
ydot_models = pd.DataFrame()

for i in range(0, len(stls_models_new)):
    xdot_models[i,] = stls_models_new[
        i
    ][:, 0]
    ydot_models[i,] = stls_models_new[
        i
    ][:, 1]

xdot_models.to_csv(
    "./sindy_aic_cubic2d_snr_%s_increasing_N%s_%s_max_iter_%s_lambda_10%s_%s_num_lambda_%s_xdot.csv"
    % (snr, START, END, max_iter_num, lambda_min, lambda_max, num_lambda)
)
ydot_models.to_csv(
    "./sindy_aic_cubic2d_snr_%s_increasing_N%s_%s_max_iter_%s_lambda_10%s_%s_num_lambda_%s_ydot.csv"
    % (snr, START, END, max_iter_num, lambda_min, lambda_max, num_lambda)
)
