#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# install packages
import pandas as pd
import multiprocessing
from functions import *
import itertools
import numpy as np
import sys
import os
sys.path.append("../")  # must at the file's location sys.path.append("../")

###################################################################################
###################################################################################
###################################################################################
###################################################################################
true_matrix_a = np.array([10, 28, -8 / 3])


def f(x, t, a):
    return [
        a[0] * (x[1] - x[0]),
        x[0] * (a[1] - x[2]) - x[1],
        x[0] * x[1] + a[2] * x[2],
    ]


dt = 0.001
n_obs = int(os.getenv("N_OBS"))
noise_start = os.getenv("START")
noise_end = os.getenv("END")
by = os.getenv("BY")
noise_levels = np.arange(float(noise_start), float(noise_end), float(by))
noise_levels = np.append(noise_levels, math.inf)
# Seed for reproducibility
seed = 123  # int(os.getenv("SEED"))
np.random.seed(seed)
LIBRARY_DEGREE = int(os.getenv("LIBRARY_DEGREE"))
# Create Random Initial Conditions
num_init_conditions = int(os.getenv("NUM_INIT_CONDITIONS"))
dummy_init_conditions = [2, 0, 1]
n = int(len(f(dummy_init_conditions, 0, true_matrix_a)))
x_init_conditions = np.random.uniform(
    -15,
    15,
    num_init_conditions,
)
y_init_conditions = np.random.uniform(
    -15,
    15,
    num_init_conditions,
)
z_init_conditions = np.random.uniform(
    10,
    40,
    num_init_conditions,
)
orig_init_condition_vector_matrix = [
    i
    for tup in zip(x_init_conditions, y_init_conditions, z_init_conditions)
    for i in tup
]
orig_init_condition_vector_matrix = np.split(
    np.array(orig_init_condition_vector_matrix), num_init_conditions
)
orig_init_condition_vector_matrix_new = orig_init_condition_vector_matrix * int(
    len(noise_levels)
)


t_total = np.arange(0, float(n_obs) * dt, dt)

# cores = int(os.getenv('CORES'))
snr_volt = 10 ** -(noise_levels / 20)

LIBRARY_TYPE = os.getenv("LIBRARY_TYPE")  # 'poly' #os.getenv("LIBRARY_TYPE")
sparsity_threshold = float(os.getenv("SPARSITY_THRESHOLD"))
n_models = int(os.getenv("N_MODELS"))
inclusion_proba = float(os.getenv("INCLUSION_PROBA"))
method = os.getenv("METHOD")
library_ens = os.getenv("LIBRARY_ENS")


all_input_values = []
for i in range(len(snr_volt)):
    input_args = [
        i,
        orig_init_condition_vector_matrix,
        t_total,
        f,
        dt,
        snr_volt,
        true_matrix_a,
        LIBRARY_TYPE,
        LIBRARY_DEGREE,
        sparsity_threshold,
        n_models,
        inclusion_proba,
        method,
        library_ens,
    ]
    all_input_values.append(input_args)


MC_THREAD = 20
with Pool(MC_THREAD) as p:
    sindy_models = p.map(
        get_results_increasing_snr_ensemble_sindy, all_input_values)
    p.close()
    p.join()


xdot_models = pd.DataFrame()
ydot_models = pd.DataFrame()
zdot_models = pd.DataFrame()
models = []
runtime = []
inclusion_probab = []
for i in range(len(sindy_models)):
    models.append(sindy_models[i][0])
    inclusion_probab.append(sindy_models[i][1])


inclusion_proba_new = list(itertools.chain(*inclusion_probab))

xdot_inclusion_probability = pd.DataFrame()
ydot_inclusion_probability = pd.DataFrame()
zdot_inclusion_probability = pd.DataFrame()

for i in range(len(inclusion_proba_new)):
    # Extract the actual array from the nested structure
    actual_array = inclusion_proba_new[i][0]
    # Now, we can populate the DataFrame
    xdot_inclusion_probability[i] = actual_array[0, :]
    ydot_inclusion_probability[i] = actual_array[1, :]
    zdot_inclusion_probability[i] = actual_array[2, :]


models_new = list(itertools.chain(*models))
models_final = list(itertools.chain(*models_new))
for i in range(0, len(models_new)):
    xdot_models[i,] = models_final[
        i
    ][0, :]
    ydot_models[i,] = models_final[
        i
    ][1, :]
    zdot_models[i,] = models_final[
        i
    ][2, :]


xdot_inclusion_probability.to_csv(
    f"./{library_ens}_{method}_ensemble_{inclusion_proba}_sindy_lorenz_degree_{LIBRARY_DEGREE}_snr_{noise_start}_{noise_end}_N{n_obs}_SEED_{seed}_n_models_{n_models}_thresh_{sparsity_threshold}_SEED_{seed}_xdot_probabilities_df.csv"
)
ydot_inclusion_probability.to_csv(
    f"./{library_ens}_{method}_ensemble_{inclusion_proba}_sindy_lorenz_degree_{LIBRARY_DEGREE}_snr_{noise_start}_{noise_end}_N{n_obs}_SEED_{seed}_n_models_{n_models}_thresh_{sparsity_threshold}_SEED_{seed}_ydot_probabilities_df.csv"
)
zdot_inclusion_probability.to_csv(
    f"./{library_ens}_{method}_ensemble_{inclusion_proba}_sindy_lorenz_degree_{LIBRARY_DEGREE}_snr_{noise_start}_{noise_end}_N{n_obs}_SEED_{seed}_n_models_{n_models}_thresh_{sparsity_threshold}_SEED_{seed}_zdot_probabilities_df.csv"
)
xdot_models.to_csv(
    f"./{library_ens}_{method}_ensemble_{inclusion_proba}_sindy_lorenz_degree_{LIBRARY_DEGREE}_snr_{noise_start}_{noise_end}_N{n_obs}_SEED_{seed}_n_models_{n_models}_thresh_{sparsity_threshold}_SEED_{seed}_xdot_model_df.csv"
)
ydot_models.to_csv(
    f"./{library_ens}_{method}_ensemble_{inclusion_proba}_sindy_lorenz_degree_{LIBRARY_DEGREE}_snr_{noise_start}_{noise_end}_N{n_obs}_SEED_{seed}_n_models_{n_models}_thresh_{sparsity_threshold}_SEED_{seed}_ydot_model_df.csv"
)
zdot_models.to_csv(
    f"./{library_ens}_{method}_ensemble_{inclusion_proba}_sindy_lorenz_degree_{LIBRARY_DEGREE}_snr_{noise_start}_{noise_end}_N{n_obs}_SEED_{seed}_n_models_{n_models}_thresh_{sparsity_threshold}_SEED_{seed}_zdot_model_df.csv"
)
