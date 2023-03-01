import numpy as np
import pandas as pd
import math
from scipy import signal
from scipy.integrate import odeint
import pysindy as ps
import itertools
from itertools import chain
from scipy import integrate
from sklearn.metrics import mean_squared_error

def sg_optimal_combination_function(x_t, dt):
    ### just copy without any change
    polyorder = [4]
    wl_max = len(x_t)*0.05
    sg_combinations_df = pd.DataFrame(columns=["polyorder", "window_length"])
    if (wl_max < 11):
        sg_combinations_df = pd.DataFrame([[4,13]])
        sg_combinations_df.columns = ['polyorder', 'window_length']
    elif (wl_max > 101):
        window_length = np.arange(5,103,2)
    elif (wl_max % 2) == 0:
        wl_max = wl_max + 1
        window_length = np.arange(5,wl_max+2,2)
    else:
        window_length = np.arange(5,wl_max+2,2)
    #sg_exists = 'sg_combinations_df' in locals() or 'sg_combinations_df' in globals()
    if len(sg_combinations_df) == 0:
        sg_combinations = [(x,y) for x in polyorder for y in window_length]
        sg_combinations_df = pd.DataFrame(sg_combinations)
        sg_combinations_df.columns = ['polyorder', 'window_length']
        sg_combinations_df = sg_combinations_df.loc[sg_combinations_df.window_length > sg_combinations_df.polyorder + 8 - sg_combinations_df.polyorder%2]
    f_dist_list = []
    for k in np.arange(len(sg_combinations_df)):
        polyorder = int(sg_combinations_df.iloc[k][0])
        window_length = int(sg_combinations_df.iloc[k][1])
        train_smoothed = signal.savgol_filter(np.ravel(x_t), window_length=window_length,polyorder=polyorder,deriv=0,delta=dt)
        train_smoothed = np.array(train_smoothed)
        f_dist = mean_squared_error(x_t,train_smoothed)
        f_dist_list.append(f_dist)
    min_fdist = f_dist_list.index(min(f_dist_list))
    sg_optimal_combination = pd.DataFrame(sg_combinations_df.iloc[min_fdist]).T
    return(sg_optimal_combination)

def training_data_generate(init_conditions, t_span, f, dt, eps ,true_matrix_a):
    '''
    eps is the noise level for the data, not snr or snr(db)
    '''
    ### generate training data
    init_conditions = init_conditions
    t_span = t_span
    states = int(len(init_conditions))
    x_total = odeint(f, init_conditions, t_span, args=(true_matrix_a,)) # noiseless data
    if eps != 0:
        x_init = x_total.copy()
        for i in range(states):
            x_total[:,i] = x_total[:, i] + eps * np.random.normal(scale=np.std(x_init[:,i]), size=x_init[:,i].shape)
    x_total = x_total[0:(int(len(x_total)*0.8)),:]
    window_length = []
    polyorder = []
    for i in range(states):
        optimal_combination = np.array(sg_optimal_combination_function(x_total[:,i], dt = dt))
        window_length.append(int(optimal_combination[:,1]))
        polyorder.append(int(optimal_combination[:,0]))
    total_smoothed = []
    dot_train_smoothed = []
    for j in range(states):
        total_smoothed.append(signal.savgol_filter(np.ravel(x_total[0:,j:(j+1)]), window_length=window_length[j],polyorder=polyorder[j],deriv=0,delta=dt))
        dot_train_smoothed.append(signal.savgol_filter(np.ravel(x_total[0:,j:(j+1)]), window_length=window_length[j],polyorder=polyorder[j],deriv=1,delta=dt))
    return(np.array(dot_train_smoothed).T, np.array(total_smoothed).T, dt)

def validation_data_generate(validation_init_condition_matrix, t_span, f, dt, eps, true_matrix_a):
    ### copy from linear2d_increase_n line 280 to 305
    ### generate validation data
    x_validation_list = []
    x_validation_smoothed_list = []
    x_dot_validation_smoothed_list = []
    states = int(len(validation_init_condition_matrix[0]))
    for i in np.arange(0, len(validation_init_condition_matrix)):
        x_validation = odeint(f, validation_init_condition_matrix[i], t_span, args=(true_matrix_a,)) # validation set are all noise less
        if eps != 0:
            x_init = x_validation.copy()
            for i in range(states):
                x_validation[:,i] = x_validation[:, i] + eps * np.random.normal(scale=np.std(x_init[:,i]), size=x_init[:,i].shape)
        x_validation_list.append(x_validation)
        window_length = []
        polyorder = []
        for k in range(states):
            optimal_combination = np.array(sg_optimal_combination_function(x_validation[:,k], dt = dt))
            window_length.append(int(optimal_combination[:,1]))
            polyorder.append(int(optimal_combination[:,0]))
        validation_smoothed = []
        dot_validation_smoothed = []
        for j in range(states):
            validation_smoothed.append(signal.savgol_filter(np.ravel(x_validation[0:,j:(j+1)]), window_length=window_length[j],polyorder=polyorder[j],deriv=0,delta=dt))
            dot_validation_smoothed.append(signal.savgol_filter(np.ravel(x_validation[0:,j:(j+1)]), window_length=window_length[j],polyorder=polyorder[j],deriv=1,delta=dt))
        validation_smoothed = np.array(validation_smoothed).T
        x_validation_smoothed_list.append(validation_smoothed)
        dot_validation_smoothed = np.array(dot_validation_smoothed).T
        x_dot_validation_smoothed_list.append(dot_validation_smoothed)
    ### Validation theta list
    validation_theta_list = []
    for i in np.arange(0, len(validation_init_condition_matrix)):
        polynomials = ps.PolynomialLibrary(degree=5)
        x = np.array(x_validation_smoothed_list[i]).reshape(-1,states)
        polynomial_features = polynomials.fit_transform(x)
        validation_theta_list.append(polynomial_features)
    
    return(len(validation_init_condition_matrix), validation_theta_list, x_validation_list, x_dot_validation_smoothed_list)

def perform_STLS_2D(threshold_sequence, poly_order, t_span, max_iter_num, training_data, validation_init_condition_matrix_new, validation_data):
    ### generate data
    training_data = training_data; validation_data=validation_data
    ### poly_order
    polyorder = poly_order
    ### time span
    t_total = t_span
    ### load training data
    x_dot_smoothed, x_smoothed, dt = training_data
    ### load validation data
    nterms_sindy_AIC, validation_theta_list, x_validation_list, x_dot_validation_smoothed_list = validation_data
    ### perform STLS on original data
    #threshold_sequence = threshold_sequence
    models = []
    models_coeff = []
    for j in np.arange(0, len(threshold_sequence)):
        model = ps.SINDy(
        optimizer=ps.STLSQ(threshold=threshold_sequence[j],
        alpha = 0,
        max_iter=int(max_iter_num)),
        feature_library=ps.PolynomialLibrary(degree=poly_order)
        )
        model.fit(
            x=x_smoothed,
            t=dt,
            x_dot=x_dot_smoothed,
            quiet=True,
        )
        models.append(model)
        models_coeff.append(model.coefficients())
    ### put models in dataframe
    xdot_eta0_100_pe_df = pd.DataFrame()
    ydot_eta0_100_pe_df = pd.DataFrame()
    for i in range(0,len(models)):
        xdot_eta0_100_pe_df[i,] = models[i].coefficients()[0]
        ydot_eta0_100_pe_df[i,] = models[i].coefficients()[1]
    ### Create combinations of STLS
    sindy_combinations_list = []
    for i in np.arange(0, xdot_eta0_100_pe_df.shape[1]):
        x_pe_df = xdot_eta0_100_pe_df.iloc[:,i]
        for j in np.arange(0, ydot_eta0_100_pe_df.shape[1]):
            y_pe_df =  ydot_eta0_100_pe_df.iloc[:,j]
            sindy_combinations = np.vstack([x_pe_df, y_pe_df]).T
            sindy_combinations_list.append(sindy_combinations)
    sindy_combinations_list = np.unique(sindy_combinations_list, axis=0)
    sindy_combinations_list_new = [element for element in sindy_combinations_list if np.count_nonzero(element) != 0]
    aicc_min_list = []
    for i in np.arange(0, len(sindy_combinations_list_new)):
        current_sindy_model = sindy_combinations_list_new[i]
        x_y_sindy_abs_error = np.zeros(len(validation_theta_list))
        for j in np.arange(0, len(validation_theta_list)):
            x_hat_sindy = np.matmul(validation_theta_list[j], current_sindy_model[:,0])
            y_hat_sindy = np.matmul(validation_theta_list[j], current_sindy_model[:,1])
            x_int_sindy = integrate.cumtrapz(y = x_hat_sindy, dx = dt, initial = 0) + validation_init_condition_matrix_new[j][0]
            y_int_sindy = integrate.cumtrapz(y = y_hat_sindy, dx = dt, initial = 0) + validation_init_condition_matrix_new[j][1]
            length_data = len(validation_theta_list[j])
            nterms_sindy_abserror = current_sindy_model.shape[1]
            x_y_sindy_abs_error[j] = np.sum(np.sum((np.absolute(x_validation_list[j][:, 0] - x_int_sindy) + np.absolute(x_validation_list[j][:, 1] - y_int_sindy))) / length_data / nterms_sindy_abserror)
        log_L = (-1*nterms_sindy_AIC) * np.log(np.sum(x_y_sindy_abs_error * x_y_sindy_abs_error) / nterms_sindy_AIC) / 2
        num_nonzero_coeff = np.count_nonzero(current_sindy_model)
        if (num_nonzero_coeff > 0) & ((length_data-num_nonzero_coeff-1) != 0):
            aic = -2*log_L + 2*num_nonzero_coeff
            aicc_min = aic + 2*num_nonzero_coeff*(num_nonzero_coeff+1)/(length_data-num_nonzero_coeff-1)
        else:
            aicc_min = -2*log_L
        aicc_min_list.append(aicc_min)
    sindy_final_model = sindy_combinations_list_new[np.argmin(aicc_min_list)]
    return(sindy_final_model)

def perform_STLS_3D(threshold_sequence, poly_order, t_span, max_iter_num, training_data, validation_init_condition_matrix_new, validation_data):
    ### generate data
    training_data = training_data; validation_data=validation_data
    ### poly_order
    polyorder = poly_order
    ### time span
    t_total = t_span
    ### load training data
    x_dot_smoothed, x_smoothed, dt = training_data
    ### load validation data
    nterms_sindy_AIC, validation_theta_list, x_validation_list, x_dot_validation_smoothed_list = validation_data
    ### perform STLS on original data
    models = []
    models_coeff = []
    for j in np.arange(0, len(threshold_sequence)):
        model = ps.SINDy(
        optimizer=ps.STLSQ(threshold=threshold_sequence[j],
        alpha = 0,
        max_iter=int(max_iter_num)),
        feature_library=ps.PolynomialLibrary(degree=poly_order)
        )
        model.fit(
            x=x_smoothed,
            t=dt,
            x_dot=x_dot_smoothed,
            quiet=True,
        )
        models.append(model)
        models_coeff.append(model.coefficients())
    ### put models in dataframe
    xdot_eta0_100_pe_df = pd.DataFrame()
    ydot_eta0_100_pe_df = pd.DataFrame()
    zdot_eta0_100_pe_df = pd.DataFrame()
    for i in range(0,len(models)):
        xdot_eta0_100_pe_df[i,] = models[i].coefficients()[0]
        ydot_eta0_100_pe_df[i,] = models[i].coefficients()[1]
        zdot_eta0_100_pe_df[i,] = models[i].coefficients()[2]
    ### Create combinations of STLS
    sindy_combinations_list = []
    for i in np.arange(0, xdot_eta0_100_pe_df.shape[1]):
        x_pe_df = xdot_eta0_100_pe_df.iloc[:,i]
        for j in np.arange(0, ydot_eta0_100_pe_df.shape[1]):
            y_pe_df =  ydot_eta0_100_pe_df.iloc[:,j]
            for k in np.arange(0, zdot_eta0_100_pe_df.shape[1]):
                z_pe_df =  zdot_eta0_100_pe_df.iloc[:,k]
                sindy_combinations = np.vstack([x_pe_df, y_pe_df, z_pe_df]).T
                sindy_combinations_list.append(sindy_combinations)
    sindy_combinations_list = np.unique(sindy_combinations_list, axis=0)
    sindy_combinations_list_new = [element for element in sindy_combinations_list if np.count_nonzero(element) != 0]
    aicc_min_list = []
    for i in np.arange(0, len(sindy_combinations_list_new)):
        current_sindy_model = sindy_combinations_list_new[i]
        x_y_sindy_abs_error = np.zeros(len(validation_theta_list))
        for j in np.arange(0, len(validation_theta_list)):
            x_hat_sindy = np.matmul(validation_theta_list[j], current_sindy_model[:,0])
            y_hat_sindy = np.matmul(validation_theta_list[j], current_sindy_model[:,1])
            z_hat_sindy = np.matmul(validation_theta_list[j], current_sindy_model[:,2])
            x_int_sindy = integrate.cumtrapz(y = x_hat_sindy, dx = dt, initial = 0) + validation_init_condition_matrix_new[j][0]
            y_int_sindy = integrate.cumtrapz(y = y_hat_sindy, dx = dt, initial = 0) + validation_init_condition_matrix_new[j][1]
            z_int_sindy = integrate.cumtrapz(y = z_hat_sindy, dx = dt, initial = 0) + validation_init_condition_matrix_new[j][2]
            length_data = len(validation_theta_list[j])
            nterms_sindy_abserror = current_sindy_model.shape[1]
            x_y_sindy_abs_error[j] = np.sum(np.sum((np.absolute(x_validation_list[j][:, 0] - x_int_sindy) + 
                                                    np.absolute(x_validation_list[j][:, 1] - y_int_sindy) +
                                                    np.absolute(x_validation_list[j][:, 2] - z_int_sindy))) / length_data / nterms_sindy_abserror)
        log_L = (-1*nterms_sindy_AIC) * np.log(np.sum(x_y_sindy_abs_error * x_y_sindy_abs_error) / nterms_sindy_AIC) / 2
        num_nonzero_coeff = np.count_nonzero(current_sindy_model)
        if (num_nonzero_coeff > 0) & ((length_data-num_nonzero_coeff-1) != 0):
            aic = -2*log_L + 2*num_nonzero_coeff
            aicc_min = aic + 2*num_nonzero_coeff*(num_nonzero_coeff+1)/(length_data-num_nonzero_coeff-1)
        else:
            aicc_min = -2*log_L
        aicc_min_list.append(aicc_min)
    try:
        sindy_final_model = sindy_combinations_list_new[np.argmin(aicc_min_list)]
    except:
        sindy_final_model = np.zeros(shape=(len(x_pe_df),3))
    return(sindy_final_model)
