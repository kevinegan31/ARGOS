"""functions.py"""
import numpy as np
import pandas as pd
from scipy import signal
from scipy.integrate import odeint
import pysindy as ps
from scipy import integrate
from sklearn.metrics import mean_squared_error


def sg_optimal_combination_function(x_t, dt):
    """Optimal Savitzky-Golay filter"""
    polyorder = [4]
    wl_max = len(x_t) * 0.05
    sg_combinations_df = pd.DataFrame(columns=["polyorder", "window_length"])
    # Build grid of window lengths
    if wl_max < 11:
        sg_combinations_df = pd.DataFrame([[4, 13]])
        sg_combinations_df.columns = ["polyorder", "window_length"]
    elif wl_max > 101:
        window_length = np.arange(5, 103, 2)
    elif (wl_max % 2) == 0:
        wl_max = wl_max + 1
        window_length = np.arange(5, wl_max + 2, 2)
    else:
        window_length = np.arange(5, wl_max + 2, 2)
    if len(sg_combinations_df) == 0:
        sg_combinations = [(x, y) for x in polyorder for y in window_length]
        sg_combinations_df = pd.DataFrame(sg_combinations)
        sg_combinations_df.columns = ["polyorder", "window_length"]
        sg_combinations_df = sg_combinations_df.loc[
            sg_combinations_df.window_length
            > sg_combinations_df.polyorder + 8 - sg_combinations_df.polyorder % 2
        ]
    # Determine combination with minimum MSE
    f_dist_list = []
    for k in np.arange(len(sg_combinations_df)):
        polyorder = int(sg_combinations_df.iloc[k][0])
        window_length = int(sg_combinations_df.iloc[k][1])
        train_smoothed = signal.savgol_filter(
            np.ravel(x_t),
            window_length=window_length,
            polyorder=polyorder,
            deriv=0,
            delta=dt,
        )
        train_smoothed = np.array(train_smoothed)
        f_dist = mean_squared_error(x_t, train_smoothed)
        f_dist_list.append(f_dist)
    min_fdist = f_dist_list.index(min(f_dist_list))
    sg_optimal_combination = pd.DataFrame(sg_combinations_df.iloc[min_fdist]).T
    return sg_optimal_combination


def training_data_generate(init_conditions, t_span, f, dt, snr_db, true_matrix_a):
    """
    Generate Training Data
    """
    # generate training data
    init_condition_values = init_conditions
    t_span_values = t_span
    states = int(len(init_conditions))
    x_total = odeint(f, init_condition_values,
                     t_span_values, args=(true_matrix_a,))
    # Add noise to system
    if snr_db != 0:
        x_init = x_total.copy()
        for i in range(states):
            x_total[:, i] = x_total[:, i] + snr_db * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )
    x_total = x_total[0: (int(len(x_total) * 0.8)), :]
    # Determine optimal SG function and smooth output
    window_length = []
    polyorder = []
    for i in range(states):
        optimal_combination = np.array(
            sg_optimal_combination_function(x_total[:, i], dt=dt)
        )
        window_length.append(int(optimal_combination[:, 1]))
        polyorder.append(int(optimal_combination[:, 0]))
    total_smoothed = []
    dot_train_smoothed = []
    for j in range(states):
        total_smoothed.append(
            signal.savgol_filter(
                np.ravel(x_total[0:, j: (j + 1)]),
                window_length=window_length[j],
                polyorder=polyorder[j],
                deriv=0,
                delta=dt,
            )
        )
        dot_train_smoothed.append(
            signal.savgol_filter(
                np.ravel(x_total[0:, j: (j + 1)]),
                window_length=window_length[j],
                polyorder=polyorder[j],
                deriv=1,
                delta=dt,
            )
        )
    return (np.array(dot_train_smoothed).T, np.array(total_smoothed).T, dt)


def validation_data_generate(
    validation_init_condition_matrix, t_span, f, dt, snr_db, true_matrix_a
):
    """Generate validation data"""
    x_validation_list = []
    x_validation_smoothed_list = []
    x_dot_validation_smoothed_list = []
    states = int(len(validation_init_condition_matrix[0]))
    for i in np.arange(0, len(validation_init_condition_matrix)):
        x_validation = odeint(
            f, validation_init_condition_matrix[i], t_span, args=(
                true_matrix_a,)
        )  # validation set are all noise less
        # Add noise
        if snr_db != 0:
            x_init = x_validation.copy()
            for i in range(states):
                x_validation[:, i] = x_validation[:, i] + snr_db * np.random.normal(
                    scale=np.std(x_init[:, i]), size=x_init[:, i].shape
                )
        x_validation_list.append(x_validation)
        window_length = []
        polyorder = []
        for k in range(states):
            optimal_combination = np.array(
                sg_optimal_combination_function(x_validation[:, k], dt=dt)
            )
            window_length.append(int(optimal_combination[:, 1]))
            polyorder.append(int(optimal_combination[:, 0]))
        validation_smoothed = []
        dot_validation_smoothed = []
        for j in range(states):
            validation_smoothed.append(
                signal.savgol_filter(
                    np.ravel(x_validation[0:, j: (j + 1)]),
                    window_length=window_length[j],
                    polyorder=polyorder[j],
                    deriv=0,
                    delta=dt,
                )
            )
            dot_validation_smoothed.append(
                signal.savgol_filter(
                    np.ravel(x_validation[0:, j: (j + 1)]),
                    window_length=window_length[j],
                    polyorder=polyorder[j],
                    deriv=1,
                    delta=dt,
                )
            )
        validation_smoothed = np.array(validation_smoothed).T
        x_validation_smoothed_list.append(validation_smoothed)
        dot_validation_smoothed = np.array(dot_validation_smoothed).T
        x_dot_validation_smoothed_list.append(dot_validation_smoothed)
    # Validation theta list
    validation_theta_list = []
    for i in np.arange(0, len(validation_init_condition_matrix)):
        polynomials = ps.PolynomialLibrary(degree=5)
        x = np.array(x_validation_smoothed_list[i]).reshape(-1, states)
        polynomial_features = polynomials.fit_transform(x)
        validation_theta_list.append(polynomial_features)

    return (
        len(validation_init_condition_matrix),
        validation_theta_list,
        x_validation_list,
        x_dot_validation_smoothed_list,
    )


'''SINDy with AIC'''


def perform_STLS_2D(
    threshold_sequence,
    poly_order,
    t_span,
    max_iter_num,
    training_data,
    validation_init_condition_matrix_new,
    validation_data,
):
    """SINDy with AIC for two-dimensional systems"""
    # generate data
    training_data = training_data
    validation_data = validation_data
    # poly_order
    polyorder = poly_order
    # time span
    t_total = t_span
    # load training data
    x_dot_smoothed, x_smoothed, dt = training_data
    # load validation data
    (
        nterms_sindy_AIC,
        validation_theta_list,
        x_validation_list,
        x_dot_validation_smoothed_list,
    ) = validation_data
    # perform STLS on original data
    # threshold_sequence = threshold_sequence
    models = []
    models_coeff = []
    for j in np.arange(0, len(threshold_sequence)):
        model = ps.SINDy(
            optimizer=ps.STLSQ(
                threshold=threshold_sequence[j], alpha=0, max_iter=int(max_iter_num)
            ),
            feature_library=ps.PolynomialLibrary(degree=poly_order),
        )
        model.fit(
            x=x_smoothed,
            t=dt,
            x_dot=x_dot_smoothed,
            quiet=True,
        )
        models.append(model)
        models_coeff.append(model.coefficients())
    # put models in dataframe
    xdot_eta0_100_pe_df = pd.DataFrame()
    ydot_eta0_100_pe_df = pd.DataFrame()
    for i in range(0, len(models)):
        xdot_eta0_100_pe_df[i,] = models[
            i
        ].coefficients()[0]
        ydot_eta0_100_pe_df[i,] = models[
            i
        ].coefficients()[1]
    # Create combinations of STLS
    sindy_combinations_list = []
    for i in np.arange(0, xdot_eta0_100_pe_df.shape[1]):
        x_pe_df = xdot_eta0_100_pe_df.iloc[:, i]
        for j in np.arange(0, ydot_eta0_100_pe_df.shape[1]):
            y_pe_df = ydot_eta0_100_pe_df.iloc[:, j]
            sindy_combinations = np.vstack([x_pe_df, y_pe_df]).T
            sindy_combinations_list.append(sindy_combinations)
    sindy_combinations_list = np.unique(sindy_combinations_list, axis=0)
    sindy_combinations_list_new = [
        element for element in sindy_combinations_list if np.count_nonzero(element) != 0
    ]
    aicc_min_list = []
    for i in np.arange(0, len(sindy_combinations_list_new)):
        current_sindy_model = sindy_combinations_list_new[i]
        x_y_sindy_abs_error = np.zeros(len(validation_theta_list))
        for j in np.arange(0, len(validation_theta_list)):
            x_hat_sindy = np.matmul(
                validation_theta_list[j], current_sindy_model[:, 0])
            y_hat_sindy = np.matmul(
                validation_theta_list[j], current_sindy_model[:, 1])
            x_int_sindy = (
                integrate.cumtrapz(y=x_hat_sindy, dx=dt, initial=0)
                + validation_init_condition_matrix_new[j][0]
            )
            y_int_sindy = (
                integrate.cumtrapz(y=y_hat_sindy, dx=dt, initial=0)
                + validation_init_condition_matrix_new[j][1]
            )
            length_data = len(validation_theta_list[j])
            nterms_sindy_abserror = current_sindy_model.shape[1]
            x_y_sindy_abs_error[j] = np.sum(
                np.sum(
                    (
                        np.absolute(x_validation_list[j][:, 0] - x_int_sindy)
                        + np.absolute(x_validation_list[j][:, 1] - y_int_sindy)
                    )
                )
                / length_data
                / nterms_sindy_abserror
            )
        log_L = (
            (-1 * nterms_sindy_AIC)
            * np.log(
                np.sum(x_y_sindy_abs_error * x_y_sindy_abs_error) /
                nterms_sindy_AIC
            )
            / 2
        )
        num_nonzero_coeff = np.count_nonzero(current_sindy_model)
        if (num_nonzero_coeff > 0) & ((length_data - num_nonzero_coeff - 1) != 0):
            aic = -2 * log_L + 2 * num_nonzero_coeff
            aicc_min = aic + 2 * num_nonzero_coeff * (num_nonzero_coeff + 1) / (
                length_data - num_nonzero_coeff - 1
            )
        else:
            aicc_min = -2 * log_L
        aicc_min_list.append(aicc_min)
    sindy_final_model = sindy_combinations_list_new[np.argmin(aicc_min_list)]
    return sindy_final_model


def perform_STLS_3D(
    threshold_sequence,
    poly_order,
    t_span,
    max_iter_num,
    training_data,
    validation_init_condition_matrix_new,
    validation_data,
):
    """Perform SINDy with AIC for 3D systems"""
    # generate data
    training_data = training_data
    validation_data = validation_data
    # poly_order
    polyorder = poly_order
    # time span
    t_total = t_span
    # load training data
    x_dot_smoothed, x_smoothed, dt = training_data
    # load validation data
    (
        nterms_sindy_AIC,
        validation_theta_list,
        x_validation_list,
        x_dot_validation_smoothed_list,
    ) = validation_data
    # perform STLS on original data
    models = []
    models_coeff = []
    for j in np.arange(0, len(threshold_sequence)):
        model = ps.SINDy(
            optimizer=ps.STLSQ(
                threshold=threshold_sequence[j], alpha=0, max_iter=int(max_iter_num)
            ),
            feature_library=ps.PolynomialLibrary(degree=poly_order),
        )
        model.fit(
            x=x_smoothed,
            t=dt,
            x_dot=x_dot_smoothed,
            quiet=True,
        )
        models.append(model)
        models_coeff.append(model.coefficients())
    # put models in dataframe
    xdot_eta0_100_pe_df = pd.DataFrame()
    ydot_eta0_100_pe_df = pd.DataFrame()
    zdot_eta0_100_pe_df = pd.DataFrame()
    for i in range(0, len(models)):
        xdot_eta0_100_pe_df[i,] = models[
            i
        ].coefficients()[0]
        ydot_eta0_100_pe_df[i,] = models[
            i
        ].coefficients()[1]
        zdot_eta0_100_pe_df[i,] = models[
            i
        ].coefficients()[2]
    # Create combinations of STLS
    sindy_combinations_list = []
    for i in np.arange(0, xdot_eta0_100_pe_df.shape[1]):
        x_pe_df = xdot_eta0_100_pe_df.iloc[:, i]
        for j in np.arange(0, ydot_eta0_100_pe_df.shape[1]):
            y_pe_df = ydot_eta0_100_pe_df.iloc[:, j]
            for k in np.arange(0, zdot_eta0_100_pe_df.shape[1]):
                z_pe_df = zdot_eta0_100_pe_df.iloc[:, k]
                sindy_combinations = np.vstack([x_pe_df, y_pe_df, z_pe_df]).T
                sindy_combinations_list.append(sindy_combinations)
    sindy_combinations_list = np.unique(sindy_combinations_list, axis=0)
    sindy_combinations_list_new = [
        element for element in sindy_combinations_list if np.count_nonzero(element) != 0
    ]
    aicc_min_list = []
    for i in np.arange(0, len(sindy_combinations_list_new)):
        current_sindy_model = sindy_combinations_list_new[i]
        x_y_sindy_abs_error = np.zeros(len(validation_theta_list))
        for j in np.arange(0, len(validation_theta_list)):
            x_hat_sindy = np.matmul(
                validation_theta_list[j], current_sindy_model[:, 0])
            y_hat_sindy = np.matmul(
                validation_theta_list[j], current_sindy_model[:, 1])
            z_hat_sindy = np.matmul(
                validation_theta_list[j], current_sindy_model[:, 2])
            x_int_sindy = (
                integrate.cumtrapz(y=x_hat_sindy, dx=dt, initial=0)
                + validation_init_condition_matrix_new[j][0]
            )
            y_int_sindy = (
                integrate.cumtrapz(y=y_hat_sindy, dx=dt, initial=0)
                + validation_init_condition_matrix_new[j][1]
            )
            z_int_sindy = (
                integrate.cumtrapz(y=z_hat_sindy, dx=dt, initial=0)
                + validation_init_condition_matrix_new[j][2]
            )
            length_data = len(validation_theta_list[j])
            nterms_sindy_abserror = current_sindy_model.shape[1]
            x_y_sindy_abs_error[j] = np.sum(
                np.sum(
                    (
                        np.absolute(x_validation_list[j][:, 0] - x_int_sindy)
                        + np.absolute(x_validation_list[j][:, 1] - y_int_sindy)
                        + np.absolute(x_validation_list[j][:, 2] - z_int_sindy)
                    )
                )
                / length_data
                / nterms_sindy_abserror
            )
        log_L = (
            (-1 * nterms_sindy_AIC)
            * np.log(
                np.sum(x_y_sindy_abs_error * x_y_sindy_abs_error) /
                nterms_sindy_AIC
            )
            / 2
        )
        num_nonzero_coeff = np.count_nonzero(current_sindy_model)
        if (num_nonzero_coeff > 0) & ((length_data - num_nonzero_coeff - 1) != 0):
            aic = -2 * log_L + 2 * num_nonzero_coeff
            aicc_min = aic + 2 * num_nonzero_coeff * (num_nonzero_coeff + 1) / (
                length_data - num_nonzero_coeff - 1
            )
        else:
            aicc_min = -2 * log_L
        aicc_min_list.append(aicc_min)
    try:
        sindy_final_model = sindy_combinations_list_new[np.argmin(
            aicc_min_list)]
    except:
        sindy_final_model = np.zeros(shape=(len(x_pe_df), 3))
    return sindy_final_model


"""Ensemble-SINDy"""


def perform_ensemble_stls(
    library_type,
    library_degree,
    training_data,
    sparsity_threshold,
    n_models,
    inclusion_proba,
    method,
    library_ens,
):
    # load training data
    x_dot_smoothed, x_smoothed, dt = training_data
    # perform STLS on original data
    # Create feature library
    if library_type == "fourier":
        polynomial_library = pl(degree=library_degree)
        fourier_library = fl()
        feature_library = ConcatLibrary([polynomial_library, fourier_library])
    else:
        feature_library = pl(degree=library_degree)
        # models = []
    models_coeff = []
    # thresh_model = []
    inclusion_probabilities_list = []
    if x_dot_smoothed.shape[1] == 1:
        feature_names = ['x']
    elif x_dot_smoothed.shape[1] == 2:
        feature_names = ['x', 'y']
    else:
        feature_names = ['x', 'y', 'z']
    model = ps.SINDy(optimizer=ps.STLSQ(threshold=sparsity_threshold, alpha=0),
                     feature_library=feature_library,
                     feature_names=feature_names)
    if library_ens == 'library':
        model.fit(
            x=x_smoothed,
            t=dt,
            x_dot=x_dot_smoothed,
            library_ensemble=True,
            n_models=n_models,
            quiet=True,
        )
        orig_coefficients = model.coefficients()
        n_models = len(model.coef_list)
        inclusion_probabilities = np.count_nonzero(
            model.coef_list, axis=0) / n_models
        # 2. Chop inclusion probabilities <= 50% (this is rather drastic for illustration)
        inclusion_probabilities[inclusion_probabilities <=
                                inclusion_proba] = 0.0
        # Find indices that are chopped for all three equations
        # since we pass the same library for all.
        chopped_inds = np.any(inclusion_probabilities != 0.0, axis=0)
        chopped_inds = np.ravel(np.where(~chopped_inds))
        # 3. Pass truncated library and then do normal ensembling
        feature_library = ps.PolynomialLibrary(degree=library_degree,
                                               library_ensemble=True,
                                               ensemble_indices=chopped_inds)
        ensemble_optimizer = ps.STLSQ(threshold=sparsity_threshold, alpha=0)
        model_new = ps.SINDy(feature_names=feature_names,
                             optimizer=ensemble_optimizer,
                             feature_library=feature_library
                             )
        model_new.fit(x=x_smoothed,
                      t=dt,
                      x_dot=x_dot_smoothed,
                      ensemble=True,
                      n_models=n_models,
                      quiet=True)
        if method == 'bagging':
            coefficients = np.mean(model_new.coef_list, axis=0)
        else:
            coefficients = model_new.coefficients()
        n_models = len(model_new.coef_list)
        inclusion_probabilities_new = np.count_nonzero(
            model_new.coef_list, axis=0) / n_models
        # Initialize arrays for coefficients and inclusion probabilities with zeros
        n_features = len(feature_library.get_feature_names())
        n_rows, n_cols = orig_coefficients.shape
        # These are arrays filled with zeros; we will populate them next
        filled_coefficients = np.zeros((n_rows, n_features))
        filled_inclusion_probabilities = np.zeros((n_rows, n_features))
        # Fill arrays based on the available data
        for row in range(n_rows):
            col_idx = 0  # Initialize column index for coefficients and inclusion_probabilities arrays
            for feature_idx in range(n_features):
                if feature_idx in chopped_inds:
                    continue  # Skip this feature, it's in the chopped list
                filled_coefficients[row,
                                    feature_idx] = coefficients[row, col_idx]
                filled_inclusion_probabilities[row,
                                               feature_idx] = inclusion_probabilities_new[row, col_idx]
                col_idx += 1  # Move to the next column for the next iteration
        coef_new = filled_coefficients
        # print(coef_new)
        models_coeff.append(coef_new)
        inclusion_probab_new = filled_inclusion_probabilities
        # print(inclusion_probab_new)
        inclusion_probabilities_list.append(inclusion_probab_new)
    else:
        model.fit(
            x=x_smoothed,
            t=dt,
            x_dot=x_dot_smoothed,
            ensemble=True,
            n_models=n_models,
            quiet=True,
        )
        n_models = len(model.coef_list)
        inclusion_probabilities = np.count_nonzero(
            model.coef_list, axis=0) / n_models
        inclusion_probabilities_list.append(inclusion_probabilities)
        if method == 'bagging':
            coefficients = np.mean(model.coef_list, axis=0)
            models_coeff.append(coefficients)
        else:
            coefficients = model.coefficients()
            models_coeff.append(coefficients)
    # thresh_model.append(coefficients)
    return (models_coeff, inclusion_probabilities_list)


def get_results_increasing_n_ensemble_sindy(args):
    """Apply sindy to each time series"""
    (
        index_value,
        orig_matrix,
        t_total_result,
        dynamical_system,
        time_step,
        snr,
        true_matrix,
        library_type,
        library_degree,
        sparsity_threshold,
        n_models,
        inclusion_proba,
        method,
        library_ens,
    ) = args
    sindy_model = []
    run_time_list = []
    sindy_probs = []
    for orig_init_matrix in enumerate(orig_matrix):
        start = time.time()
        training_data = training_data_generate(
            orig_init_matrix[1],
            t_total_result[index_value],
            dynamical_system,
            time_step,
            snr,
            true_matrix,
        )
        sindy = perform_ensemble_stls(
            library_type,
            library_degree,
            training_data,
            sparsity_threshold,
            n_models,
            inclusion_proba,
            method,
            library_ens,
        )
        end = time.time()
        run_time = end - start
        run_time_list.append(run_time)
        sindy_model.append(sindy[0])
        sindy_probs.append(sindy[1])
    return (sindy_model, sindy_probs, run_time_list)


def get_results_increasing_snr_ensemble_sindy(args):
    """Apply sindy to each time series"""
    (
        index_value,
        orig_matrix,
        t_total,
        dynamical_system,
        time_step,
        snr_volt,
        true_matrix,
        library_type,
        library_degree,
        sparsity_threshold,
        n_models,
        inclusion_proba,
        method,
        library_ens,
    ) = args
    sindy_model = []
    sindy_probs = []
    for orig_init_matrix in enumerate(orig_matrix):
        training_data = training_data_generate(
            orig_init_matrix[1],
            t_total,
            dynamical_system,
            time_step,
            snr_volt[index_value],
            true_matrix,
        )
        sindy = perform_ensemble_stls(
            library_type,
            library_degree,
            training_data,
            sparsity_threshold,
            n_models,
            inclusion_proba,
            method,
            library_ens,
        )
        sindy_model.append(sindy[0])
        sindy_probs.append(sindy[1])
    return (sindy_model, sindy_probs)
