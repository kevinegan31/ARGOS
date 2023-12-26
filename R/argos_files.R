#' Lasso
#'
#' This function performs lasso regression using the cv.glmnet function,
#' then refits the model using ordinary least squares.
#'
#' @param data A data frame or matrix containing the predictors and response.
#'             The response must be in the first column.
#' @param index A numeric vector of indices indicating the rows of 'data' to
#'              use for the lasso regression.
#' @param ols_ps A logical scalar. If TRUE (default), the function returns the
#'               coefficients from the OLS fit. If FALSE, it returns the
#'               coefficients from the lasso fit.
#'
#' @return A numeric vector of coefficients. If 'ols_ps' is TRUE, these are the
#'         coefficients from the OLS fit. If 'ols_ps' is FALSE, these are the
#'         coefficients from the lasso fit. If an error occurs during the lasso
#'         or OLS fit, the function returns a vector of NAs.
#'
#' @export
#' @import Matrix
#' @import glmnet
#' @import tidyverse
#' @importFrom stats lm
#' @importFrom stats coef
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom stats BIC
#' @importFrom glmnet cv.glmnet
lasso <- function(data, index, ols_ps = TRUE) {
  tryCatch({
    x <- as.matrix(data[index,-1])
    y <- as.matrix(data[index, 1, drop = FALSE])
    lasso_init <-
      cv.glmnet(x, y, alpha = 1, intercept = TRUE) #alpha=1, lasso
    lasso_init_lambda_min <- lasso_init$lambda.min
    lambda_init_grid <- lasso_init$lambda
    coef <- as.numeric(coef(lasso_init,
                            lasso_init_lambda_min))
    if (lasso_init$lambda.min ==
        lasso_init$lambda[length(lasso_init$lambda)]) {
      lower_bound_grid <- lasso_init$lambda.min / 10
      upper_bound_grid <-
        min(lasso_init$lambda[1], 1.1 * lasso_init$lambda.min)
      lambda_grid <-
        seq(upper_bound_grid, lower_bound_grid, length = 100)
      lasso_new <- cv.glmnet(x,
                             y,
                             alpha = 1,
                             lambda = lambda_grid,
                             intercept = TRUE)
      lasso_second_grid <- lasso_new$lambda
      coef <-
        as.numeric(coef(lasso_new,
                        lasso_new$lambda.min))
    }
    threshold_sequence <- 10 ^ (-8:1)
    lasso_final_coefficients_list <-
      lapply(threshold_sequence, function(x) {
        ifelse(abs(coef) <= x,
               0,
               coef)
      })
    coef_logical_list <-
      sapply(lasso_final_coefficients_list, function(e) {
        !all(e == 0)
      })
    lasso_final_coefficients_list <-
      lasso_final_coefficients_list[which(coef_logical_list)]
    ols_list <- lapply(lasso_final_coefficients_list, function(e) {
      coef_nonzero <- e != 0
      if (sum(coef_nonzero) > 0) {
        if (coef_nonzero[1] & any(coef_nonzero[-1])) {
          selected_x <- x[, coef_nonzero[-1], drop = FALSE]
          ols <- lm(y ~ as.matrix(selected_x))
        } else if (coef_nonzero[1]) {
          ols <- lm(y ~ 1)
        } else {
          selected_x <- x[, coef_nonzero[-1], drop = FALSE]
          ols <- lm(y ~ 0 + as.matrix(selected_x))
        }
      }
    })
    bic_min_list <- lapply(ols_list, function(e) {
      BIC(e)
    })
    lasso_ols_coefficients_list <-
      lapply(seq_along(lasso_final_coefficients_list), function(e) {
        coef_nonzero <- lasso_final_coefficients_list[[e]] != 0
        vect_coef <- rep(0, ncol(data))
        vect_coef[coef_nonzero] <- ols_list[[e]]$coefficients
        return(vect_coef)
      })
    lasso_final_coefficients <-
      lasso_final_coefficients_list[[which.min(bic_min_list)]]
    lasso_ols_coefficients <-
      lasso_ols_coefficients_list[[which.min(bic_min_list)]]
    if (ols_ps) {
      coef <- lasso_ols_coefficients
    } else {
      coef <- lasso_final_coefficients
    }
    return(coef)
  }, error = function(e) {
    rep(NA, ncol(data))
  })
}
#' Adaptive Lasso
#'
#' This function performs adaptive lasso regression using the cv.glmnet function,
#' then refits the model using ordinary least squares.
#'
#' @param data A data frame or matrix containing the predictors and response.
#'             The response must be in the first column.
#' @param index A numeric vector of indices indicating the rows of 'data' to use
#'              for the adaptive lasso regression.
#' @param weights_method A character string specifying the method to calculate
#'                       the weights. Can be either "ols" or "ridge". Default
#'                       is "ols".
#' @param ols_ps A logical scalar. If TRUE (default), the function returns the
#'               coefficients from the OLS fit. If FALSE, it returns the
#'               coefficients from the lasso fit.
#'
#' @return A numeric vector of coefficients. If 'ols_ps' is TRUE, these are the
#'         coefficients from the OLS fit. If 'ols_ps' is FALSE, these are the
#'         coefficients from the lasso fit. If an error occurs during the lasso
#'         or OLS fit, the function returns a vector of NAs.
#'
#' @export
#' @import Matrix
#' @import glmnet
#' @importFrom stats lm
#' @importFrom stats lsfit
#' @importFrom stats coef
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom stats BIC
#' @importFrom glmnet cv.glmnet
alasso <-
  function(data,
           index,
           weights_method = c("ols", "ridge"),
           ols_ps = TRUE) {
    tryCatch({
      x <- as.matrix(data[index,-1])
      y <- as.matrix(data[index, 1, drop = FALSE])
      if (weights_method == "ols") {
        ols <-
          lsfit(
            x = x,
            y = y,
            intercept = FALSE,
            tolerance = 1e-20
          )[[1]]
        ols_coef <- ols
        ols_coef[is.na(ols_coef)] <- 0
        weight <- ols_coef
      }
      if (weights_method == "ridge") {
        ridge_init <- cv.glmnet(x, y, alpha = 0, intercept = TRUE)
        ridge_coef <- as.numeric(coef(ridge_init,
                                      s = ridge_init$lambda.min))
        if (ridge_init$lambda.min ==
            ridge_init$lambda[length(ridge_init$lambda)]) {
          lower_bound_grid <- ridge_init$lambda.min / 10
          upper_bound_grid <-
            min(ridge_init$lambda[1], 1.1 * ridge_init$lambda.min)
          lambda_grid <-
            seq(upper_bound_grid, lower_bound_grid, length = 100)
          ridge_new <-
            cv.glmnet(x,
                      y,
                      alpha = 0,
                      lambda = lambda_grid,
                      intercept = TRUE)
          ridge_coef <- as.numeric(coef(ridge_new,
                                        ridge_new$lambda.min))
        }
        ridge_coef[is.na(ridge_coef)] <- 0
        weight <- ridge_coef[-1]
      }
      # alpha=1, lasso
      alasso_init <- cv.glmnet(
        x,
        y,
        alpha = 1,
        penalty.factor = 1 / abs(weight),
        intercept = TRUE
      )
      alasso_init_lambda_min <- alasso_init$lambda.min
      alasso_init_lambda_grid <- alasso_init$lambda
      coef <- as.numeric(coef(alasso_init, alasso_init$lambda.min))
      if (alasso_init$lambda.min ==
          alasso_init$lambda[length(alasso_init$lambda)]) {
        lower_bound_grid <- alasso_init$lambda.min / 10
        upper_bound_grid <-
          min(alasso_init$lambda[1], 1.1 * alasso_init$lambda.min)
        lambda_grid <-
          seq(upper_bound_grid, lower_bound_grid, length = 100)
        alasso_new <- cv.glmnet(
          x,
          y,
          alpha = 1,
          penalty.factor = 1 / abs(weight),
          lambda = lambda_grid,
          intercept = TRUE
        )
        alasso_second_lambda_grid <- alasso_new$lambda
        coef <- as.numeric(coef(alasso_new, alasso_new$lambda.min))
      }
      threshold_sequence <- 10 ^ (-8:1)
      alasso_final_coefficients_list <- lapply(threshold_sequence, function(x) {
        ifelse(abs(coef) <= x,
               0,
               coef)
      })
      coef_logical_list <-
        sapply(alasso_final_coefficients_list, function(e) {
          !all(e == 0)
        })
      alasso_final_coefficients_list <-
        alasso_final_coefficients_list[which(coef_logical_list)]
      ols_list <- lapply(alasso_final_coefficients_list, function(e) {
        coef_nonzero <- e != 0
        if (sum(coef_nonzero) > 0) {
          if (coef_nonzero[1] & any(coef_nonzero[-1])) {
            selected_x <- x[, coef_nonzero[-1], drop = FALSE]
            ols <- lm(y ~ as.matrix(selected_x))
          } else if (coef_nonzero[1]) {
            ols <- lm(y ~ 1)
          } else {
            selected_x <- x[, coef_nonzero[-1], drop = FALSE]
            ols <- lm(y ~ 0 + as.matrix(selected_x))
          }
        }
      })
      bic_min_list <- lapply(ols_list, function(e) {
        BIC(e)
      })
      alasso_ols_coefficients_list <-
        lapply(seq_along(alasso_final_coefficients_list), function(e) {
          coef_nonzero <- alasso_final_coefficients_list[[e]] != 0
          vect_coef <- rep(0, ncol(data))
          vect_coef[coef_nonzero] <- ols_list[[e]]$coefficients
          return(vect_coef)
        })
      alasso_final_coefficients <-
        alasso_final_coefficients_list[[which.min(bic_min_list)]]
      alasso_ols_coefficients <-
        alasso_ols_coefficients_list[[which.min(bic_min_list)]]
      if (ols_ps) {
        coef <- alasso_ols_coefficients
      } else {
        coef <- alasso_final_coefficients
      }
      return(coef)
    }, error = function(e) {
      rep(NA, ncol(data))
    })
  }
#' Optimal Savitzky-Golay Filter Parameters Finder
#'
#' This function finds the optimal parameters for the Savitzky-Golay filter
#' by evaluating combinations of polynomial orders and window lengths.
#'
#' @param x_t A numeric vector or one-column matrix. The data to be smoothed.
#' @param dt A numeric scalar. The time-step interval of the data. Default is 1.
#' @param polyorder A numeric scalar. The order of the polynomial to be used in
#'                  the Savitzky-Golay filter. If not specified, 4 will be used
#'                  by default.
#'
#' @return A list with three elements:
#'   - sg_combinations: a matrix where each row represents a combination of
#'                      polynomial order and window length tried.
#'   - sg_order_wl: a vector of length 2 with the optimal polynomial order and
#'                  window length.
#'   - f_dist: a data frame with the mean squared error of the differences
#'             between the original data and the smoothed data for each
#'             combination.
#'
#' @export
#' @import signal
#' @import tidyverse
#' @importFrom tidyr expand_grid
sg_optimal_combination <- function(x_t, dt = 1, polyorder) {
  ### Create Combinations
  wl_max <- round((nrow(as.matrix(x_t)) * 0.05), 0)
  wl_max <- ifelse(wl_max %% 2 == 0, wl_max + 1, wl_max)

  ### Polynomial Order
  polyorder <- if(missing(polyorder)) 4 else polyorder
  ### If the Window length calculation is less than 11
  ### we will just try the two minimum values.
  if (wl_max < 13) {
    ### Combinations
    sg_combinations <- cbind(4, 13)
  } else {
    ### Combinations
    if (wl_max > 101) {
      window_length <- seq(5, 101, by = 2)
    } else {
      if (wl_max %% 2 == 0) {
        wl_max <- wl_max + 1
        window_length <- seq(5, wl_max, by = 2)
      } else {
        window_length <- seq(5, wl_max, by = 2)
      }
    }
    sg_combinations <- expand_grid(polyorder, window_length) %>%
      subset(window_length > polyorder + 7 - polyorder %% 2) %>%
      as.matrix()
    if (nrow(sg_combinations) == 1) {
      sg_combinations <- cbind(4, 13)
    }
  }
  ### Determine MSE for Combinations
  mse_xt <- sapply(seq_len(nrow(sg_combinations)), function(i) {
    x_t_smoothed <- x_t %>% sgolayfilt(p = sg_combinations[i, 1],
                                       n = sg_combinations[i, 2],
                                       m = 0,
                                       ts = dt)
    Metrics::mse(x_t, x_t_smoothed)
  })

  mse_df <- data.frame(mse_xt = unlist(mse_xt))

  sg_best_combination <- which.min(mse_df$mse_xt)
  sg_order_wl <- cbind(sg_combinations[sg_best_combination, 1],
                       sg_combinations[sg_best_combination, 2])
  return(
    list(
      sg_combinations = sg_combinations,
      sg_order_wl = sg_order_wl,
      mse_df = mse_df
    )
  )
}
#' Build Design Matrix
#'
#' This function first smooths the data and approximates the
#' derivative before building the design matrix to include monomial and fourier
#' terms.
#'
#' @param x_t Matrix of observations.
#' @param dt Time step (default is 1).
#' @param sg_poly_order Polynomial order for Savitzky-Golay Filter.
#' @param library_degree Degree of polynomial library (default is 5).
#' @param library_type Type of library to use. Can be one of "poly",
#'                     "four", or "poly_four".
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{sorted_theta} - A matrix with sorted polynomial/trigonometric
#'         terms.
#'   \item \code{monomial_orders} - A vector indicating the order of each
#'         polynomial term.
#'   \item \code{xdot_filtered} - A matrix with derivative terms
#'         (dependent variable).
#' }
#' @export
#' @examples
#' # Build a design matrix using the Duffing Oscillator as the state-space.
#' # Output provides matrix, and derivative matrix monomial orders
#' # (needed for running `argos`).
#' x_t <- duffing_oscillator(n=5000, dt = 0.01,
#'                           init_conditions = c(1, 0),
#'                           gamma_value = 0.1, kappa_value = 1,
#'                           epsilon_value = 5, snr = 49)
#' duffing_design_matrix <-
#'          build_design_matrix(x_t, dt = 0.01, sg_poly_order = 4,
#'                              library_degree = 5, library_type = "poly")
#' head(duffing_design_matrix$sorted_theta)
#' @importFrom signal sgolayfilt
#' @importFrom magrittr %>%
#' @importFrom stats polym
build_design_matrix <- function(x_t,
                                dt = 1,
                                sg_poly_order = 4,
                                library_degree = 5,
                                library_type = c("poly", "four", "poly_four")) {
  monomial_degree <- library_degree
  dt <- dt
  # Filter x_t
  num_columns <- ncol(x_t)
  x_filtered <- list()
  xdot_filtered <- list()
  # Filter x_t
  for (i in 1:num_columns) {
    if (x_t[1, i]) {
      sg_combinations <- sg_optimal_combination(x_t[, i],
                                                dt,
                                                polyorder = sg_poly_order)[[2]]
      x_filtered[[i]] <- sgolayfilt(
        x_t[, i],
        p = sg_combinations[1, 1],
        n = sg_combinations[1, 2],
        m = 0,
        ts = dt
      )
      xdot_filtered[[i]] <- sgolayfilt(
        x_t[, i],
        p = sg_combinations[1, 1],
        n = sg_combinations[1, 2],
        m = 1,
        ts = dt
      )
    }
  }
  # Combine filtered data and derivatives
  x_t <- do.call(cbind, x_filtered)
  sg_dx <- do.call(cbind, xdot_filtered)
  # Get the number of columns in the matrix
  num_columns_sg_dx <- ncol(sg_dx)
  # Create column names based on the pattern
  colnames(sg_dx) <- paste0("xdot", 1:num_columns_sg_dx)
  ### Sort state variables for expansion
  ### x_t needs to be in reverse order because of how poly function expands
  ### We do this here so that we can use it for the for loop to determine
  ### optimal SG parameters
  out_sorted <- x_t %>%
    data.frame() %>%
    rev()
  if (library_type == "poly" | library_type == "poly_four") {
    # Polynomial Expansion
    expanded_theta <- polym(as.matrix(out_sorted),
                            degree = monomial_degree, raw = TRUE)
    # Order by degree using as.numeric_version numeric_version allows to
    # convert names of variables and expand without limit
    ordered_results <- order(attr(expanded_theta, "degree"),
                             as.numeric_version(colnames(expanded_theta)))
    # Sort Theta Matrix
    sorted_theta <- expanded_theta[, ordered_results]
    sorted_theta <- data.frame(sorted_theta)
    # Change Variable Names
    s <- strsplit(substring(colnames(sorted_theta), 2), "\\.")
    colnames(sorted_theta) <- sapply(s, function(powers) {
      terms <- mapply(function(power, index) {
        if (power == "0") {
          return(NULL)
        } else if (power == "1") {
          return(paste0("x", index))
        } else {
          return(paste0("x", index, "^", power))
        }
      }, powers, rev(seq_along(powers)), SIMPLIFY = FALSE)

      # Filter out any NULL values from the terms list
      terms <- Filter(Negate(is.null), terms)

      # Sort terms alphabetically
      sorted_terms <- sort(unlist(terms))

      # Collapse the sorted terms into one string
      paste(sorted_terms, collapse = "")
    })
    # That lost the attributes, so put them back
    attr(sorted_theta, "degree") <-
      attr(expanded_theta, "degree")[ordered_results]
    monomial_orders <-
      attr(expanded_theta, 'degree')[ordered_results]
  }
  if (library_type == "four" | library_type == "poly_four") {
    if (ncol(x_t) == 1) {
      trig_functions <- cbind(sin(x_t[, 1]), cos(x_t[, 1]))
      attr(trig_functions, "degree") <- c(1, 1)
    } else if (ncol(x_t) == 2) {
      trig_functions <- cbind(sin(x_t[, 1]), cos(x_t[, 1]),
                              sin(x_t[, 2]), cos(x_t[, 2]))
      attr(trig_functions, "degree") <- c(1, 1, 1, 1)
    } else {
      trig_functions <- cbind(sin(x_t[, 1]), cos(x_t[, 1]),
                              sin(x_t[, 2]), cos(x_t[, 2]),
                              sin(x_t[, 3]), cos(x_t[, 3]))
      attr(trig_functions, "degree") <- c(1, 1, 1, 1, 1, 1)
    }
    num_columns <- ncol(trig_functions)
    column_names <- character(num_columns)

    for (i in seq(1, num_columns, by = 2)) {
      # sin for odd columns
      column_names[i] <- paste("sin_x", ceiling(i/2), sep = "")

      # If there's an even column left
      if (i + 1 <= num_columns) {
        column_names[i + 1] <- paste("cos_x", ceiling(i/2), sep = "")
      }
    }
    colnames(trig_functions) <- column_names
    if (library_type == "four") {
      sorted_theta <- trig_functions
      attr(sorted_theta, "degree") <-
        attr(sorted_theta, "degree")[c(attr(trig_functions, "degree"))]
      # That lost the attributes again, so put them back
      monomial_orders <-
        attr(trig_functions, "degree")
    } else {
      sorted_theta <- cbind(trig_functions, sorted_theta)
      attr(sorted_theta, "degree") <-
        attr(expanded_theta, "degree")[c(attr(trig_functions, "degree"), ordered_results)]
      # That lost the attributes again, so put them back
      monomial_orders <-
        attr(expanded_theta, "degree")[c(attr(trig_functions, "degree"), ordered_results)]
    }

  }
  return(list(sorted_theta = cbind(sorted_theta),
              monomial_orders = monomial_orders,
              xdot_filtered = sg_dx))
}
#' Automatic Regression for Governing Equations (ARGOS)
#'
#' This function performs sparse regression on a data set to identify the
#' governing equations of the system. It takes a list of data from
#' `build_design_matrix` then applies the Lasso or Adaptive Lasso for variable
#' selection.
#'
#' @param design_matrix A list containing data frame, vector of predictor
#'                      variable orders for 'theta', and derivative matrix.
#' @param library_type A character vector (default: c("poly", "four",
#'                      "poly_four")) specifying the type of library being used.
#' @param state_var_deriv An integer. The index of the state variable for which
#'                        the derivative is calculated. Default is 1.
#' @param alpha_level A numeric scalar. The level of significance for
#'                    confidence intervals. Default is 0.05.
#' @param num_samples An integer. The number of bootstrap samples. Default is
#'                    2000.
#' @param sr_method A character string. The sparse regression method to be used,
#'                  either "lasso" or "alasso". Default is "lasso".
#' @param weights_method A string or NULL. The method for calculating weights in
#'                       the Adaptive Lasso. If NULL, ridge regression pilot
#'                       estimates are used. Default is NULL.
#' @param ols_ps A logical. If TRUE, post-selection OLS is performed after the
#'               Lasso or Adaptive Lasso. Default is TRUE.
#' @param parallel A character string. The type of parallel computation to be
#'                 used, either "no", "multicore" or "snow". Default is "no".
#' @param ncpus An integer or NULL. The number of cores to be used in parallel
#'              computation. If NULL, the function will try to detect the
#'              number of cores. Default is NULL.
#'
#' @return A list with three elements:
#'   - point_estimates: a vector of point estimates for the coefficients.
#'   - ci: a matrix where each column represents the lower and upper bounds of
#'         the confidence interval for a coefficient.
#'   - identified_model: a matrix of coefficients of the identified model.
#'
#' @export
#' @examples
#' # Identify the x1 equation of the Duffing Oscillator with ARGOS.
#' # Output provides point estimates, confidence intervals, and identified model.
#' x_t <- duffing_oscillator(n=1000, dt = 0.01,
#'                           init_conditions = c(1, 0),
#'                           gamma_value = 0.1, kappa_value = 1,
#'                           epsilon_value = 5, snr = 49)
#' duffing_design_matrix <-
#'        build_design_matrix(x_t, dt = 0.01, sg_poly_order = 4,
#'                            library_degree = 5, library_type = "poly")
#' design_matrix <- duffing_design_matrix
#' state_var_deriv = 1 # Denotes first equation/derivative to be identified
#' alpha_level = 0.05
#' num_samples = 10
#' sr_method = "lasso"
#' weights_method = NULL
#' ols_ps = TRUE
#' parallel = "no"
#' ncpus = NULL
#' library_type <- "poly"
#' perform_argos <- argos(design_matrix = design_matrix,
#'                        library_type = library_type,
#'                        state_var_deriv = state_var_deriv,
#'                        alpha_level = alpha_level,
#'                        num_samples = num_samples,
#'                        sr_method = "lasso",
#'                        weights_method = NULL,
#'                        ols_ps = TRUE,
#'                        parallel = "no",
#'                        ncpus = NULL)
#' perform_argos$point_estimates
#' perform_argos$ci
#' perform_argos$identified_model
#' @import boot
#' @import tidyverse
#' @importFrom stats polym
#' @importFrom magrittr `%>%`
argos <- function(design_matrix,
                  library_type = c("poly", "four", "poly_four"),
                  state_var_deriv = 1,
                  alpha_level = 0.05,
                  num_samples = 2000,
                  sr_method = c("lasso", "alasso"),
                  weights_method = NULL,
                  ols_ps = TRUE,
                  parallel = c("no", "multicore", "snow"),
                  ncpus = NULL) {
  # Unpack design matrix
  sorted_theta <- design_matrix$sorted_theta
  monomial_orders <- design_matrix$monomial_orders
  xdot <- design_matrix$xdot
  parallel <- match.arg(parallel)  # add this line
  sr_method <- match.arg(sr_method)  # add this line
  # Check if parallel processing is requested
  if (parallel != "no") {
    # Check if ncpus is NULL
    if (is.null(ncpus)) {
      # Detect number of cores and assign it to ncpus
      ncpus <- parallel::detectCores()
    }
  }
  # Create derivative and combine with theta matrix with SG Golay
  num_deriv_columns <- ncol(xdot)
  derivative_data <- list()
  for (i in 1:num_deriv_columns) {
    deriv_col <- xdot[, i]
    dot_df <- data.frame(cbind(deriv_col, sorted_theta))
    derivative_data[[i]] <- dot_df
  }
  # Access the desired data frame using the derivative variable
  data <- derivative_data[[state_var_deriv]]
  # Perform initial sparse regression to determine polynomial order of design matrix
  sr_method <- if(missing(sr_method)) "lasso" else sr_method
  weights_method <- if(missing(weights_method)) "ridge" else weights_method
  if (sr_method == "alasso") {
    initial_estimate <-
      alasso(data, weights_method = weights_method, ols_ps = ols_ps)
  } else {
    initial_estimate <- lasso(data, ols_ps = ols_ps)
  }
  # max nonzero value from sparse regression
  init_nz_max <- max(which(initial_estimate != 0))
  # Determine new theta order based on max nonzero value.
  # Include all monomials in max value
  new_theta_order <- sum(monomial_orders <=
                           monomial_orders[init_nz_max])
  if (library_type == "four") {
    post_lasso_matrix <- data
  } else {
    # Rerun Bootstrap with Truncated Matrix
    if (is.na(new_theta_order) |
        new_theta_order == length(monomial_orders)) {
      post_lasso_matrix <- data
    } else {
      post_lasso_matrix <- data[-1][, 1:(new_theta_order)]
      post_lasso_matrix <-
        cbind.data.frame(data[1], post_lasso_matrix)
    }
  }
  # Create list to compile necessary information for bootstrap.
  # Add updated matrix
  if (sr_method == "alasso") {
    boot_info <-
      c(
        list(data = post_lasso_matrix, R = num_samples),
        statistic = match.fun("alasso"),
        weights_method = weights_method,
        ols_ps = ols_ps,
        parallel = parallel,
        ncpus = ncpus
      )
  } else {
    boot_info <-
      c(
        list(data = post_lasso_matrix, R = num_samples),
        statistic = match.fun("lasso"),
        ols_ps = ols_ps,
        parallel = parallel,
        ncpus = ncpus
      )
  }

  # boot Function on Original Dataframe
  boot_s <- do.call(boot, boot_info)
  # Matrix of coefficients from bootstrap samples
  boot_t0 <- boot_s$t0 # point estimates
  boot_t <- boot_s$t # sample estimates
  ### In case of string/character, change to numeric
  boot_t <-
    matrix(as.numeric(boot_t),
           nrow = num_samples,
           ncol = ncol(boot_t))
  # subset any NAs
  num_nas_boot <- sum(apply(boot_t, 1, function(x)
    any(is.na(x))))
  boot_t <-
    subset(boot_t, apply(boot_t, 1, function(x)
      any(!is.na(x))))
  # ordered polynomial degree of variables alpha
  # typically equal to 0.05, 0.01, or 0.10
  b <- nrow(boot_t)
  q_normal <- alpha_level
  # Lower bound
  q1_normal <- (b * q_normal) / 2
  # Upper bound
  q2_normal <- b - q1_normal + 1
  if (round(q1_normal) <= 0) {
    q1_normal <- 1
  }
  if (q2_normal > b) {
    q2_normal <- b
  }
  # Sort and determine value of lower and upper bound
  ci <- apply(boot_t, 2, function(u) {
    sort(u)[c(round(q1_normal, 0), round(q2_normal, 0))]
  })
  ci[is.na(ci)] <- 0
  count_zero <- apply(boot_t, 2, function(x) {
    length(which(x == 0))
  })
  percent_zero <- apply(boot_t, 2, function(x) {
    length(which(x == 0)) / length(x)
  })
  df_columns <- c("(Intercept)", colnames(post_lasso_matrix)[-1])
  df_columns <- gsub("\\.", "^", df_columns)
  identified_model <- matrix(data = NA, nrow = length(boot_t0))
  rownames(identified_model) <- c("(Intercept)", df_columns[-1])
  ### Check if confidence intervals contain variable and do not cross zero
  for (i in seq_along(identified_model)) {
    if (ci[1, i] <= boot_t0[i] & ci[2, i] >= boot_t0[i] &
        ((ci[1, i] <= 0 && ci[2, i] >= 0)) == FALSE) {
      identified_model[i,] <- boot_t0[i]
    } else {
      identified_model[i,] <- 0
    }
  }
  colnames(ci) <- df_columns
  return(
    list(
      point_estimates = boot_t0,
      ci = ci,
      identified_model = identified_model
    )
  )
}
#' Cubic 2D System
#'
#' Simulates a two-dimensional damped oscillator with cubic dynamics and optional
#' noise.
#'
#' @param n Number of time points (rounded to the nearest integer).
#' @param init_conditions Initial conditions as a numeric vector of length 2.
#' @param dt Time step between observations.
#' @param snr Signal-to-noise ratio (in dB). Use Inf for no noise.
#'
#' @return A numeric matrix representing the system's state over time. Each row
#'         corresponds to a time point, and each column represents a variable.
#'
#' @examples
#' # Simulate a 2D cubic system with 100 time points and no noise
#' data <- cubic2d_system(n = 100, init_conditions = c(1, 2), dt = 0.01, snr = Inf)
#'
#' @details
#' This function simulates a two-dimensional damped oscillator with cubic dynamics.
#' It uses the specified time step and initial conditions to compute the system's
#' state over time. If a non-Infinite SNR is provided, Gaussian noise is added to
#' the system.
#'
#' @import deSolve
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @export
cubic2d_system <- function(n, init_conditions, dt, snr = Inf) {
  n <- round(n, 0)
  dt <- dt
  # n = number of time points rounded to nearest integer
  # snr = added noise to system (dB)
  # times: n - 1 to round off total n given to start at t_init = 0
  times <- seq(0, ((n) - 1) * dt, by = dt)
  init_conditions <- init_conditions
  matrix_a <- matrix(c(-0.1, -2,
                       2, -0.1), 2, 2)
  cubic2d <- function(t, init_conditions, parameters) {
    with(as.list(c(init_conditions, parameters)), {
      dx <- matrix_a[1, 1] * init_conditions[1] ** 3 + matrix_a[1, 2] * init_conditions[2] ** 3
      dy <- matrix_a[2, 1] * init_conditions[1] ** 3 + matrix_a[2, 2] * init_conditions[2] ** 3
      list(c(dx, dy))
    })
  }
  out <- ode(y = init_conditions, times = times,
             func = cubic2d, parms = matrix_a,
             atol = 1.49012e-8, rtol = 1.49012e-8)[, -1]
  # Add Noise
  if (!is.infinite(snr)) {
    length <- nrow(out) * ncol(out)
    # Convert to snr voltage (dB)
    snr_volt <- 10 ^ -(snr / 20)
    noise_matrix <- snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
    out <- out + noise_matrix
  }
  # Return x_t
  return(x_t = out)
}
#' Linear 2D System
#'
#' Simulates a two-dimensional damped oscillator with linear dynamics and optional
#' noise.
#'
#' @param n Number of time points (rounded to the nearest integer).
#' @param init_conditions Initial conditions as a numeric vector of length 2.
#' @param dt Time step between observations.
#' @param snr Signal-to-noise ratio (in dB). Use Inf for no noise.
#'
#' @return A numeric matrix representing the system's state over time. Each row
#'         corresponds to a time point, and each column represents a variable.
#'
#' @examples
#' # Simulate a 2D linear system with 100 time points and no noise
#' data <- linear2d_system(n = 100, init_conditions = c(-1, 1), dt = 0.01, snr = Inf)
#'
#' @details
#' This function simulates a two-dimensional damped oscillator with linear dynamics.
#' It uses the specified time step and initial conditions to compute the system's
#' state over time. If a non-Infinite SNR is provided, Gaussian noise is added to
#' the system.
#'
#' @import deSolve
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @export
linear2d_system <- function(n, init_conditions, dt, snr = Inf) {
  n <- round(n, 0)
  dt <- dt
  snr <- snr
  # n = number of time points rounded to nearest integer
  # times: n - 1 to round off total n given that we start at 0
  times <- seq(0, ((n) - 1) * dt, by = dt)
  init_conditions <- init_conditions
  matrix_a <- matrix(c(-0.1, -2,
                       2, -0.1), 2, 2)
  linear2d <- function(t, init_conditions, parameters) {
    with(as.list(c(init_conditions, parameters)), {
      dx <- matrix_a[1, 1] * init_conditions[1] + matrix_a[1, 2] * init_conditions[2]
      dy <- matrix_a[2, 1] * init_conditions[1] + matrix_a[2, 2] * init_conditions[2]
      list(c(dx, dy))
    })
  }
  out <- ode(y = init_conditions, times = times,
             func = linear2d, parms = matrix_a,
             atol = 1.49012e-8, rtol = 1.49012e-8)[, -1]
  # Add Noise
  if (!is.infinite(snr)) {
    length <- nrow(out) * ncol(out)
    # Convert to snr voltage (dB)
    snr_volt <- 10 ^ -(snr / 20)
    noise_matrix <- snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
    out <- out + noise_matrix
  }
  # Return x_t
  return(x_t = out)
}
#' Linear 3D System
#'
#' Simulates a three-dimensional linear dynamical system with optional noise.
#'
#' @param n Number of time points (rounded to the nearest integer).
#' @param init_conditions Initial conditions as a numeric vector of length 3.
#' @param dt Time step between observations.
#' @param snr Signal-to-noise ratio (in dB). Use Inf for no noise.
#'
#' @return A numeric matrix representing the system's state over time. Each row
#'         corresponds to a time point, and each column represents a variable.
#'
#' @examples
#' # Simulate a 3D linear system with 100 time points and no noise
#' data <- linear3d_system(n = 100, init_conditions = c(1, 2, 3), dt = 0.01, snr = Inf)
#'
#' @details
#' This function simulates a three-dimensional linear dynamical system.
#' It uses the specified time step and initial conditions to compute the system's
#' state over time. If a non-Infinite SNR is provided, Gaussian noise is added to
#' the system.
#'
#' @import deSolve
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @export
linear3d_system <- function(n, init_conditions, dt, snr = Inf)  {
  n <- round(n, 0)
  dt <- dt
  # n = number of time points rounded to nearest integer
  # snr = added noise to system (dB)
  # times: n - 1 to round off total n given to start at t_init = 0
  times <- seq(0, ((n) - 1) * dt, by = dt)
  matrix_a <- matrix(c(-0.1, -2, 0,
                       2, -0.1, 0,
                       0, 0, -0.3), 3, 3)
  init_conditions <- init_conditions
  linear3d <- function(t, init_conditions, parameters) {
    with(as.list(c(init_conditions, parameters)), {
      dx <- matrix_a[1, 1] * init_conditions[1] + matrix_a[1, 2] * init_conditions[2]
      dy <- matrix_a[2, 1] * init_conditions[1] + matrix_a[2, 2] * init_conditions[2]
      dz <- matrix_a[3, 3] * init_conditions[3]
      list(c(dx, dy, dz))
    })
  }
  out <- ode(y = init_conditions, times = times,
             func = linear3d, parms = matrix_a,
             atol = 1.49012e-8, rtol = 1.49012e-8)[, -1]
  # Add Noise
  if (!is.infinite(snr)) {
    length <- nrow(out) * ncol(out)
    # Convert to snr voltage (dB)
    snr_volt <- 10 ^ -(snr / 20)
    noise_matrix <- snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
    out <- out + noise_matrix
  }
  # Return x_t
  return(x_t = out)
}
#' Duffing Oscillator
#'
#' Simulates the Duffing oscillator with optional noise.
#'
#' @param n Number of time points (rounded to the nearest integer).
#' @param dt Time step between observations.
#' @param init_conditions Initial conditions as a numeric vector of length 2.
#' @param gamma_value Value of gamma parameter.
#' @param kappa_value Value of kappa parameter.
#' @param epsilon_value Value of epsilon parameter.
#' @param snr Signal-to-noise ratio (in dB). Use Inf for no noise.
#'
#' @return A numeric matrix representing the system's state over time. Each row
#'         corresponds to a time point, and each column represents a variable.
#'
#' @examples
#' # Simulate a Duffing oscillator with 100 time points and no noise
#' data <- duffing_oscillator(
#'   n = 100,
#'   dt = 0.01,
#'   init_conditions = c(2, 6),
#'   gamma_value = 0.1,
#'   kappa_value = 1,
#'   epsilon_value = 5,
#'   snr = Inf
#' )
#'
#' @details
#' This function simulates a Duffing oscillator with the specified parameters.
#' It uses the specified time step and initial conditions to compute the system's
#' state over time. If a non-Infinite SNR is provided, Gaussian noise is added to
#' the system.
#'
#' @import deSolve
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @export
duffing_oscillator <-
  function(n,
           dt,
           init_conditions,
           gamma_value,
           kappa_value,
           epsilon_value,
           snr = Inf) {
    n <- round(n, 0)
    dt <- dt
    # n = number of time points rounded to nearest integer
    # snr = added noise to system (dB)
    # times: n - 1 to round off total n given to start at t_init = 0
    init_conditions <- init_conditions
    times <- seq(0, ((n) - 1) * dt, by = dt)
    duff_parameters <- c(gamma_value, kappa_value, epsilon_value)
    duff_osc <- function(t,
                         init_conditions,
                         duff_parameters) {
      with(as.list(c(init_conditions,
                     duff_parameters)), {
                       dx <- init_conditions[2]
                       dy <-
                         (-duff_parameters[1] * init_conditions[2]) - (duff_parameters[2] * init_conditions[1]) - (duff_parameters[3] * (init_conditions[1] ^ 3))
                       list(c(dx, dy))
                     })
    }
    # Oscillator
    out <- ode(
      y = init_conditions,
      func = duff_osc,
      times = times,
      parms = duff_parameters,
      atol = 1.49012e-8,
      rtol = 1.49012e-8
    )[, -1]
    # Add Noise
    if (!is.infinite(snr)) {
      length <- nrow(out) * ncol(out)
      # Convert to snr voltage (dB)
      snr_volt <- 10 ^ -(snr / 20)
      noise_matrix <-
        snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
      out <- out + noise_matrix
    }
    # Return x_t
    return(x_t = out)
  }
#' Van der Pol Oscillator
#'
#' Simulates the Van der Pol oscillator with optional noise.
#'
#' @param n Number of time points (rounded to the nearest integer).
#' @param dt Time step between observations.
#' @param init_conditions Initial conditions as a numeric vector of length 2.
#' @param mu Parameter controlling the nonlinear damping level of the system.
#' @param snr Signal-to-noise ratio (in dB). Use Inf for no noise.
#'
#' @return A numeric matrix representing the system's state over time. Each row
#'         corresponds to a time point, and each column represents a variable.
#'
#' @examples
#' # Simulate a Van der Pol oscillator with 100 time points and no noise
#' data <- vdp_oscillator(
#'   n = 100,
#'   dt = 0.01,
#'   init_conditions = c(-1, 1),
#'   mu = 1.2,
#'   snr = Inf
#' )
#'
#' @details
#' This function simulates a Van der Pol oscillator with the specified parameters.
#' It uses the specified time step and initial conditions to compute the system's
#' state over time. If a non-Infinite SNR is provided, Gaussian noise is added to
#' the system.
#'
#' @import deSolve
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @export
vdp_oscillator <- function(n, dt, init_conditions, mu, snr = Inf) {
  n <- round(n, 0)
  dt <- dt
  mu <- mu
  # mu = "negative" resistance of triode passing a small current
  # n = number of time points rounded to nearest integer
  # snr = added noise to system (dB)
  # times: n - 1 to round off total n given to start at t_init = 0
  init_conditions <- init_conditions
  times <- seq(0, ((n) - 1) * dt, by = dt)
  mu <- mu
  vdpol <- function(t, init_conditions, mu) {
    with(as.list(c(init_conditions, mu)), {
      dx <- init_conditions[2]
      dy <-
        mu * (1 - ((init_conditions[1]) ^ 2)) * init_conditions[2] - init_conditions[1]
      list(c(dx, dy))
    })
  }
  out <- ode(
    y = init_conditions,
    func = vdpol,
    times = times,
    parms = mu,
    atol = 1.49012e-8,
    rtol = 1.49012e-8
  )[,-1]
  # Add Noise
  if (!is.infinite(snr)) {
    length <- nrow(out) * ncol(out)
    # Convert to snr voltage (dB)
    snr_volt <- 10 ^ -(snr / 20)
    noise_matrix <-
      snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
    out <- out + noise_matrix
  }
  return(x_t = out)
}
#' Lotka-Volterra System
#'
#' Simulates the Lotka-Volterra predator-prey system with optional noise.
#'
#' @param n Number of time points (rounded to the nearest integer).
#' @param dt Time step between observations.
#' @param init_conditions Initial conditions as a numeric vector of length 2.
#' @param snr Signal-to-noise ratio (in dB). Use Inf for no noise.
#'
#' @return A numeric matrix representing the system's state over time. Each row
#'         corresponds to a time point, and each column represents a variable.
#'
#' @examples
#' # Simulate a Lotka-Volterra system with 100 time points and no noise
#' data <- lotka_volterra(
#'   n = 100,
#'   dt = 0.01,
#'   init_conditions = c(2, 1),
#'   snr = Inf
#' )
#'
#' @details
#' This function simulates the Lotka-Volterra predator-prey system with the
#' specified parameters. It uses the specified time step and initial conditions
#' to compute the system's state over time. If a non-Infinite SNR is provided,
#' Gaussian noise is added to the system.
#'
#' @import deSolve
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @export
lotka_volterra <- function(n, init_conditions, dt, snr = Inf) {
  n <- round(n, 0)
  dt <- dt
  # n = number of time points rounded to nearest integer
  # snr = added noise to system (dB)
  # times: n - 1 to round off total n given to start at t_init = 0
  parameters <- c(c0 = 1, c1 = -1, c2 = -1, c3 = 1)
  init_conditions <- init_conditions
  lv <- function(t, init_conditions, parameters) {
    with(as.list(c(init_conditions, parameters)), {
      dx <- (parameters[1] * init_conditions[1]) + (parameters[2] * (init_conditions[1] * init_conditions[2]))
      dy <- (parameters[3] * init_conditions[2]) + (parameters[4] * (init_conditions[1] * init_conditions[2]))
      list(c(dx, dy))
    })
  }
  times <- seq(0, ((n) - 1) * dt, by = dt)
  out <- ode(y = init_conditions, times = times,
             func = lv, parms = parameters,
             atol = 1.49012e-8, rtol = 1.49012e-8)[, -1]
  # Add Noise
  if (!is.infinite(snr)) {
    length <- nrow(out) * ncol(out)
    # Convert to snr voltage (dB)
    snr_volt <- 10 ^ -(snr / 20)
    noise_matrix <- snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
    out <- out + noise_matrix
  }
  # Return x_t
  return(x_t = out)
}
#' Lorenz Chaotic System
#'
#' Simulates the Lorenz chaotic system with optional noise.
#'
#' @param n Number of time points (rounded to the nearest integer).
#' @param dt Time step between observations.
#' @param init_conditions Initial conditions as a numeric vector of length 3 (X, Y, Z).
#' @param snr Signal-to-noise ratio (in dB). Use Inf for no noise.
#'
#' @return A numeric matrix representing the system's state over time. Each row
#'         corresponds to a time point, and each column represents a variable (X, Y, Z).
#'
#' @examples
#' # Simulate the Lorenz system with 1000 time points and no noise
#' data <- lorenz_system(
#'   n = 1000,
#'   dt = 0.01,
#'   init_conditions = c(-8, 7, 27),
#'   snr = Inf
#' )
#'
#' @details
#' This function simulates the Lorenz chaotic system with the specified
#' parameters. It uses the specified time step and initial conditions to compute
#' the system's state over time. If a non-Infinite SNR is provided, Gaussian noise
#' is added to the system.
#'
#' @import deSolve
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @export
lorenz_system <- function(n, init_conditions, dt, snr = Inf) {
  n <- round(n, 0)
  dt <- dt
  # n = number of time points rounded to nearest integer
  # snr = added noise to system (dB)
  # times: n - 1 to round off total n given to start at t_init = 0
  # Lorenz Parameters: sigma, rho, beta
  parameters <- c(s = 10, r = 28, b = 8 / 3)
  # init_conditions <- c(X = -8, Y = 7, Z = 27) # Original Initial Conditions
  init_conditions <- init_conditions
  lorenz <- function(t, init_conditions, parameters) {
    with(as.list(c(init_conditions, parameters)), {
      dx <- parameters[1] * (init_conditions[2] - init_conditions[1])
      dy <- init_conditions[1] * (parameters[2] - init_conditions[3]) - init_conditions[2]
      dz <- init_conditions[1] * init_conditions[2] - parameters[3] * init_conditions[3]
      list(c(dx, dy, dz))
    })
  }
  times <- seq(0, ((n) - 1) * dt, by = dt)
  out <- ode(y = init_conditions, times = times,
             func = lorenz, parms = parameters,
             atol = 1.49012e-8, rtol = 1.49012e-8)[, -1]
  # Add Noise
  if (!is.infinite(snr)) {
    length <- nrow(out) * ncol(out)
    # Convert to snr voltage (dB)
    snr_volt <- 10 ^ -(snr / 20)
    noise_matrix <- snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
    out <- out + noise_matrix
  }
  # Return x_t
  return(x_t = out)
}
#' Rossler Chaotic System
#'
#' Simulates the Rossler chaotic system with optional noise.
#'
#' @param n Number of time points (rounded to the nearest integer).
#' @param dt Time step between observations.
#' @param init_conditions Initial conditions as a numeric vector of length 3 (X, Y, Z).
#' @param a Rossler parameter 1
#' @param b Rossler parameter 2
#' @param c Rossler parameter 3
#' @param snr Signal-to-noise ratio (in dB). Use Inf for no noise.
#'
#' @return A numeric matrix representing the system's state over time. Each row
#'         corresponds to a time point, and each column represents a variable (X, Y, Z).
#'
#' @examples
#' # Simulate the Rossler system with 1000 time points and no noise
#' data <- rossler_system(
#'   n = 1000,
#'   dt = 0.01,
#'   init_conditions = c(0, 2, 0),
#'   a = 0.2, b = 0.2, c = 5.7,
#'   snr = Inf
#' )
#'
#' @details
#' This function simulates the Rossler chaotic system with the specified
#' parameters. It uses the specified time step and initial conditions to compute
#' the system's state over time. If a non-Infinite SNR is provided, Gaussian noise
#' is added to the system.
#'
#' @import deSolve
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @export
rossler_system <-
  function(n,
           dt,
           init_conditions,
           a, b, c,
           snr = Inf) {
    n <- round(n, 0)
    dt <- dt
    # n = number of time points rounded to nearest integer
    # snr = added noise to system (dB)
    # times: n - 1 to round off total n given to start at t_init = 0
    init_conditions <- init_conditions
    times <- seq(0, ((n) - 1) * dt, by = dt)
    rossler_parameters <- c(a, b, c)
    rossler <- function(t,
                        init_conditions,
                        rossler_parameters) {
      with(as.list(c(init_conditions,
                     rossler_parameters)), {
                       dx <- -init_conditions[2] - init_conditions[3]
                       dy <-
                         init_conditions[1] + (rossler_parameters[1] * init_conditions[2])
                       dz <-
                         rossler_parameters[2] + (init_conditions[3] * (init_conditions[1] - rossler_parameters[3]))
                       list(c(dx, dy, dz))
                     })
    }
    out <- ode(
      y = init_conditions,
      func = rossler,
      times = times,
      parms = rossler_parameters,
      atol = 1.49012e-8,
      rtol = 1.49012e-8
    )[,-1]
    # Add Noise
    if (!is.infinite(snr)) {
      length <- nrow(out) * ncol(out)
      # Convert to snr voltage (dB)
      snr_volt <- 10 ^ -(snr / 20)
      noise_matrix <-
        snr_volt * matrix(rnorm(length, mean = 0, sd = sd(out)), nrow(out))
      out <- out + noise_matrix
    }
    # Return x_t
    return(x_t = out)
  }
