### Adaptive Lasso
alasso_fn <- function(data, index, weights_method = c("ols", "ridge"),
                   ols_ps = TRUE) {
  x <- as.matrix(data[index, -1])
  y <- as.matrix(data[index, 1, drop = FALSE])
  if (weights_method == "ols") {
    ols <- lsfit(x = x, y = y, intercept = FALSE, tolerance = 1e-20)[[1]]
    # ols_coef <- coef(ols)
    ols_coef <- ols
    ols_coef[is.na(ols_coef)] <- 0
    weight <- ols_coef
  }
  if (weights_method == "ridge") {
    ridge_init <- cv.glmnet(x, y, alpha = 0, intercept = TRUE)
    ridge_coef <- as.numeric(coef(
      ridge_init,
      s = ridge_init$lambda.min
    ))
    if (ridge_init$lambda.min ==
        ridge_init$lambda[length(ridge_init$lambda)]) {
      lower_bound_grid <- ridge_init$lambda.min / 10
      upper_bound_grid <- min(ridge_init$lambda[1], 1.1 * ridge_init$lambda.min)
      lambda_grid <- seq(upper_bound_grid, lower_bound_grid, length = 100)
      ridge_new <-
        cv.glmnet(x,
                  y,
                  alpha = 0,
                  lambda = lambda_grid,
                  intercept = TRUE)
      ridge_coef <- as.numeric(coef(
        ridge_new,
        ridge_new$lambda.min
      ))
    }
    ridge_coef[is.na(ridge_coef)] <- 0
    weight <- ridge_coef[-1]
  }
  # alpha=1, lasso
  alasso_init <- cv.glmnet(x, y, alpha = 1,
                           penalty.factor = 1 / abs(weight),
                           intercept = TRUE)
  alasso_init_lambda_min <- alasso_init$lambda.min
  alasso_init_lambda_grid <- alasso_init$lambda
  coef <- as.numeric(coef(alasso_init, alasso_init$lambda.min))
  if (alasso_init$lambda.min ==
      alasso_init$lambda[length(alasso_init$lambda)]) {
    lower_bound_grid <- alasso_init$lambda.min / 10
    upper_bound_grid <- min(alasso_init$lambda[1], 1.1 * alasso_init$lambda.min)
    lambda_grid <- seq(upper_bound_grid, lower_bound_grid, length = 100)
    alasso_new <- cv.glmnet(x, y, alpha = 1,
                            penalty.factor = 1 / abs(weight),
                            lambda = lambda_grid, intercept = TRUE)
    alasso_second_lambda_grid <- alasso_new$lambda
    coef <- as.numeric(coef(alasso_new, alasso_new$lambda.min))
  }
  threshold_sequence <- 10 ^ (-8:1)
  alasso_final_coefficients_list <- lapply(
    threshold_sequence, function(x) {
      ifelse(
        abs(coef) <= x,
        0,
        coef
      )
    }
  )
  coef_logical_list <-
    sapply(alasso_final_coefficients_list, function(e) {
      !all(e == 0)
    })
  alasso_final_coefficients_list <-
    alasso_final_coefficients_list[which(coef_logical_list)]
  ols_list <- lapply(alasso_final_coefficients_list, function(e) {
    coef_nonzero <- e != 0
    if (sum(coef_nonzero) > 0) {
      if(coef_nonzero[1] & any(coef_nonzero[-1])) {
        selected_x <- x[, coef_nonzero[-1], drop = FALSE]
        ols <- lm(y ~ as.matrix(selected_x))
      } else if (coef_nonzero[1]) {
        ols <- lm(y ~ 1)
      } else {
        selected_x <- x[, coef_nonzero[-1], drop = FALSE]
        ols <- lm(y ~ 0 + as.matrix(selected_x))
      }
    }
  }
  )
  bic_min_list <- lapply(ols_list, function(e) {
    BIC(e)
  })
  alasso_ols_coefficients_list <- lapply(seq_along(alasso_final_coefficients_list), function(e) {
    coef_nonzero <- alasso_final_coefficients_list[[e]] != 0
    vect_coef <- rep(0, ncol(data))
    vect_coef[coef_nonzero] <- ols_list[[e]]$coefficients
    return(vect_coef)
  })
  alasso_final_coefficients <- alasso_final_coefficients_list[[which.min(bic_min_list)]]
  alasso_ols_coefficients <- alasso_ols_coefficients_list[[which.min(bic_min_list)]]
  if(ols_ps){
    coef <- alasso_ols_coefficients
  } else {
    coef <- alasso_final_coefficients
  }
  return(coef)
}

alasso <- function(data, index, weights_method = c("ols", "ridge"),
                   ols_ps = TRUE){
  tryCatch(alasso_fn(data, index, weights_method,
                     ols_ps),
           error = function(e) {rep(NA, ncol(data))}
  )
}
