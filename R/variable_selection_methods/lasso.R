### Lasso
lasso_fn <- function(data, index, ols_ps = TRUE) {
  x <- as.matrix(data[index,-1])
  y <- as.matrix(data[index, 1, drop = FALSE])
  lasso_init <-
    cv.glmnet(x, y, alpha = 1, intercept = TRUE) #alpha=1, lasso
  lasso_init_lambda_min <- lasso_init$lambda.min
  lambda_init_grid <- lasso_init$lambda
  coef <- as.numeric(coef(
    lasso_init,
    lasso_init_lambda_min
  ))
  if (lasso_init$lambda.min ==
      lasso_init$lambda[length(lasso_init$lambda)]) {
    lower_bound_grid <- lasso_init$lambda.min / 10
    upper_bound_grid <- min(lasso_init$lambda[1], 1.1 * lasso_init$lambda.min)
    lambda_grid <-
      seq(upper_bound_grid, lower_bound_grid, length = 100)
    lasso_new <- cv.glmnet(x,
                           y,
                           alpha = 1,
                           lambda = lambda_grid,
                           intercept = TRUE)
    lasso_second_grid <- lasso_new$lambda
    coef <-
      as.numeric(coef(
        lasso_new,
        lasso_new$lambda.min
      ))
  }
  threshold_sequence <- 10 ^ (-8:1)
  lasso_final_coefficients_list <- lapply(
    threshold_sequence, function(x) {
      ifelse(
        abs(coef) <= x,
        0,
        coef
      )
    }
  )
  coef_logical_list <-
    sapply(lasso_final_coefficients_list, function(e) {
      !all(e == 0)
    })
  lasso_final_coefficients_list <-
    lasso_final_coefficients_list[which(coef_logical_list)]
  ols_list <- lapply(lasso_final_coefficients_list, function(e) {
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
  lasso_ols_coefficients_list <- lapply(seq_along(lasso_final_coefficients_list), function(e) {
    coef_nonzero <- lasso_final_coefficients_list[[e]] != 0
    vect_coef <- rep(0, ncol(data))
    vect_coef[coef_nonzero] <- ols_list[[e]]$coefficients
    return(vect_coef)
  })
  lasso_final_coefficients <- lasso_final_coefficients_list[[which.min(bic_min_list)]]
  lasso_ols_coefficients <- lasso_ols_coefficients_list[[which.min(bic_min_list)]]
  if(ols_ps){
    coef <- lasso_ols_coefficients
  } else {
    coef <- lasso_final_coefficients
  }
  return(coef)
}

lasso <- function(data, index, ols_ps = TRUE){
  tryCatch(lasso_fn(data, index, ols_ps),
           error = function(e) {rep(NA, ncol(data))}
  )
}
