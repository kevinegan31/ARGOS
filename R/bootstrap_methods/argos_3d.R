### Automatic Regression for Governing Equations (ARGOS) -- 3D
boot_lasso <- function(x_t,
                       monomial_degree = 5,
                       dt = 1,
                       alpha = 0.05,
                       num_samples = 2000,
                       sg_combinations_x1,
                       sg_combinations_x2,
                       sg_combinations_x3,
                       sr_method = c("lasso", "alasso"),
                       bs_method,
                       derivative = c("xdot", "ydot", "zdot")) {
  monomial_degree <- monomial_degree
  dt <- dt
  # Filter x_t
  x_1 <-
    sgolayfilt(
      x_t[, 1],
      p = sg_combinations_x1[, 1],
      n = sg_combinations_x1[, 2],
      m = 0,
      ts = dt
    )
  x_2 <-
    sgolayfilt(
      x_t[, 2],
      p = sg_combinations_x2[, 1],
      n = sg_combinations_x2[, 2],
      m = 0,
      ts = dt
    )
  x_3 <-
    sgolayfilt(
      x_t[, 3],
      p = sg_combinations_x3[, 1],
      n = sg_combinations_x3[, 2],
      m = 0,
      ts = dt
    )
  x_t <- cbind(x_1, x_2, x_3)
  # Approximate sg_dx
  xdot_1 <-
    sgolayfilt(
      x_t[, 1],
      p = sg_combinations_x1[, 1],
      n = sg_combinations_x1[, 2],
      m = 1,
      ts = dt
    )
  xdot_2 <-
    sgolayfilt(
      x_t[, 2],
      p = sg_combinations_x2[, 1],
      n = sg_combinations_x2[, 2],
      m = 1,
      ts = dt
    )
  xdot_3 <-
    sgolayfilt(
      x_t[, 3],
      p = sg_combinations_x3[, 1],
      n = sg_combinations_x3[, 2],
      m = 1,
      ts = dt
    )
  sg_dx <- cbind(xdot_1, xdot_2, xdot_3)
  ### Sort state variables for expansion
  ### x_t needs to be in reverse order because of how poly function expands
  ### We do this here so that we can use it for the for loop to determine
  ### optimal SG parameters
  out_sorted <- x_t %>%
    data.frame() %>%
    rev()
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
  colnames(sorted_theta) <- sapply(s, function(x) {
    vec <- c("x", "y", "z")[seq_along(x)]
    x <- as.integer(x)
    y <- rep(vec, rev(x))
    paste(y, collapse = "")
  })
  # That lost the attributes, so put them back
  attr(sorted_theta, "degree") <-
    attr(expanded_theta, "degree")[ordered_results]
  sorted_theta <-
    sorted_theta[, order(attr(sorted_theta, "degree"), colnames(sorted_theta))]
  # That lost the attributes again, so put them back
  attr(sorted_theta, "degree") <-
    attr(expanded_theta, "degree")[ordered_results]
  ### Create derivative and combine with theta matrix with SG Golay
  if (derivative == "xdot") {
    xdot <- sg_dx[, 1]
    xdot_df <-
      data.frame(cbind(xdot, sorted_theta))
  }
  if (derivative == "ydot") {
    ydot <- sg_dx[, 2]
    ydot_df <-
      data.frame(cbind(ydot, sorted_theta))
  }
  if (derivative == "zdot") {
    zdot <- sg_dx[, 3]
    zdot_df <-
      data.frame(cbind(zdot, sorted_theta))
  }
  data <- switch(derivative,
                 xdot = xdot_df,
                 ydot = ydot_df,
                 zdot = zdot_df)
  monomial_orders <- attr(expanded_theta, 'degree')[ordered_results]
  # ordered polynomial degree of variables alpha
  # typically equal to 0.05, 0.01, or 0.10
  # Perform initial sparse regression to determine polynomial order of design matrix
  if (sr_method == "alasso") {
    initial_test_info <-
      c(list(data = data), bs_method[length(bs_method) - 1], bs_method[length(bs_method)])
  } else {
    initial_test_info <-
      c(list(data = data), bs_method[length(bs_method)])
  }
  initial_estimate <- do.call(bs_method[[1]], initial_test_info)
  # max nonzero value from alasso
  init_nz_max <- max(which(initial_estimate != 0))
  # init_nz_max <- max(which(initial_estimate_test != 0))
  # Determine new theta order based on max nonzero value.
  # Include all monomials in max value
  new_theta_order <- sum(monomial_orders <=
                           monomial_orders[init_nz_max])
  # Rerun Bootstrap with Truncated Matrix
  if (is.na(new_theta_order) |
      new_theta_order == length(monomial_orders)) {
    post_lasso_matrix <- data
  } else {
    post_lasso_matrix <- data[-1][, 1:(new_theta_order)]
    post_lasso_matrix <-
      cbind.data.frame(data[1], post_lasso_matrix)
  }
  # Create list to compile necessary information for bootstrap.
  # Add updated matrix
  boot_info <-
    c(list(data = post_lasso_matrix, R = num_samples), bs_method)
  # Run boot on function using
  boot_s <- do.call(boot, boot_info)
  # Matrix of coefficients from bootstrap samples
  boot_t0 <- boot_s$t0
  boot_t <- boot_s$t
  ### In case of string/character, change to numeric
  boot_t <-
    matrix(as.numeric(boot_t),
           nrow = num_samples,
           ncol = ncol(boot_t))
  num_nas_boot <- sum(apply(boot_t, 1, function(x)
    any(is.na(x))))
  boot_t <-
    subset(boot_t, apply(boot_t, 1, function(x)
      any(!is.na(x))))
  # b <- num_samples - num_nas_boot
  b <- nrow(boot_t)
  q_normal <- alpha
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
  bound_percentile_normal <- apply(boot_t, 2, function(u) {
    sort(u)[c(round(q1_normal, 0), round(q2_normal, 0))]
  })
  bound_percentile_normal[is.na(bound_percentile_normal)] <- 0
  count_zero <- apply(boot_t, 2, function(x) {
    length(which(x == 0))
  })
  percent_zero <- apply(boot_t, 2, function(x) {
    length(which(x == 0)) / length(x)
  })
  df_columns <- colnames(post_lasso_matrix)
  return(
    list(
      point_estimates = boot_t0,
      ci = bound_percentile_normal,
      num_bs_zero = count_zero,
      percent_bs_zero = percent_zero,
      theta_colnames = df_columns
    )
  )
}
