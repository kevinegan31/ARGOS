get_can_names <- function(data, monomial_degree=5) {
  ncol_data <- ncol(data)
  if(ncol_data == 2){
    sta_name <- c('x','y')
    # Rearrange for Theta matrix: X0.0.1 = x, X0.1.0 = y, X1.0.0 = z
    out_sorted <- data.frame(data[, c(2, 1)])
  }
  if(ncol_data == 3){
    sta_name <- c('x', 'y', 'z')
    out_sorted <- data.frame(data[, c(3, 2, 1)])
  }
  # Polynomial Expansion
  expanded_theta <- polym(as.matrix(out_sorted),
                          degree = monomial_degree, raw = T)
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
    vec <- sta_name[seq_along(x)]
    x <- as.integer(x)
    y <- rep(vec, rev(x))
    paste(y, collapse = "")
  })
  # Add ones column to theta matrix
  sorted_theta <- data.frame(1, sorted_theta)
  # That lost the attributes, so put them back
  attr(sorted_theta, "degree") <-
    c(0, attr(expanded_theta, "degree")[ordered_results])
  sorted_theta <-
    sorted_theta[, order(attr(sorted_theta, "degree"), colnames(sorted_theta))]
  # That lost the attributes again, so put them back
  attr(sorted_theta, "degree") <-
    c(0, attr(expanded_theta, "degree")[ordered_results])
  return(colnames(sorted_theta))
}

