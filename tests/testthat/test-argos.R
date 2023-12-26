# test-argos.R

# This script performs unit testing on functions within the ARGOS package using the testthat package.

# Load the necessary libraries for this script:
# - testthat: Provides functions for unit testing.
library(testthat)

# Load the ARGOS package, which contains the functions we're testing.
library(ARGOS)  # Ensure this matches the actual name of your package.

# Define common variables that will be used across multiple tests.
# These variables set up the initial conditions and parameters for the functions being tested.
x_t <- duffing_oscillator(n=1000, dt = 0.01, init_conditions = c(1, 0),
                          gamma_value = 0.1, kappa_value = 1, epsilon_value = 5, snr = 49)
dt <- 0.01
sg_poly_order <- 4
library_degree <- 5
library_type <- "poly"

# The first test checks that the 'build_design_matrix' function returns data with the correct structure.
test_that("build_design_matrix returns data with correct structure", {
  # Call the function with a standard set of parameters.
  result <- build_design_matrix(x_t = x_t,
                                dt = dt,
                                sg_poly_order = sg_poly_order,
                                library_degree = library_degree,
                                library_type = library_type)

  # Verify that the result is a list, as expected.
  expect_is(result, "list")

  # Check that the list contains specific components/names.
  # Replace these with the actual names expected based on your function's implementation.
  expect_named(result, c("sorted_theta", "monomial_orders", "xdot_filtered"))
})

# The second test ensures that 'build_design_matrix' function properly handles incorrect inputs.
test_that("build_design_matrix handles inputs correctly", {
  # If NULL is passed to the function, it should throw an error indicating "argument of length 0".
  expect_error(build_design_matrix(NULL), "argument of length 0")

  # If the 'library_type' is missing, the function should throw an error.
  # The actual error message should be specified here in place of "the condition has length > 1".
  expect_error(build_design_matrix(x_t = x_t,
                                   dt = dt,
                                   sg_poly_order = sg_poly_order,
                                   library_degree = library_degree), "the condition has length > 1")
})

# The third test verifies that the 'argos' function returns the correct type of output with the expected components.
test_that("argos returns a list with the correct components", {
  # Prepare the input by calling 'build_design_matrix' with a known set of parameters.
  design_matrix <- build_design_matrix(x_t = x_t,
                                       dt = dt,
                                       sg_poly_order = sg_poly_order,
                                       library_degree = library_degree,
                                       library_type = library_type)
  # Set parameters specific to the 'argos' function.
  state_var_deriv = 1  # Indicates the first equation/derivative to identify.
  alpha_level = 0.05  # The significance level for the statistical tests within 'argos'.
  num_samples = 10  # The number of samples/data points to use.
  sr_method = "lasso"  # Specifies the method for sparse regression.
  weights_method = NULL  # Indicates no specific method for weighting is used.
  ols_ps = TRUE  # Whether to include ordinary least squares post-selection.
  parallel = "no"  # Do not use parallel computation.
  ncpus = NULL  # Do not specify a number of CPU cores, as parallel computation is disabled.

  # Call the 'argos' function with the prepared inputs and parameters.
  perform_argos <- argos(design_matrix = design_matrix,
                         library_type = library_type,
                         state_var_deriv = state_var_deriv,
                         alpha_level = alpha_level,
                         num_samples = num_samples,
                         sr_method = sr_method,
                         weights_method = weights_method,
                         ols_ps = ols_ps,
                         parallel = parallel,
                         ncpus = ncpus)

  # Verify that the result is a list, as expected.
  expect_is(perform_argos, "list")

  # Check that the list contains specific components/names.
  # Replace these with the actual names expected based on your function's implementation.
  expect_named(perform_argos, c("point_estimates", "ci", "identified_model"))
})

# # This command initiates the execution of all tests included in this script, using the ARGOS package.
# test_check("ARGOS")
