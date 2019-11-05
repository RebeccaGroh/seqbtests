context("helpers")

# check if get_main_columns() works correctly for test_benchmark 
test_that("get_main_columns for test_benchmark", {
  columns = get_main_columns(test_benchmark)
  expect_output(str(columns), "chr [1:3]", fixed = TRUE)
})

# check if get_parameter_columns() works correctly for test_benchmark 
test_that("get_parameter_columns() for test_benchmark", {
  columns = get_parameter_columns(test_benchmark)
  expect_match(columns, "parameter_algorithm")
})


# check if get_measure_columns() works correctly for test_benchmark
test_that("get_measure_columns for test_benchmark", {
  columns = get_measure_columns(test_benchmark)
  expect_match(columns, "measure_col")
})


# check if get_main_columns_count() works correctly for test_benchmark 
test_that("get_main_columns_count for test_benchmark", {
  count = get_main_columns_count(test_benchmark)
  expect_equal(count, 3)
})


# check if get_parameter_columns_count() works correctly for test_benchmark
test_that("get_parameter_columns_count for test_benchmark", {
  count = get_parameter_columns_count(test_benchmark)
  expect_equal(count, 1)
})


# check if get_measure_columns_count() works correctly for test_benchmark
test_that("get_measure_columns_count for test_benchmark", {
  count = get_measure_columns_count(test_benchmark)
  expect_equal(count, 1)
})

# check if get_replications() works correctly for test_benchmark 
test_that("get_replications for test_benchmark" , {
  data <- get_replications(df = test_benchmark, i = 10)
  expect_type(data, "list")
})


# check if get_replications_count() works correctly on test samples 
test_that("get_replications tests for the same number of replications", {
  x <- rnorm(25, 1, 1)
  y <- rnorm(25, 1.5, 1)
  expect_true(get_replications_count(x, y))
})

# check if data_transformation() returns data frame, with only numeric columns 
test_that("data_transformation returns data frame with numeric columns", {
  data <- data_transformation(df = test_benchmark_small, 
                              algo = "algo_1", measure = "measure_col")
  expect_type(data, "list")
  for (i in ncol(data)) {
    expect_type(data[, i], "double")
  }
})

# check if get_results() returns a list 
test_that("get_results returns a list", {
  expect_type(get_results(baseline = "baseline", method = "method", 
                          measure = "measure", data = "data"), "list")
})

# check if get_results() returns a list 
test_that("get_results_htest returns a list", {
  expect_type(get_results_htest(baseline = "baseline", method = "method", 
                          measure = "measure", data = "data"), "list")
})

# check if get_data_frame() returns list
test_that("get_data_frame returns list", {
  results <- b_corr_t_test(df= test_benchmark_small, problem = "problem_a", 
                           baseline = "algo_1")
  expect_type(results$data_frame, "list")
})

# check if get_data_frame_seq() returns list
test_that("get_data_frame_seq returns list", {
  results <- seq_b_corr_t_test(df= test_benchmark_small, problem = "problem_a", 
                           baseline = "algo_1")
  expect_type(results$data_frame, "list")
})

# check if get_data_frame_htest() returns list
test_that("get_data_frame_htest returns list", {
  results <- corr_t_test(df= test_benchmark_small, problem = "problem_a", 
                         baseline = "algo_1", algorithm = "algo_2")
  expect_type(results$data_frame, "list")
})

# check if get_data_frame_htest_small() returns list
test_that("get_data_frame_htest_small returns list", {
  results <- friedman_test(test_benchmark) 
  expect_type(results$data_frame, "list")
})

# check if get_extras_scmamp() returns list 
test_that("get_extras_scmamp returns list", {
  results <- b_corr_t_test(df= test_benchmark_small, problem = "problem_a",
    baseline = "algo_1", compare = "equal")
  expect_type(results$extra, "list")
})

# check if get_rows() returns list 
test_that("get_rows returns list", {
  results <- seq_b_corr_t_test(df = test_benchmark_small, rho=0.1,
    problem = "problem_b", baseline = "algo_1", compare = "better", 
    max_repls = 10, min_repls = 5)
  expect_type(results$data_frame, "list")
})

# check if get_probabilities() returns character 
test_that("get_probabilities returns character", {
  results <- seq_b_corr_t_test(df = test_benchmark_small, rho=0.1,
    problem = "problem_b", baseline = "algo_1", compare = "better", 
    max_repls = 10, min_repls = 5)
  expect_type(results$data_frame$probabilities, "character")
  
})

