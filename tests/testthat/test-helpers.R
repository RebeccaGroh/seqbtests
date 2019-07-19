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
  expect_match(columns, "measure_mmce")
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


# check if paste_algo_pars() works correctly for test_benchmark 
test_that("paste_algo_pars for test_benchmark", {
  data <- paste_algo_pars(algorithm = test_benchmark$algorithm, 
                          parameter_algorithm = test_benchmark$parameter_algorithm)
  expect_output(str(data), "chr [1:2500]", fixed = TRUE)
})


# check if data_transformation() returns data frame 
test_that("data_transformation returns data frame", {
  data <- data_transformation(df= test_benchmark_small, algo = "algo_1", 
                              measure = "measure_col")
  expect_data_frame(data)
})
