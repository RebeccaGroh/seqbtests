context("check_structure")


# check if check_names() works correctly for test_benchmark_small 
test_that("check_names for test_benchmark_small", {
  expect_true(check_names(test_benchmark_small, problem = "problem_a", 
                          baseline = "algo_1", algorithm = "algo_2", 
                          measure = "measure_col"))
})

# check if check_column_names() works correctly for test_benchmark 
test_that("check_column_names for test_benchmark", {
  expect_true(check_column_names(df = test_benchmark_small))
})

# check if check_structure() works correctly for test_benchmark
test_that("check_structure for test_benchmark", {
  expect_true(check_structure(test_benchmark_small))
})

# check if check_structure() throws error without problem, algorithm 
# and replications
test_that("check_structure without problem/algorithm/replications", {
  tmp <- test_benchmark
  tmp$problem <- NULL 
  expect_error(check_structure(tmp))
  tmp <- test_benchmark
  tmp$algorithm <- NULL 
  expect_error(check_structure(tmp))
  tmp <- test_benchmark
  tmp$replications <- NULL 
  expect_error(check_structure(tmp))
})

# check if check_structure() throws error for unknown column
test_that("check_structure with unknown column", {
  tmp <- test_benchmark
  tmp$unknown <- rep(1, nrow(tmp))
  expect_error(check_structure(tmp))
})

# check if check_structure() throws error without measure column
test_that("check_structure without problem/algorithm/replications", {
  tmp <- test_benchmark
  tmp$measure_col <- NULL 
  expect_error(check_structure(tmp))
})


# check if check_structure() throws error when NAs exist
test_that("check_structure with NAs", {
  tmp <- test_benchmark
  tmp$algorithm[[1]] <- NA
  expect_error(check_structure(tmp))
  tmp <- test_benchmark
  tmp$parameter_algorithm[[1]] <- NA
  expect_error(check_structure(tmp))
  tmp <- test_benchmark
  tmp$measure_col[[1]] <- NA
  expect_error(check_structure(tmp))
  tmp <- test_benchmark
  tmp$measure_col[[1]] <- "error"
  expect_error(check_structure(tmp))
})

# check if paste_algo_pars() returns list 
test_that("check if paste_algo_pars returns list", {
  data <- paste_algo_pars(df = test_benchmark)
  expect_type(data, "list")
})




#-------------------------------------------------------------------------------








