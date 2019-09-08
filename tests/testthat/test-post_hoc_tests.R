context("post_hoc_tests")
 
# check if nemenyi_test() returns a list with right information 
test_that("nemenyi_test returns a list" , {
  results <- nemenyi_test(df= test_benchmark_small)
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_output(str(results$method), "Nemenyi test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$matrix, "double")
})

# check if nemenyi_test() returns error for data with NA
test_that("nemenyi_test returns error", {
  data_tmp <- test_benchmark_small
  data_tmp$measure_col[1] <- NA 
  expect_error(nemenyi_test(data_tmp))
})

# check if nemenyi_test() returns error if names are not correct 
test_that("nemenyi_test() returns error", {
  expect_error(nemenyi_test(df= test_benchmark_small, measure = "measure"))
})

# check if friedman_post() returns a list with right information 
test_that("friedman_post returns a list" , {
  results <- friedman_post(df= test_benchmark_small)
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_output(str(results$method), "Friedman post hoc test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "double")
})

# check if friedman_post() returns error for data with NA
test_that("friedman_post returns error", {
  data_tmp <- test_benchmark_small
  data_tmp$measure_col[1] <- NA 
  expect_error(friedman_post(data_tmp))
})

# check if friedman_post() returns error if names are not correct 
test_that("friedman_post() returns error", {
  expect_error(friedman_post(df= test_benchmark_small, measure = "measure"))
})

