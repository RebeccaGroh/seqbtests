context("post_hoc_tests")
 

# check if nemenyi_test() returns a list with right information 
test_that("nemenyi_test returns a list" , {
  results <- nemenyi_test(df= test_benchmark_small)
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_output(str(results$method), "Nemenyi test", fixed = TRUE)
  expect_matrix(results$diff_matrix)
  expect_matrix(results$significance)
})

# check if nemenyi_test() returns error if names are not correct 
test_that("nemenyi_test() returns error", {
  expect_error(nemenyi_test(df= test_benchmark_small, measure = "measure"))
})


# check if friedman_post() returns a list with right information 
test_that("friedman_post returns a list" , {
  results <- friedman_post(df= test_benchmark_small)
  expect_type(results, "list")
  expect_output(str(results), "List of 3")
  expect_output(str(results$method), "Friedman post hoc test", fixed = TRUE)
  expect_matrix(results$matrix)
})

# check if friedman_post() returns error if names are not correct 
test_that("friedman_post() returns error", {
  expect_error(friedman_post(df= test_benchmark_small, measure = "measure"))
})

