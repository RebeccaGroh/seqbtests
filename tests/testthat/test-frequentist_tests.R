context("frequentist_tests")

# check if corr_t_test() returns a list with right information 
test_that("corr_t_test returns a list" , {
  results <- corr_t_test(df= test_benchmark_small, problem = "problem_a", 
                         baseline = "algo_1", algorithm = "algo_2")
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_type(results$baseline, "character")
  expect_output(str(results$method), "Correlated t-test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})


# check if corr_t_test() returns error if names are not correct 
test_that("corr_t_test() returns error", {
  expect_error(corr_t_test(df= test_benchmark_small, problem = "problem_1",
                           baseline = "algo_1", algorithm = "algo_2"))
})

# check if friedman_test() returns list with right information 
test_that("friedman_test returns right information", {
  results <- results <- friedman_test(test_benchmark_small) 
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_output(str(results$method), "Friedman's rank sum test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})

# check if friedman_test() returns error if names are not correct 
test_that("friedman_test() returns error", {
  expect_error(friedman_test(df= test_benchmark_small, measure = "measure"))
})

# check if wilcoxon_signed_test() returns a list with right information 
test_that("wilcoxon_signed_test returns a list" , {
  results <- wilcoxon_signed_test(df = test_benchmark, baseline = "algo_1",
                                algorithm = "algo_2", problem = "problem_a") 
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_type(results$baseline, "character")
  expect_output(str(results$method), "Wilcoxon Signed-Rank test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})

# check if wilcoxon_signed_test() returns error if names are not correct 
test_that("wilcoxon_signed_test() returns error", {
  expect_error(wilcoxon_signed_test(df = test_benchmark, problem = "problem_1", 
                                    baseline = "algo_1", algorithm = "algo_2"))
})


