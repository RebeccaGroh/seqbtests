context("bayesian_tests")
Sys.unsetenv("R_TESTS")

# check if b_corr_t_test() returns a list with right information 
test_that("b_corr_t_test returns a list" , {
  results <- b_corr_t_test(df= test_benchmark_small, problem = "problem_a", 
    baseline = "algo_1", algorithm = "algo_2")
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_type(results$baseline, "character")
  expect_output(str(results$method), "Bayesian correlated t-test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})

# check if b_corr_t_test() returns error if names are not correct 
test_that("b_corr_t_test() returns error", {
  expect_error(b_corr_t_test(df= test_benchmark_small, problem = "problem_1",
    baseline = "algo_1", algorithm = "algo_2"))
})

# check if b_corr_t_test() returns the same results as the test from scmamp
test_that("b_corr_t_test returns same results as origin", {
  sample_a <- 
    test_benchmark_small[test_benchmark_small[["problem"]] == "problem_a"
    & test_benchmark_small[["algorithm"]] == "algo_1", "measure_col"]
  sample_b <- 
    test_benchmark_small[test_benchmark_small[["problem"]] == "problem_a"
    & test_benchmark_small[["algorithm"]] == "algo_2", "measure_col"]  
  result_scmamp <- scmamp::bCorrelatedTtest(x = sample_a, y = sample_b, 
    rho = 0.1, rope=c(-0.01, 0.01))
  result <- b_corr_t_test(df= test_benchmark_small, problem = "problem_a",
    baseline = "algo_1", algorithm = "algo_2")
  expect_equal(as.numeric(result_scmamp$posterior.probabilities[1]), 
    as.numeric(result$data_frame[, "left"]))
  expect_equal(as.numeric(result_scmamp$posterior.probabilities[2]), 
    as.numeric(result$data_frame[, "rope"]))
  expect_equal(as.numeric(result_scmamp$posterior.probabilities[3]), 
    as.numeric(result$data_frame[, "right"]))
})

# check if b_sign_test() returns a list with right information 
test_that("b_sign_test returns a list" , {
  results <- b_sign_test(df= test_benchmark_small, problem = "problem_a",
    baseline = "algo_1", algorithm = "algo_2")
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_type(results$baseline, "character")
  expect_output(str(results$method), "Bayesian Sign Test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})

# check if b_sign_test() returns error if names are not correct 
test_that("b_sign_test() returns error", {
  expect_error(b_sign_test(df= test_benchmark_small,
    baseline = "algo_1", algorithm = "algo_b"))
})

# check if b_signed_rank_test() returns a list with right information 
test_that("b_signed_rank_test returns a list" , {
  results <- b_signed_rank_test(df= test_benchmark_small, problem = "problem_a",
    baseline = "algo_1", algorithm = "algo_2")
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_type(results$baseline, "character")
  expect_output(str(results$method), "Bayesian Signed-Rank Test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})

# check if b_signed_rank_test() returns error if names are not correct 
test_that("b_signed_rank_test() returns error", {
  expect_error(b_signed_rank_test(df= test_benchmark_small, 
    baseline = "algo_a", algorithm = "algo_2"))
})


# check if b_hierarchical_test() returns a list with right information 
test_that("b_hierarchical_test returns a list" , {
  results <- b_hierarchical_test(df= test_benchmark_small,
    baseline = "algo_1", algorithm = "algo_2", parallel = FALSE)
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_type(results$baseline, "character")
  expect_output(str(results$method), "Hierarchical Bayesian correlated model", 
                fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})

# check if b_hierarchical_test() returns error if names are not correct 
test_that("b_hierarchical_test() returns error", {
  expect_error(b_hierarchical_test(df= test_benchmark_small,
    baseline = "algo_a", algorithm = "algo_2"))
})
