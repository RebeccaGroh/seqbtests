context("bayesian_sequential_tests")
Sys.unsetenv("R_TESTS")

# check if seq_b_corr_t_test() returns a list with right information 
test_that("seq_b_corr_t_test returns a list" , {
  results <- seq_b_corr_t_test(df = test_benchmark_small, rho=0.1,
    problem = "problem_a", baseline = "algo_1", compare = "equal", 
    max_repls = 10)
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_type(results$baseline, "character")
  expect_output(str(results$method), "Bayesian correlated t-test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})

# check if seq_b_corr_t_test() returns error if names are not correct
test_that("seq_b_corr_t_test() returns error", {
  expect_error(seq_b_corr_t_test(df = test_benchmark_small, rho=0.1,
    problem = "problem_1", baseline = "algo_1", compare = "equal", 
    max_repls = 10))
})

# check that if theres early stopping, the left column needs to be >= 0.95
test_that("check if early stopping works for seq_b_corr_t_test", {
  results <- seq_b_corr_t_test(df = test_benchmark_small, rho=0.1,
    problem = "problem_a", baseline = "algo_1", compare = "equal", 
    max_repls = 10)
  for (i in nrow(results$data_frame)) {
    replications <- results$data_frame$repls[i]
    if (replications < 10) {
      expect_false(results$data_frame$probabilities[i] == "no decision")
    }
  }
})

# check if seq_b_sign_test() returns a list with right information 
test_that("seq_b_sign_test returns a list" , {
  results <- seq_b_sign_test(df = test_benchmark_small, 
    baseline = "algo_1", max_repls = 10)
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_type(results$baseline, "character")
  expect_output(str(results$method), "Bayesian Sign Test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})

# check if seq_b_sign_test() returns error if names are not correct
test_that("seq_b_sign_test() returns error", {
  expect_error(seq_b_sign_test(df = test_benchmark_small, 
    baseline = "algo_a", max_repls = 10))
})

# check that if theres early stopping, the left column needs to be >= 0.95
test_that("check if early stopping works for seq_b_corr_t_test", {
  results <- seq_b_sign_test(df = test_benchmark_small, 
    baseline = "algo_1", max_repls = 10)
  for (i in nrow(results$data_frame)) {
    replications <- results$data_frame$repls
    if (replications[i] < 10) {
      expect_false(results$data_frame$probabilities[i] == "no decision")
    }
  }
})


# check if seq_b_signed_rank_test() returns a list with right information 
test_that("seq_b_signed_rank_test returns a list" , {
  results <- seq_b_signed_rank_test(df = test_benchmark_small, 
    baseline = 'algo_1', max_repls = 10)
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_type(results$baseline, "character")
  expect_output(str(results$method), "Bayesian Signed-Rank Test", fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})

# check if seq_b_signed_rank_test() returns error if names are not correct
test_that("seq_b_signed_rank_test() returns error", {
  expect_error(seq_b_signed_rank_test(df = test_benchmark_small, 
    baseline = 'algo_a', max_repls = 10))
})

# check that if theres early stopping, the left column needs to be >= 0.95
test_that("check if early stopping works for seq_b_corr_t_test", {
  results <- seq_b_signed_rank_test(df = test_benchmark_small, 
    baseline = 'algo_1', max_repls = 10)
  for (i in nrow(results$data_frame)) {
    replications <- results$data_frame$repls
    if (replications[i] < 10) {
      expect_false(results$data_frame$probabilities[i] == "no decision")
    }
  }
})


# check if seq_b_hierarchical_test() returns a list with right information 
test_that("seq_b_hierarchical_test returns a list" , {
  results <- seq_b_hierarchical_test(df = test_benchmark_small,
    baseline = 'algo_1', algorithm = "algo_3", min_repls = 8, max_repls = 10)
  results
  expect_type(results, "list")
  expect_output(str(results), "List of 5")
  expect_type(results$baseline, "character")
  expect_output(str(results$method), "Hierarchical Bayesian correlated model", 
                fixed = TRUE)
  expect_type(results$measure, "character")
  expect_type(results$data_frame, "list")
})

# check if seq_b_hierarchical_test() returns error if names are not correct
test_that("seq_b_hierarchical_test() returns error", {
  expect_error(seq_b_hierarchical_test(df = test_benchmark_small,
    baseline = 'algo_a', max_repls = 10))
})

# check that if theres early stopping, the left column needs to be >= 0.95
test_that("check if early stopping works for seq_b_hierarchical_test", {
  results <- seq_b_hierarchical_test(df = test_benchmark_small,
    baseline = 'algo_1', max_repls = 10)
  for (i in nrow(results$data_frame)) {
    replications <- results$data_frame$repls
    if (replications[i] < 10) {
      expect_false(results$data_frame$probabilities[i] == "no decision")
    }
  }
})

