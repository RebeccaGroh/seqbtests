context("frequentist_tests")

# check if corr_t_test() returns a list with right information 
test_that("corr_t_test returns a list" , {
  results <- corr_t_test(df= test_benchmark_small, problemset = "problem_a", 
                         learner_a = "algo_1", learner_b = "algo_2")
  expect_type(results, "list")
  expect_output(str(results), "List of 2")
  expect_output(str(class(results$teststatistic)), "List of 3")
  expect_output(str(class(results$teststatistic)), "htest")
})

# check if corr_t_test() returns error if names are not correct 
test_that("corr_t_test() returns error", {
  expect_error(corr_t_test(df= test_benchmark_small, problemset = "problem_1",
                             learner_a = "algo_1", learner_b = "algo_2"))
})



# check if friedman_test() returns list with right information 
test_that("friedman_test returns right information", {
  results <- results <- friedman_test(test_benchmark) 
  expect_type(results, "list")
  expect_output(str(results), "List of 4")
  expect_output(str(results$method), "Friedman's rank sum test", fixed = TRUE)
  
})

# check if friedman_test() returns error if names are not correct 
test_that("friedman_test() returns error", {
  expect_error(friedman_test(df= test_benchmark_small, measure = "measure"))
})



# check if wilcoxon_signed_test() returns a list with right information 
test_that("wilcoxon_signed_test returns a list" , {
  results <- wilcoxon_signed_test(df = test_benchmark, problemset = "problem_a", 
                                  learner_a = "algo_1", learner_b = "algo_2")
  expect_type(results, "list")
  expect_output(str(results), "List of 4")
  expect_output(str(results$method), "Wilcoxon Signed-Rank test", fixed = TRUE)
})

# check if wilcoxon_signed_test() returns error if names are not correct 
test_that("wilcoxon_signed_test() returns error", {
  expect_error(wilcoxon_signed_test(df = test_benchmark, problemset = "problem_1", 
                                    learner_a = "algo_1", learner_b = "algo_2"))
})


