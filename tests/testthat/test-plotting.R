context("plotting")

# check  if plot_densities() creates a list
test_that("plot_densities creates a list", {
  expect_type(plot_densities(test_benchmark_small), "list")
})

# check  if plot_boxplot() creates a list
test_that("plot_boxplot creates a list", {
  expect_type(plot_boxplot(test_benchmark_small), "list")
})

# check  if plot_cd() creates a list
test_that("plot_cd creates a list", {
  expect_type(plot_cd(test_benchmark_small), "list")
})

# check  if plot_posterior() creates a list
test_that("plot_posterior creates a list", {
  results <- b_corr_t_test(df= test_benchmark_small, problemset = "problem_a", 
                           baseline = "algo_1", algorithm = "algo_2")
  expect_type(plot_posterior(results, method = "b_corr_t_test"), "list")
})


