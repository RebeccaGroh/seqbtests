context("utils_plotting")

# test that plot_triangles() returns list within plot_posterior
test_that("plot_triangles returns list", {
  results <- b_sign_test(df= test_benchmark_small, 
    problem = "problem_a", baseline = "algo_1", algorithm = "algo_2")
  expect_type(plot_posterior(results, method = "b_sign_test"), "list")
})

