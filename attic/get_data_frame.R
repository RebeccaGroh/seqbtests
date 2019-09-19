

b_corr_t_test <- function(df, problem, baseline, algorithm = NULL, 
                          measure = NULL, compare = NULL, rho = 0.1, 
                          rope = c(-0.01, 0.01), prob = 0.95) {
  part_result <- data.frame()
  result_bind <- data.frame()
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problem, baseline, 
                                     algorithm = NULL, measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  # define samples
  x <- df[df[["problem"]] == problem 
          & df[["algorithm"]] == baseline, measure]
  algorithms <- unique(df[["algorithm"]])
  if (!is.null(algorithm)) {
    algorithms <- algorithm
  }
  for (k in algorithms[algorithms != baseline]) {
    y <- df[df[["problem"]] == problem 
            & df[["algorithm"]] == k, measure]
    # Bayesian correlated t Test
    b_corr <- scmamp::bCorrelatedTtest(x, y, rho, rope)
    # results
    result_test <- get_data_frame(k = k, left = b_corr$posterior.probabilities[1], 
                             rope = b_corr$posterior.probabilities[2], 
                             right = b_corr$posterior.probabilities[3])
    result_bind <- rbind(result_bind, result_test)
    if (is.null(compare)) {compare <- "better"}
    if (compare == "better") { 
      threshold <- b_corr$posterior.probabilities[1]
      threshold_vv <- b_corr$posterior.probabilities[3]
    } else if (compare == "equal") {
      threshold <- b_corr$posterior.probabilities[2] + 
        b_corr$posterior.probabilities[1]
      threshold_vv <- b_corr$posterior.probabilities[2] + 
        b_corr$posterior.probabilities[3]
    } 
    if (is.null(prob)) {
      prob <- 0.95
    }
    if (threshold > prob | threshold_vv > prob) {
      part_result[k, "significant"] <- TRUE
    } else {
      part_result[k, "significant"] <- FALSE
    }
  }
  result <- cbind(result_bind, part_result)
  output <- get_results(baseline, measure, method = b_corr$method, 
                        data = result, 
                        extra = get_extras_scmamp(b_corr))
  return(output)
}

results_test <- b_corr_t_test(df= test_benchmark_small, problem = "problem_b",
                       baseline = "algo_1")
results_test



