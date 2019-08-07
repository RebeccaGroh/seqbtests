

seq_b_corr_t_test <- function(problemset, baseline, learner_b = NULL, measure = NULL, test = NULL, rho = 0.1, rope = c(-0.01, 0.01), max_repls = 20, ...) {
  result = data.frame()
    for (i in 2:max_repls) {
        data <- get_replications(i, ...)
        ## check if passed names, define columns in dataset
        checkmate::assert_true(check_structure(df = data))
        checkmate::assert_true(check_names(df = data, problemset, baseline, learner_b = NULL, measure = NULL, parameter_algorithm = NULL))
        if (is.null(measure)) {
            measure <- get_measure_columns(data)[1]
        }
        # define samples
        x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]
        algorithms <- unique(data[["algorithm"]])
        
    }
    return(result)
}


results <- seq_b_corr_t_test(df = test_benchmark_small, problemset = 'problem_a', baseline = 'algo_1', max_repls = 10, rho=0.1, rope=c(-0.01, 0.01)) 
results

iterate_algorithms = function() {
  for (k in algorithms[algorithms != baseline]) {
    if (!is.null(learner_b)) {
      k <- learner_b
      y <- data[data[["problem"]] == problemset & data[["algorithm"]] == k, measure]
    } else {
      y <- data[data[["problem"]] == problemset & data[["algorithm"]] == k, measure]
    }
    # Bayesian correlated t Test
    b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
    if (b_test$posterior.probabilities[3] > 0.95) {
      result[k, "significance_appears"] <- TRUE
    } else {
      result[k, "significance_appears"] <- FALSE
    }
    if (result[k, "significance_appears"] == TRUE) {
      break
    }
    #if (b_test$posterior.probabilities[3] > 0.95) {
    #  break
    #}
    result[k, "baseline"] <- baseline
    result[k, "method"] <- b_test$method
    result[k, "measure"] <- measure
    result[k, "left"] <- b_test$posterior.probabilities[1]
    result[k, "rope"] <- b_test$posterior.probabilities[2]
    result[k, "right"] <- b_test$posterior.probabilities[3]
    result[k, "repls"] <- i
  }
}