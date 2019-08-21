
b_corr_t_test <- function(df, problemset, baseline, learner_b = NULL, measure = NULL,
                          compare = NULL, rho = 0.1, rope = c(-0.01, 0.01)) {
  result <- data.frame()
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problemset, baseline, learner_b = NULL, 
                                     measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  # define samples
  x <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == baseline, measure]
  algorithms <- unique(df[["algorithm"]])
  for (k in algorithms[algorithms != baseline]) {
    if (!is.null(learner_b)) {
      k <- learner_b
      y <- df[df[["problem"]] == problemset 
              & df[["algorithm"]] == k, measure]
    } else {
      y <- df[df[["problem"]] == problemset 
              & df[["algorithm"]] == k, measure]
    }
    # Bayesian correlated t Test
    b_corr <- scmamp::bCorrelatedTtest(x, y, rho, rope)
    # results
    #result[k, "baseline"] <- baseline
    #result[k, "method"] <- b_corr$method
    #result[k, "measure"] <- measure
    result[k, "algorithm"] <- k
    result[k, "left"] <- b_corr$posterior.probabilities[1]
    result[k, "rope"] <- b_corr$posterior.probabilities[2]
    result[k, "right"] <- b_corr$posterior.probabilities[3]
    if (is.null(compare)) {compare <- "better"}
    if (compare == "better") { 
      threshold <- b_corr$posterior.probabilities[3]
    } else if (compare == "equal") {
      threshold <- b_corr$posterior.probabilities[2] + 
        b_corr$posterior.probabilities[3]
    } 
    if (threshold > 0.95) {
      result[k, "significance_appears"] <- TRUE
    } else {
      result[k, "significance_appears"] <- FALSE
    }
    output <- get_test_results(baseline, method = b_corr$method, measure, data = result)
    class(output) <- "btest"
  }
  return(output)
}

#results <- b_corr_t_test(df = test_benchmark_small, rho=0.1, rope=c(-0.01, 0.01),
#                         problemset = 'problem_e', baseline = 'algo_1')
#results


#------------------------------------------------------------------------------#
btest <- function(test){
  UseMethod("btest")
}

get_test_results <- function(baseline, method, measure, data, extra = NULL) {
  output <- list(baseline = baseline, 
                 methode = method, 
                 measure = measure, 
                 data = data, 
                 extra = extra)
  class(output) = "btest"
}

print.btest <- function(output,...) {
  print("Result of Bayesian Test")
  #result <- output[[-"extra"]]
  print(output)
}
