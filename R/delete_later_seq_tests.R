## alle Algorithmen sollen gegeneinader getestet werden: 
# dafür muss keine Baseline und kein learner gewählt werden, sondern es werden 
# alle einfach so verwendet 

# zunächst alle werden gegeneinander getestet auch mit Wiederholungen 
seq_b_corr_t_test <- function(problemset, baseline = NULL, learner_b = NULL, 
                              measure = NULL, compare = NULL, rho = 0.1, 
                              rope = c(-0.01, 0.01), max_repls = 20, ...) {
  result = data.frame()
  algos <- unique(data[["algorithm"]])
  for (f in algos) {
    baseline <- f
    for (i in 2:max_repls) {
      data <- get_replications(i, ...)
      ## check if passed names, define columns in dataset
      checkmate::assert_true(check_structure(df = data))
      checkmate::assert_true(check_names(df = data, problemset, baseline, 
                                         learner_b = NULL, measure = NULL, 
                                         parameter_algorithm = NULL))
      if (is.null(measure)) {
        measure <- get_measure_columns(data)[1]
      }
      # define samples
      x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]
      algorithms <- unique(data[["algorithm"]])
      if (i == 2) {
        liste <- c()
      }
      algorithms <- setdiff(algorithms, liste)
      for (k in algorithms[algorithms != baseline]) {
        if (!is.null(learner_b)) {
          k <- learner_b
          y <- data[data[["problem"]] == problemset 
                    & data[["algorithm"]] == k, measure]
        } else {
          y <- data[data[["problem"]] == problemset 
                    & data[["algorithm"]] == k, measure]
        }
        # Bayesian correlated t Test
        b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
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
        if (is.null(compare)) {compare <- "better"}
        if (compare == "better") { 
          threshold <- b_test$posterior.probabilities[3]
        } else if (compare == "equal") {
          threshold <- b_test$posterior.probabilities[2] + 
            b_test$posterior.probabilities[3]
        } 
        if (threshold > 0.95) {
          result[k, "significance_appears"] <- TRUE
        } else {
          result[k, "significance_appears"] <- FALSE
        }
        liste <-  rownames(result[result[["significance_appears"]] == TRUE, ])
      }
      if (!is.null(learner_b)) {
        if (threshold > 0.95) {
          break 
        }
      }
    }
  }
  return(result)
}

#results <- seq_b_corr_t_test(df = test_benchmark_small, problemset = 'problem_a', 
#                             max_repls = 10, rho=0.1, 
#                             rope=c(-0.01, 0.01)) 
#results

