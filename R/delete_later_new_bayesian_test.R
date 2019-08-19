


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
    result[k, "baseline"] <- baseline
    result[k, "method"] <- b_corr$method
    result[k, "measure"] <- measure
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
  }
  return(result)
}

#results <- b_corr_t_test(df = test_benchmark_small, rho=0.1, rope=c(-0.01, 0.01),
#                         problemset = 'problem_e', baseline = 'algo_1')
#results


b_sign_test <- function(df, problemset, baseline, learner_b = NULL, measure = NULL, 
                        s = 1, z_0 = 0, weights = c(s/2, rep(1, length(x))), 
                        mc_samples = 1e+05, rope = c(-0.01, 0.01)) {
  result <- data.frame()
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the 
                  rope (min, max), but the values are not orderd. They will be 
                  swapped to follow with the procedure")
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problemset, baseline, learner_b, 
                                     measure = NULL, parameter_algorithm = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algorithms <- unique(df[["algorithm"]])
  for (k in algorithms[algorithms != baseline]) {
    # define samples when testing on multiple datasets
    if (is.null(problemset)) {
      data_wide <- tidyr::spread(df, algorithm, measure)
      sum_data <- aggregate(data_wide[, c(baseline, learner_b)], 
                            by = list(data_wide[["problem"]]), FUN = mean)
      x <- sum_data[, baseline]
      y <- sum_data[, learner_b]
    } else {
      # define samples when testing on a single dataset
      x <- df[df[["problem"]] == problemset 
              & df[["algorithm"]] == baseline, measure]
      y <- df[df[["problem"]] == problemset 
              & df[["algorithm"]] == learner_b, measure]
    }
    n.samples <- mc_samples
    # Bayesian Sign Test
    b_sign <- rNPBST::bayesianSign.test(x, y, s, z_0, rope.min, rope.max, 
                                        weights, n.samples)
    # results
    result[k, "baseline"] <- baseline
    result[k, "method"] <- b_sign$method
    result[k, "measure"] <- measure
    result[k, "left"] <- b_sign$posterior.probabilities[1]
    result[k, "rope"] <- b_sign$posterior.probabilities[2]
    result[k, "right"] <- b_sign$posterior.probabilities[3]
    if (is.null(compare)) {compare <- "better"}
    if (compare == "better") { 
      threshold <- b_sign$posterior.probabilities[3]
    } else if (compare == "equal") {
      threshold <- b_sign$posterior.probabilities[2] + 
        b_sign$posterior.probabilities[3]
    } 
    if (threshold > 0.95) {
      result[k, "significance_appears"] <- TRUE
    } else {
      result[k, "significance_appears"] <- FALSE
    }
  }
  return(result)
}


#results <- b_sign_test(df= test_benchmark_small, 
#                       problemset = 'problem_a', 
#                       baseline = 'algo_1', learner_b = 'algo_2')
