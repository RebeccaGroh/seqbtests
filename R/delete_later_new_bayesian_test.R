


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

#------------------------------------------------------------------------------#

b_sign_test <- function(df, problemset, baseline, learner_b = NULL, measure = NULL, 
                        s = 1, z_0 = 0, weights = c(s/2, rep(1, length(x))), compare = NULL, 
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
    if (!is.null(learner_b)) {
       k <- learner_b    ## testen ob das so einfach funktioniert oder ob das dann trotzdem k mal ausgeführt wird
    }
    # define samples when testing on multiple datasets
    if (is.null(problemset)) {
      data_wide <- tidyr::spread(df, algorithm, measure)
      sum_data <- aggregate(data_wide[, c(baseline, k)], 
                            by = list(data_wide[["problem"]]), FUN = mean)
      x <- sum_data[, baseline]
      y <- sum_data[, k]
    } else {
      # define samples when testing on a single dataset
      x <- df[df[["problem"]] == problemset 
              & df[["algorithm"]] == baseline, measure]
      y <- df[df[["problem"]] == problemset 
              & df[["algorithm"]] == k, measure]
    }
    n.samples <- mc_samples
    # Bayesian Sign Test
    b_sign <- rNPBST::bayesianSign.test(x, y, s, z_0, rope.min, rope.max, 
                                        weights, n.samples)
    # results
    result[k, "baseline"] <- baseline
    result[k, "method"] <- b_sign$method
    result[k, "measure"] <- measure
    result[k, "left"] <- b_sign$probabilities[1]
    result[k, "rope"] <- b_sign$probabilities[2]
    result[k, "right"] <- b_sign$probabilities[3]
    if (is.null(compare)) {compare <- "better"}
    if (compare == "better") { 
      threshold <- b_sign$probabilities[3]
    } else if (compare == "equal") {
      threshold <- b_sign$probabilities[2] + 
        b_sign$probabilities[3]
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
#                       baseline = 'algo_1', learner_b = 'algo_3')
#results


#------------------------------------------------------------------------------#



b_signed_rank_test <- function(df, problemset = NULL, baseline, learner_b = NULL, 
                               measure = NULL, parameter_algorithm = NULL, 
                               s = 0.5, z_0 = 0, weights = NULL, compare = NULL, 
                               mc_samples = 1e+05, rope = c(-0.01, 0.01)) {
  result <- data.frame()
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the rope 
            (min, max), but the values are not orderd. They will be swapped to
            follow with the procedure")
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problemset = NULL, baseline, 
                                     learner_b, measure = NULL, 
                                     parameter_algorithm = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algorithms <- unique(df[["algorithm"]])
  for (k in algorithms[algorithms != baseline]) {
    if (!is.null(learner_b)) {
      k <- learner_b    ## testen ob das so einfach funktioniert oder ob das dann trotzdem k mal ausgeführt wird
    }
    # define samples when testing on multiple datasets
    if (is.null(problemset)) {
      data_wide <- tidyr::spread(df, algorithm, measure)
      sum_data <- aggregate(data_wide[, c(baseline, k)], 
                            by = list(data_wide[["problem"]]), FUN = mean)
      x <- sum_data[, baseline]
      y <- sum_data[, k]
    } else {
      # define samples when testing on a single dataset
      x <- df[df[["problem"]] == problemset 
              & df[["algorithm"]] == baseline, measure]
      y <- df[df[["problem"]] == problemset 
              & df[["algorithm"]] == k, measure]
    }
    mc.samples <- mc_samples
    # Bayesian signed rank test
    b_signed_rank <- rNPBST::bayesianSignedRank.test(x, y, s, z_0, 
                                                   rope.min, rope.max, 
                                                   weights, mc.samples)
    # results
    result[k, "baseline"] <- baseline
    result[k, "method"] <- b_signed_rank$method
    result[k, "measure"] <- measure
    result[k, "left"] <- b_signed_rank$probabilities[1]
    result[k, "rope"] <- b_signed_rank$probabilities[2]
    result[k, "right"] <- b_signed_rank$probabilities[3]
    if (is.null(compare)) {compare <- "better"}
    if (compare == "better") { 
      threshold <- b_signed_rank$probabilities[3]
    } else if (compare == "equal") {
      threshold <- b_signed_rank$probabilities[2] + 
        b_signed_rank$probabilities[3]
    } 
    if (threshold > 0.95) {
      result[k, "significance_appears"] <- TRUE
    } else {
      result[k, "significance_appears"] <- FALSE
    }
  }
  return(result)
}

#results <- b_signed_rank_test(df= test_benchmark_small, 
#                       problemset = 'problem_a', 
#                       baseline = 'algo_1')
#results
 
#------------------------------------------------------------------------------#




b_hierarchical_test <- function(df, baseline, learner_b = NULL,  measure = NULL, 
                                parameter_algorithm = NULL, rho = 0.1, 
                                std.upper = 1000, d0.lower = NULL, compare = NULL, 
                                d0.upper = NULL, alpha.lower = 0.5, 
                                alpha.upper = 5, beta.lower = 0.05, 
                                beta.upper = 0.15, rope = c(-0.01, 0.01), 
                                nsim = 2000, nchains = 8, parallel = TRUE, 
                                stan.output.file = NULL, 
                                seed = as.numeric(Sys.time()), ...) {
  result <- data.frame()
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problemset = NULL, baseline, 
                                     learner_b = NULL, measure = NULL, 
                                     parameter_algorithm = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algorithms <- unique(df[["algorithm"]])
  for (k in algorithms[algorithms != baseline]) {
    if (!is.null(learner_b)) {
      k <- learner_b    ## testen ob das so einfach funktioniert oder ob das dann trotzdem k mal ausgeführt wird
    }
    # define samples
    x.matrix <- data_transformation(df, algo = baseline, measure)
    y.matrix <- data_transformation(df, algo = k, measure)
    # check numbers in sample
    checkmate::assert_true(get_replications_count(x.matrix, y.matrix))
    # Bayesian correlated t Test
    b_hierarchical <- scmamp::bHierarchicalTest(x.matrix, y.matrix, rho, std.upper, d0.lower, d0.upper, 
                                                alpha.lower, alpha.upper, beta.lower, beta.upper, 
                                                rope, nsim, nchains, parallel, stan.output.file,
                                                seed)
    # results
    result[k, "baseline"] <- baseline
    result[k, "method"] <- b_hierarchical$method
    result[k, "measure"] <- measure
    result[k, "left"] <- b_hierarchical$posterior.probabilities[1]
    result[k, "rope"] <- b_hierarchical$posterior.probabilities[2]
    result[k, "right"] <- b_hierarchical$posterior.probabilities[3]
    if (is.null(compare)) {compare <- "better"}
    if (compare == "better") { 
      threshold <- b_hierarchical$posterior.probabilities[3]
    } else if (compare == "equal") {
      threshold <- b_hierarchical$posterior.probabilities[2] + 
        b_hierarchical$posterior.probabilities[3]
    } 
    if (threshold > 0.95) {
      result[k, "significance_appears"] <- TRUE
    } else {
      result[k, "significance_appears"] <- FALSE
    }
  }
  return(result)
}


#results <- b_hierarchical_test(df= test_benchmark_small, baseline = 'algo_1', 
#                               learner_b = 'algo_2', rho=0.1, rope = c(-0.1, 0.1), 
#                                nsim=2000,  nchains=5)
#results
