#' @title Sequential bayesian correlated t test 
#' @description 
#' This function implements a sequential approach to compare the performance of 
#' machine learning algorithms to one another. Sample size is not fixed in 
#' adavnce, data are evaluated as they are collected. Further sampling is 
#' stopped in accordance with a pre-defined stopping rule as soon as significant 
#' results are obtained.  
#' @param problemset Problemset on which the test should be performed. 
#' @param baseline First algorithm.
#' @param learner_b Second algorithm. If not defined, every algorithm will be 
#' tested against baseline. 
#' @param measure Measure column. 
#' @param compare Defines whether the performances should be tested for either 
#' being better ('better') or being just as good ('equal'). If not defined, the 
#' default is to test for 'better'.
#' @param rho Correlation factor. 
#' @param rope Region of practical equivalence. 
#' @param max_repls maximum number of replications that should be build in 
#' get_replications, or maximum number of replications in data frame, if 
#' complete data frame is being used.  
#' @param rope Region of practical equivalence. 
#' @return A data frame with one row for each considered algorithm that is 
#' tested against the baseline, containing the following components:
#' \item{code{measure}}{a string with the name of the measure column used}
#' \item{code{method}}{a string with the name of the method used}
#' \item{code{posteriror_probabilities}}{a vector with the left, rope and right 
#' probabilities}
#' \item{code{repls}}{number of the considered replications}
#' @details The basics of the test have first been implemented in scmamp. 
#' Note that the default value for measure is the first measure column in the 
#' data frame. The default of rho is 0.1. If rho equals 0 this converts the test 
#' in the equivalent of the standard t test. 
#' @example results <- seq_b_corr_t_test(df = test_benchmark_small, rho=0.1,
#'                                       problemset = 'problem_a', 
#'                                       baseline = 'algo_1', test = 'equal', 
#'                                       max_repls = 10,  rope=c(-0.01, 0.01))
#' @export
seq_b_corr_t_test <- function(problemset, baseline, learner_b = NULL, 
                              measure = NULL, compare = NULL, rho = 0.1, 
                              rope = c(-0.01, 0.01), max_repls = 20, ...) {
  result = data.frame()
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
      if (threshold > 0.95 & i >= 5) {
        result[k, "significance_appears"] <- TRUE
      } else {
        result[k, "significance_appears"] <- FALSE
      }
      liste <-  rownames(result[result[["significance_appears"]] == TRUE, ])
    }
    if (!is.null(learner_b)) {
      if (threshold > 0.95 & i >= 5) {
        break 
      }
    }
  }
  return(result)
}

#------------------------------------------------------------------------------#
#----------------------------- Bayesian Sign Test -----------------------------# 


seq_b_sign_test <- function(problemset = NULL, baseline, learner_b = NULL, 
                            measure = NULL, compare = NULL, s = 1, z_0 = 0,
                            weights = c(s/2, rep(1, length(x))), 
                            mc_samples = 1e+05, rope = c(-0.01, 0.01), 
                            max_repls = 20, ...) {
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the rope 
            (min, max), but the values are not orderd. They will be swapped to
            follow with the procedure")
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  result = data.frame()
  for (i in 2:max_repls) {
    data <- get_replications(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, problemset = NULL, baseline, 
                                       learner_b = NULL, measure = NULL, 
                                       parameter_algorithm = NULL))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    ## alle "learner_b" mit k ersetzen? 
    algorithms <- unique(data[["algorithm"]])
    if (i == 2) {
      liste <- c()
    }
    algorithms <- setdiff(algorithms, liste)
    for (k in algorithms[algorithms != baseline]) {
      if (!is.null(learner_b)) {
        k <- learner_b
      }
      # define samples when testing on multiple datasets
      if (is.null(problemset)) {
        data_wide <- tidyr::spread(data, algorithm, measure)
        sum_data <- aggregate(data_wide[, c(baseline, k)], 
                              by = list(data_wide[["problem"]]), FUN = mean)
        x <- sum_data[, baseline]
        y <- sum_data[, k]
      } else {
        # define samples when testing on a single dataset
        x <- data[data[["problem"]] == problemset 
                  & data[["algorithm"]] == baseline, measure]
        y <- data[data[["problem"]] == problemset 
                  & data[["algorithm"]] == k, measure]
      }
      n.samples <- mc_samples
      # Bayesian Sign Test
      b_sign <- rNPBST::bayesianSign.test(x, y, s, z_0, rope.min, rope.max, 
                                          weights, n.samples)
      
      result[k, "baseline"] <- baseline
      result[k, "method"] <- b_sign$method
      result[k, "measure"] <- measure
      result[k, "left"] <- b_sign$probabilities[1]
      result[k, "rope"] <- b_sign$probabilities[2]
      result[k, "right"] <- b_sign$probabilities[3]
      result[k, "repls"] <- i
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
      liste <-  rownames(result[result[["significance_appears"]] == TRUE, ])
    }
    if (!is.null(learner_b)) {
      if (threshold > 0.95) {
        break 
      }
    }
  }
  return(result)
}




#------------------------------------------------------------------------------#
#-------------------------- Bayesian Signed Rank Test -------------------------# 


seq_b_signed_rank_test <- function(problemset = NULL, baseline, learner_b = NULL, 
                                   measure = NULL, compare = NULL, s = 0.5, z_0 = 0,
                                   weights = NULL, mc_samples = 1e+05, 
                                   rope = c(-0.01, 0.01), max_repls = 20, ...) {
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the rope 
            (min, max), but the values are not orderd. They will be swapped to
            follow with the procedure")
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  result = data.frame()
  for (i in 2:max_repls) {
    data <- get_replications(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, problemset = NULL, baseline, 
                                       learner_b = NULL, measure = NULL, 
                                       parameter_algorithm = NULL))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    ## alle "learner_b" mit k ersetzen? 
    algorithms <- unique(data[["algorithm"]])
    if (i == 2) {
      liste <- c()
    }
    algorithms <- setdiff(algorithms, liste)
    for (k in algorithms[algorithms != baseline]) {
      if (!is.null(learner_b)) {
        k <- learner_b
      }
      # define samples when testing on multiple datasets
      if (is.null(problemset)) {
        data_wide <- tidyr::spread(data, algorithm, measure)
        sum_data <- aggregate(data_wide[, c(baseline, k)], 
                              by = list(data_wide[["problem"]]), FUN = mean)
        x <- sum_data[, baseline]
        y <- sum_data[, k]
      } else {
        # define samples when testing on a single dataset
        x <- data[data[["problem"]] == problemset 
                  & data[["algorithm"]] == baseline, measure]
        y <- data[data[["problem"]] == problemset 
                  & data[["algorithm"]] == k, measure]
      }
      mc.samples <- mc_samples
      # Bayesian Sign Test
      b_sign <- rNPBST::bayesianSignedRank.test(x, y, s, z_0, rope.min, rope.max, weights, mc.samples)
      result[k, "baseline"] <- baseline
      result[k, "method"] <- b_sign$method
      result[k, "measure"] <- measure
      result[k, "left"] <- b_sign$probabilities[1]
      result[k, "rope"] <- b_sign$probabilities[2]
      result[k, "right"] <- b_sign$probabilities[3]
      result[k, "repls"] <- i
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
      liste <-  rownames(result[result[["significance_appears"]] == TRUE, ])
    }
    if (!is.null(learner_b)) {
      if (threshold > 0.95) {
        break 
      }
    }
  }
  return(result)
}



#------------------------------------------------------------------------------#
#------------------ Bayesian hierarchical correlated t-test -------------------# 

seq_b_hierarchical_test <- function(baseline, learner_b = NULL, measure = NULL, 
                                    compare = NULL, rho = 0.1, max_repls = 20, 
                                    rope = c(-0.01, 0.01), std.upper = 1000,
                                    d0.lower = NULL, d0.upper = NULL,
                                    alpha.lower = 0.5, alpha.upper = 5, 
                                    beta.lower = 0.05, beta.upper = 0.15,
                                    nsim = 2000, nchains = 8, parallel = TRUE,
                                    stan.output.file = NULL, 
                                    seed = as.numeric(Sys.time()), ...) {
  result = data.frame()
  for (i in 2:max_repls) {
    data <- get_replications(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, baseline, learner_b = NULL,
                                       measure = NULL, problemset = NULL))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    algorithms <- unique(data[["algorithm"]])
    if (i == 2) {
      liste <- c()
    }
    algorithms <- setdiff(algorithms, liste)
    # define samples
    x.matrix <- data_transformation(data, algo = baseline, measure)
    for (k in algorithms[algorithms != baseline]) {
      if (!is.null(learner_b)) {
        k <- learner_b
        y.matrix <- data_transformation(data, algo = k, measure)
      } else {
        y.matrix <- data_transformation(data, algo = k, measure)
      }
      # check numbers in sample
      checkmate::assert_true(get_replications_count(x.matrix, y.matrix))
      # Bayesian correlated t Test
      b_hierarchical <- scmamp::bHierarchicalTest(x.matrix, y.matrix, rho, 
                                                  std.upper, d0.lower, 
                                                  d0.upper, alpha.lower,
                                                  alpha.upper, beta.lower,
                                                  beta.upper, rope, nsim,
                                                  nchains, parallel, 
                                                  stan.output.file, seed)
      result[k, "baseline"] <- baseline
      result[k, "method"] <- b_hierarchical$method
      result[k, "measure"] <- measure
      result[k, "left"] <- b_hierarchical$posterior.probabilities[1]
      result[k, "rope"] <- b_hierarchical$posterior.probabilities[2]
      result[k, "right"] <- b_hierarchical$posterior.probabilities[3]
      result[k, "repls"] <- i
      if (is.null(compare)) {
        compare <- "better"
      }
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
      liste <- rownames(result[result[["significance_appears"]] == TRUE,])
    }
    if (!is.null(learner_b)) {
      if (threshold > 0.95) {
        break
      }
    }
  }
  return(result)
}


