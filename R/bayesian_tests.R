#' @title Bayesian correlated T test 
#' @description 
#' This function implements the Bayesian version of the correlated t-test. 
#' @param df Input data frame. 
#' @param problemset Problemset on which the test should be performed. 
#' @param learner_a First algorithm.
#' @param learner_b Second algorithm. 
#' @param measure Measure column. 
#' @param parameter_algorithm Specifies parameters concerning the corresponding 
#' algorithm. 
#' @param rho Correlation factor. 
#' @param rope Region of practical equivalence. 
#' @return A list containing the following components:
#' \item{code{measure}}{a string with the name of the measure column used}
#' \item{code{method}}{a string with the name of the method used}
#' \item{code{posteriror_probabilities}}{a vector with the left, rope and right 
#' probabilities}
#' @details 
#' The test has first been implemented in scmamp. 
#' Note that the default value for measure is the first measure column in the 
#' data frame. The default of rho is 0.1. If rho equals 0 this converts the test 
#' in the equivalent of the standard t test    
#' @references \url{https://github.com/b0rxa/scmamp}
#' @export
b_corr_t_test <- function(df, problemset, learner_a, learner_b, 
                          measure = NULL, parameter_algorithm = NULL, 
                          rho = 0.1, rope = c(-0.01, 0.01)) {
  requireNamespace("scmamp", quietly = TRUE)
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  ## oder der User gibt direkt den richtigen Namen für learner_a/_b an. 
  if (!is.null(parameter_algorithm)){
    learner_a <- paste_algo_pars(algorithm = learner_a, parameter_algorithm)
    learner_b <- paste_algo_pars(algorithm = learner_b, parameter_algorithm)
    df[["algorithm"]] <- paste_algo_pars(algorithm = df[["algorithm"]], 
                                         parameter_algorithm = df[["parameter_algorithm"]])
  }
  # define samples 
  x <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_a, measure]
  y <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_b, measure]
  # check numbers in sample
  checkmate::assert_true(get_replications_count(x, y))
  # Bayesian correlated t Test 
  b_corr <- scmamp::bCorrelatedTtest(x, y, rho, rope)
  result <- list()
  result$measure <- measure
  result$method <- b_corr$method
  result$posteriror_probabilities <- b_corr$posterior.probabilities
  # needed for plotting 
  #result$approximate <- b_corr$approximate
  #result$posterior <- b_corr$posterior
  #result$additional <- b_corr$additional
  #result$parameters <- b_corr$parameters
  return(result)
}


#' @title Bayesian Sign test 
#' @description 
#' This function implements the Bayesian version of the sign test. 
#' @param df Input data frame. 
#' @param problemset Problemset on which the test should be performed. 
#' @param learner_a First algorithm.
#' @param learner_b Second algorithm. 
#' @param measure Measure column. 
#' @param parameter_algorithm Specifies parameters concerning the corresponding 
#' algorithm. 
#' @param z_0 Prior pseudo-observation. 
#' @param s Prior pseudo-observation probability. 
#' @param weights A-priori weigthts. 
#' @param mc_samples Number of samples of the distribution. 
#' @param rope Region of practical equivalence. 
#' @return A list containing the following components:
#' \item{code{measure}}{a string with the name of the measure column used}
#' \item{code{method}}{a string with the name of the method used}
#' \item{code{posteriror_probabilities}}{a vector with the left, rope and right 
#' probabilities}
#' @details 
#' The test has first been implemented in rNPBST.  
#' Note that the default value for measure is the first measure column in the 
#' data frame.
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @export
b_sign_test <- function(df, problemset, learner_a, learner_b, measure = NULL, 
                        parameter_algorithm = NULL, s = 1, z_0 = 0, 
                        weights = c(s/2, rep(1, length(x))), mc_samples = 100000, 
                        rope = c(-0.01, 0.01)){
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the rope
            (min, max), but the values are not orderd. They will be swapped to
            follow with the procedure")
    
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  ## oder der User gibt direkt den richtigen Namen für learner_a/_b an. 
  if (!is.null(parameter_algorithm)){
    learner_a <- paste_algo_pars(algorithm = learner_a, parameter_algorithm)
    learner_b <- paste_algo_pars(algorithm = learner_b, parameter_algorithm)
    df[["algorithm"]] <- paste_algo_pars(algorithm = df[["algorithm"]], 
                                         parameter_algorithm = df[["parameter_algorithm"]])
  }
  # define samples 
  x <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_a, measure]
  y <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_b, measure]
  n.samples <- mc_samples 
  # Bayesian Sign Test 
  b_sign <- rNPBST::bayesianSign.test(x, y, s, z_0, rope.min, rope.max,
                                      weights, n.samples)
  result <- list()
  result$measure <- measure
  result$method <- b_sign$method
  result$posteriror_probabilities <- b_sign$probabilities
  return(result)
}


 
#' @title Bayesian Signed Rank test 
#' @description 
#' This function implements the Bayesian version of the signed rank test. 
#' @param df Input data frame. 
#' @param problemset Problemset on which the test should be performed. 
#' @param learner_a First algorithm.
#' @param learner_b Second algorithm. 
#' @param measure Measure column. 
#' @param parameter_algorithm Specifies parameters concerning the corresponding 
#' algorithm. 
#' @param z_0 Prior pseudo-observation. 
#' @param s Prior pseudo-observation probability. 
#' @param weights A-priori weigthts. 
#' @param mc_samples Number of samples of the distribution. 
#' @param rope Region of practical equivalence. 
#' @return A list containing the following components:
#' \item{code{measure}}{a string with the name of the measure column used}
#' \item{code{method}}{a string with the name of the method used}
#' \item{code{posteriror_probabilities}}{a vector with the left, rope and right 
#' probabilities}
#' @details 
#' The test has first been implemented in rNPBST. 
#' Note that the default value for measure is the first measure column in the 
#' data frame. 
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @export
b_signed_rank_test <- function (df, problemset = NULL, learner_a, learner_b, measure = NULL, 
                                parameter_algorithm = NULL, s = 0.5, z_0 = 0, 
                                weights = NULL, mc_samples = 100000, 
                                rope = c(-0.01, 0.01)){
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the rope
            (min, max), but the values are not orderd. They will be swapped to
            follow with the procedure")
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  ## oder der User gibt direkt den richtigen Namen für learner_a/_b an. 
  if (!is.null(parameter_algorithm)){
    learner_a <- paste_algo_pars(algorithm = learner_a, parameter_algorithm)
    learner_b <- paste_algo_pars(algorithm = learner_b, parameter_algorithm)
    df[["algorithm"]] <- paste_algo_pars(algorithm = df[["algorithm"]], 
                                         parameter_algorithm = df[["parameter_algorithm"]])
  }
  # define samples when testing on multiple datasets
  if (is.null(problemset)) {
    data_wide <- spread(df, algorithm, measure)
    sum_data <- aggregate(data_wide[, c(learner_a, learner_b)],
                          by = list(data_wide[["problem"]]), FUN = mean)
    x <- sum_data[,  learner_a]
    y <- sum_data[,  learner_b]
  } else{
    # define samples when testing on a single dataset 
    x <- df[df[["problem"]] == problemset 
            & df[["algorithm"]] == learner_a, measure]
    y <- df[df[["problem"]] == problemset 
            & df[["algorithm"]] == learner_b, measure]
  }
  mc.samples <- mc_samples 
  # Bayesian signed rank test 
  b_signed_rank <- rNPBST::bayesianSignedRank.test(x, y, s, z_0, rope.min, 
                                                   rope.max, weights, mc.samples)
  result <- list()
  result$measure <- measure
  result$method <- b_signed_rank$method
  result$posteriror_probabilities <- b_signed_rank$probabilities
  return(result)
}

#-------------------------------------------------------------------------------
# Bayesian hierarchical correlated t-test




#-------------------------------------------------------------------------------
## testen 
## mit measure 
results <- b_sign_test(df = benchmark_test_full, problemset = "Adiac", learner_a = "classif.xgboost", learner_b = "classif.ksvm", measure = "measure_ber", rope=c(-0.01, 0.01))
## ohne measure 
results <- b_corr_t_test(df = benchmark_test_full, problemset = "Adiac", learner_a = "classif.xgboost", learner_b = "classif.ksvm", rho=0.1, rope=c(-0.01, 0.01))
results

