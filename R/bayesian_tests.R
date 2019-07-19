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
#' @example 
#' results <- b_corr_t_test(df= test_benchmark_small, problemset = "problem_a", 
#'                          learner_a = "algo_1", learner_b = "algo_2")
#' results
#' @export
b_corr_t_test <- function(df, problemset, learner_a, learner_b, 
                          measure = NULL, parameter_algorithm = NULL, 
                          rho = 0.1, rope = c(-0.01, 0.01)) {
  requireNamespace("scmamp", quietly = TRUE)
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problemset, learner_a, learner_b, 
                                     measure = NULL, parameter_algorithm = NULL))
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
#' For testing over multiple datasets, don´t specify the problemset argument in 
#' the function. 
#' Note that the default value for measure is the first measure column in the 
#' data frame.
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @example 
#' results <- b_sign_test(df= test_benchmark_small, problemset = "problem_a", 
#'                        learner_a = "algo_1", learner_b = "algo_2")
#' results
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
  checkmate::assert_true(check_names(df, problemset, learner_a, learner_b, 
                                     measure = NULL, parameter_algorithm = NULL))
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
#' For testing over multiple datasets, don´t specify the problemset argument in 
#' the function. 
#' Note that the default value for measure is the first measure column in the 
#' data frame.
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @example 
#' results <- b_signed_rank_test(df= test_benchmark_small, 
#'                               learner_a = "algo_1", learner_b = "algo_2")
#' results
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
  checkmate::assert_true(check_names(df, problemset = NULL, learner_a, 
                                     learner_b, measure = NULL, 
                                     parameter_algorithm = NULL))
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



#' @title Bayesian hierarchical correlated t-test
#' @description 
#' This function implements a Bayesian hierarchical test.
#' @param df Input data frame. 
#' @param learner_a First algorithm.
#' @param learner_b Second algorithm. 
#' @param measure Measure column. 
#' @param parameter_algorithm Specifies parameters concerning the corresponding algorithm. 
#' @param rho Correlation factor. 
#' @param std.upper Factor to set the upper bound for both sigma_i and sigma_0 (see Benavoli \emph{et al.} 2017 for more details)
#' @param d0.lower Lower bound for the prior for mu_0. If not provided, the smallest observed difference is used
#' @param d0.upper Upper bound for the prior for mu_0. If not provided, the biggest observed difference is used
#' @param alpha.lower Lower bound for the (uniform) prior for the alpha hyperparameter (see Benavoli \emph{et al.} 2017 for more details). Default value set at 0.5, as in the original paper
#' @param alpha.upper Upper bound for the (uniform) prior for the alpha hyperparameter (see Benavoli \emph{et al.} 2017 for more details). Default value set at 5, as in the original paper
#' @param beta.lower Lower bound for the (uniform) prior for the beta hyperparameter (see Benavoli \emph{et al.} 2017 for more details). Default value set at 0.05, as in the original paper
#' @param beta.lower Upper bound for the (uniform) prior for the beta hyperparameter (see Benavoli \emph{et al.} 2017 for more details). Default value set at 0.15, as in the original paper
#' @param z0 Position of the pseudo-observation associated to the prior Dirichlet Process. The default value is set to 0 (inside the rope)
#' @param rope Interval for the difference considered as "irrelevant"
#' @param nsim Number of samples (per chain) used to estimate the posterior distribution. Note that, by default, half the simulations are used for the burn-in
#' @param nchain Number of MC chains to be simulated. As half the simulations are used for the warm-up, the total number of simulations will be \code{nchain}*\code{nsim}/2
#' @param parallel Logical value. If \code{true}, Stan code is executed in parallel
#' @param stan.output.file String containing the base name for the output files produced by Stan. If \code{NULL}, no files are stored.
#' @param seed Optional parameter used to fix the random seed
#' @param ... Additional arguments for the rstan::stan function that runs the analysis 
#' @return A list with the following elements: 
#' \item{\code{method}}{a string with the name of the method used}
#' \item{\code{parameters}}{parameters used by the method}
#' \item{\code{posterior.probabilities}}{a vector with the left, rope and right probabilities}
#' \item{\code{approximated}}{a logical value, \code{TRUE} if the posterior distribution is approximated (sampled) and \code{FALSE} if it is exact}
#' \item{\code{posterior}}{Sampled probabilities (see details)}
#' \item{\code{additional}}{Additional information provided by the model. This includes:\code{per.dataset}, the results per dataset (left, rope and right probabilities together with the expected mean value); \code{global.sin} sampled probabilities of mu_0 being positive or negative and \code{stan.results}, the complete set of results produced by Stan program}
#' @details 
#' The results includes the typical information relative to the three areas of 
#' the posterior density (left, right and rope probabilities), both global and
#' per dataset (in the additional information). Also, the simulation results are 
#' included.
#' As for the prior parameters, they are set to the default values indicated in 
#' Benavoli \emph{et al.} 2017, except for the bound for the prior distribution 
#' of mu_0, which are set to the maximum and minimum values observed in the 
#' sample. You should not modify them unless you know what you are doing.
#' @example 
#' results <- b_hierarchical_test(df= test_benchmark_small, 
#'                                learner_a = "algo_1", learner_b = "algo_2", 
#'                                rho=0.1, rope=c(-0.01, 0.01), nsim=2000, 
#'                                nchains=5)
#' results 
#' @references \url{https://github.com/b0rxa/scmamp}
#' @export
b_hierarchical_test <- function(df, learner_a, learner_b, measure = NULL, 
                                parameter_algorithm = NULL, rho = 0.1, 
                                std.upper=1000, d0.lower=NULL, d0.upper=NULL, 
                                alpha.lower=0.5, alpha.upper=5, 
                                beta.lower=0.05, beta.upper=0.15,
                                rope = c(-0.01, 0.01), nsim=2000, nchains=8, 
                                parallel=TRUE, stan.output.file=NULL,
                                seed=as.numeric(Sys.time()), ...) {
  requireNamespace("scmamp", quietly = TRUE)
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problemset = NULL, 
                                     learner_a, learner_b, measure = NULL, 
                                     parameter_algorithm = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  if (!is.null(parameter_algorithm)){
    learner_a <- paste_algo_pars(algorithm = learner_a, parameter_algorithm)
    learner_b <- paste_algo_pars(algorithm = learner_b, parameter_algorithm)
    df[["algorithm"]] <- paste_algo_pars(algorithm = df[["algorithm"]], 
                                         parameter_algorithm = df[["parameter_algorithm"]])
  }
  # define samples 
  x.matrix <- data_transformation(df, algo = learner_a, measure)
  y.matrix <- data_transformation(df, algo = learner_b, measure)
  # check numbers in sample
  checkmate::assert_true(get_replications_count(x.matrix, y.matrix))
  # Bayesian correlated t Test 
  b_hierarchical <- scmamp::bHierarchicalTest(x.matrix, y.matrix, rho, 
                                              std.upper, d0.lower, d0.upper, 
                                              alpha.lower, alpha.upper, 
                                              beta.lower, beta.upper, 
                                              rope, nsim, nchains, parallel, 
                                              stan.output.file, seed)
  result <- list()
  result$measure <- measure
  result$method <- b_hierarchical$method
  result$posteriror_probabilities <- b_hierarchical$posterior.probabilities
  return(result)
}

