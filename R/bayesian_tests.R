#' @title Bayesian correlated t test 
#' @description 
#' This function implements the Bayesian version of the correlated t-test. The 
#' performance of one baseline algorithm on one data set is compared to either 
#' one or multiple algorithms.  
#' @param df Input data frame. 
#' @param problemset Problem set on which the test should be performed. 
#' @param baseline First algorithm. 
#' @param algorithm Algorithm to be compared. If no algorithm is defined, the 
#' baseline is compared to every algorithm in the data frame. 
#' @param measure Measure column. 
#' @param rho Correlation factor. 
#' @param rope Region of practical equivalence. 
#' @return A list containing the following components:
#' \item{code{measure}}{A string with the name of the measure column used.}
#' \item{code{method}}{A string with the name of the method used.}
#' \item{code{baseline}{A string with the name of the baseline algorithm.}
#' \item{code{posteriror_probabilities}}{A data frame with one row for every 
#' algorithm that is compared to the baseline. The columns show the posterior 
#' probabilities and whether significance appears.} 
#' @details
#' The test has first been implemented in scmamp. 
#' Note that the default value for measure is the first measure column in the 
#' data frame. The default of rho is 0.1. If rho equals 0 this converts the test 
#' in the equivalent of the standard t test.   
#' @references \url{https://github.com/b0rxa/scmamp}
#' @example 
#' results <- b_corr_t_test(df= test_benchmark_small, problemset = "problem_a", 
#'                          baseline = "algo_1", algorithm = "algo_2")
#' @export
b_corr_t_test <- function(df, problemset, baseline, algorithm = NULL, 
                          measure = NULL, compare = NULL, rho = 0.1, 
                          rope = c(-0.01, 0.01)) {
    result <- data.frame()
    checkmate::assert_true(check_structure(df))
    checkmate::assert_true(check_names(df, problemset, baseline, 
                                       algorithm = NULL, measure = NULL))
    if (is.null(measure)) {
        measure <- get_measure_columns(df)[1]
    }
    # define samples
    x <- df[df[["problem"]] == problemset 
            & df[["algorithm"]] == baseline, measure]
    algorithms <- unique(df[["algorithm"]])
    for (k in algorithms[algorithms != baseline]) {
        if (!is.null(algorithm)) {
            k <- algorithm
            y <- df[df[["problem"]] == problemset 
                    & df[["algorithm"]] == k, measure]
        } else {
            y <- df[df[["problem"]] == problemset 
                    & df[["algorithm"]] == k, measure]
        }
        # Bayesian correlated t Test
        b_corr <- scmamp::bCorrelatedTtest(x, y, rho, rope)
        # results
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
            result[k, "significant"] <- TRUE
        } else {
            result[k, "significant"] <- FALSE
        }
    }
    output_test <- get_results(baseline, measure, method = b_corr$method, 
                               data = result, 
                               extra = list(b_corr$additional, 
                                            b_corr$approximate, 
                                            b_corr$parameters, 
                                            b_corr$posterior))
    return(output_test)
}

#' @title Bayesian sign test 
#' @description 
#' This function implements the Bayesian version of the sign test. 
#' @param df Input data frame. 
#' @param problemset Problem set on which the test should be performed. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. 
#' @param measure Measure column. 
#' @param z_0 Prior pseudo-observation. 
#' @param s Prior pseudo-observation probability. 
#' @param weights A-priori weights. 
#' @param mc_samples Number of samples of the distribution. 
#' @param rope Region of practical equivalence. 
#' @return A list containing the following components:
#' \item{code{measure}}{A string with the name of the measure column used.}
#' \item{code{method}}{A string with the name of the method used.}
#' \item{code{posteriror_probabilities}}{A vector with the left, rope and right 
#' probabilities.}
#' @details 
#' The test has first been implemented in rNPBST.  
#' For testing over multiple datasets, don´t specify the problem set argument 
#' in the function. 
#' Note that the default value for measure is the first measure column in the 
#' data frame.
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @example 
#' results <- b_sign_test(df= test_benchmark_small, problemset = "problem_a", 
#'                        baseline = "algo_1", algorithm = "algo_2")
#' @export
b_sign_test <- function(df, problemset, baseline, algorithm = NULL, 
                        measure = NULL, compare = NULL, 
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
  checkmate::assert_true(check_names(df, problemset, baseline, algorithm, 
                                     measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algorithms <- unique(df[["algorithm"]])
  for (k in algorithms[algorithms != baseline]) {
    if (!is.null(algorithm)) {
      k <- algorithm    ## testen ob das so einfach funktioniert oder ob das dann trotzdem k mal ausgeführt wird
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
    result[k, "algorithm"] <- k
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
      result[k, "significant"] <- TRUE
    } else {
      result[k, "significant"] <- FALSE
    }
  }
  output_test <- get_results(baseline, measure, method = b_sign$method, 
                             data = result, 
                             extra = list(b_sign$sample))
  return_test <- format_test(output_test)
  return(return_test)
}


#' @title Bayesian Signed Rank test 
#' @description 
#' This function implements the Bayesian version of the signed rank test. 
#' @param df Input data frame. 
#' @param problemset Problem set on which the test should be performed. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. 
#' @param measure Measure column. 
#' @param z_0 Prior pseudo-observation. 
#' @param s Prior pseudo-observation probability. 
#' @param weights A-priori weights. 
#' @param mc_samples Number of samples of the distribution. 
#' @param rope Region of practical equivalence. 
#' @return A list containing the following components:
#' \item{code{measure}}{A string with the name of the measure column used.}
#' \item{code{method}}{A string with the name of the method used.}
#' \item{code{posteriror_probabilities}}{A vector with the left, rope and right 
#' probabilities.}
#' @details 
#' The test has first been implemented in rNPBST. 
#' For testing over multiple datasets, don't specify the problem set argument 
#' in the function. 
#' Note that the default value for measure is the first measure column in the 
#' data frame.
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @example 
#' results <- b_signed_rank_test(df= test_benchmark_small, 
#'                               baseline = "algo_1", algorithm = "algo_2")
#' @export
b_signed_rank_test <- function(df, problemset = NULL, baseline, compare = NULL,
                               algorithm = NULL, measure = NULL, 
                               s = 0.5, z_0 = 0, weights = NULL,  
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
                                     algorithm, measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algorithms <- unique(df[["algorithm"]])
  for (k in algorithms[algorithms != baseline]) {
    if (!is.null(algorithm)) {
      k <- algorithm    ## testen ob das so einfach funktioniert oder ob das dann trotzdem k mal ausgeführt wird
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
    result[k, "algorithm"] <- k
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
      result[k, "significanct"] <- TRUE
    } else {
      result[k, "significanct"] <- FALSE
    }
  }
  output_test <- get_results(baseline, measure, method = b_signed_rank$method, 
                             data = result, 
                             extra = list(b_signed_rank$sample))
  return_test <- format_test(output_test)
  return(return_test)
}


#' @title Bayesian hierarchical correlated t-test
#' @description 
#' This function implements a Bayesian hierarchical test.
#' @param df Input data frame. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. 
#' @param measure Measure column. 
#' @param rho Correlation factor. 
#' @param std.upper Factor to set the upper bound for both sigma_i and sigma_0 
#' (see Benavoli \emph{et al.} 2017 for more details)
#' @param d0.lower Lower bound for the prior for mu_0. If not provided, 
#' the smallest observed difference is used
#' @param d0.upper Upper bound for the prior for mu_0. If not provided, 
#' the biggest observed difference is used
#' @param alpha.lower Lower bound for the (uniform) prior for the alpha 
#' hyperparameter (see Benavoli \emph{et al.} 2017 for more details). 
#' Default value set at 0.5, as in the original paper
#' @param alpha.upper Upper bound for the (uniform) prior for the alpha 
#' hyperparameter (see Benavoli \emph{et al.} 2017 for more details). 
#' Default value set at 5, as in the original paper
#' @param beta.lower Lower bound for the (uniform) prior for the beta 
#' hyperparameter (see Benavoli \emph{et al.} 2017 for more details). 
#' Default value set at 0.05, as in the original paper
#' @param beta.lower Upper bound for the (uniform) prior for the beta 
#' hyperparameter (see Benavoli \emph{et al.} 2017 for more details). 
#' Default value set at 0.15, as in the original paper
#' @param z0 Position of the pseudo-observation associated to the prior 
#' Dirichlet Process. The default value is set to 0 (inside the rope)
#' @param rope Interval for the difference considered as 'irrelevant'
#' @param nsim Number of samples (per chain) used to estimate the posterior 
#' distribution. Note that, by default, half the simulations are used for the 
#' burn-in
#' @param nchain Number of MC chains to be simulated. As half the simulations 
#' are used for the warm-up, the total number of simulations will 
#' be \code{nchain}*\code{nsim}/2
#' @param parallel Logical value. If \code{true}, Stan code is executed in 
#' parallel
#' @param stan.output.file String containing the base name for the output files 
#' produced by Stan. If \code{NULL}, no files are stored.
#' @param seed Optional parameter used to fix the random seed
#' @param ... Additional arguments for the rstan::stan function that runs the 
#' analysis 
#' @return A list with the following elements: 
#' \item{\code{method}}{a string with the name of the method used}
#' \item{\code{parameters}}{parameters used by the method}
#' \item{\code{posterior.probabilities}}{a vector with the left, rope and right 
#' probabilities}
#' \item{\code{approximated}}{a logical value, \code{TRUE} if the posterior 
#' distribution is approximated (sampled) and \code{FALSE} if it is exact}
#' \item{\code{posterior}}{Sampled probabilities (see details)}
#' \item{\code{additional}}{Additional information provided by the model. 
#' This includes:\code{per.dataset}, the results per dataset (left, rope and 
#' right probabilities together with the expected mean value); \code{global.sin} 
#' sampled probabilities of mu_0 being positive or negative and 
#' \code{stan.results}, the complete set of results produced by Stan program}
#' @details 
#' The results includes the typical information relative to the three areas of 
#' the posterior density (left, right and rope probabilities), both global and
#' per dataset (in the additional information). Also, the simulation results are 
#' included.
#' As for the prior parameters, they are set to the default values indicated in 
#' Benavoli \emph{et al.} 2017, except for the bound for the prior distribution 
#' of mu_0, which are set to the maximum and minimum values observed in the 
#' sample. You should not modify them unless you know what you are doing.
#' @example results <- b_hierarchical_test(df= test_benchmark_small, 
#'                                         baseline = 'algo_1', 
#'                                         algorithm = 'algo_2', 
#'                                         rho=0.1, rope=c(-0.01, 0.01), 
#'                                         nsim=2000,  nchains=5)
#' @references \url{https://github.com/b0rxa/scmamp}
#' @export
b_hierarchical_test <- function(df, baseline, algorithm = NULL,  measure = NULL, 
                                rho = 0.1, compare = NULL, std.upper = 1000,
                                d0.lower = NULL, d0.upper = NULL, 
                                alpha.lower = 0.5, alpha.upper = 5, 
                                beta.lower = 0.05, beta.upper = 0.15, 
                                rope = c(-0.01, 0.01), nsim = 2000, 
                                parallel = TRUE, stan.output.file = NULL, 
                                nchains = 8, seed = as.numeric(Sys.time())) {
  result <- data.frame()
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, baseline, algorithm = NULL, 
                                     measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algorithms <- unique(df[["algorithm"]])
  for (k in algorithms[algorithms != baseline]) {
    if (!is.null(algorithm)) {
      k <- algorithm    ## testen ob das so einfach funktioniert oder ob das dann trotzdem k mal ausgeführt wird
    }
    # define samples
    x.matrix <- data_transformation(df, algo = baseline, measure)
    y.matrix <- data_transformation(df, algo = k, measure)
    # check numbers in sample
    checkmate::assert_true(get_replications_count(x.matrix, y.matrix))
    # Bayesian correlated t Test
    b_hierarchical <- 
      scmamp::bHierarchicalTest(x.matrix, y.matrix, rho, std.upper, 
                                d0.lower, d0.upper, alpha.lower, alpha.upper, 
                                beta.lower, beta.upper, rope, nsim, nchains, 
                                parallel, stan.output.file, seed)
    # results
    result[k, "algorithm"] <- k
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
      result[k, "significanct"] <- TRUE
    } else {
      result[k, "significanct"] <- FALSE
    }
  }
  output_test <- get_results(baseline, measure, method = b_hierarchical$method, 
                             data = result, replications = i)
  return_test <- format_test(output_test)
  return(return_test)
}

