#' @title Bayesian correlated t test 
#' @description 
#'     This function implements the Bayesian version of the correlated t-test. 
#'     The performance of one baseline algorithm on one data set is compared to 
#'     either one or multiple algorithms.  
#' @param df Input data frame. 
#' @param problem Problem set on which the test should be performed. 
#' @param baseline First algorithm. 
#' @param algorithm Algorithm to be compared. If no algorithm is defined, the 
#'     baseline is compared to every algorithm in the data frame. 
#' @param measure Measure column. 
#' @param rho Correlation factor. 
#' @param rope Region of practical equivalence. 
#' @param prob Probability, which the decision that the Baseline is better than 
#'     the algorithm is based on. The default is 0.95. 
#' @return A list containing the following components:
#' \itemize{
#'     \item{code{measure}} A string with the name of the measure column used.
#'     \item{code{method}} A string with the name of the method used.
#'     \item{code{baseline}} A string with the name of the baseline algorithm.
#'     \item{code{posteriror_probabilities}} A data frame with one row for every 
#'         algorithm that is compared to the baseline. The columns show the 
#'         posterior probabilities and whether significance appears.
#' }
#' @details
#'     The test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used. The default of rho is 
#'     0.1. If rho equals 0 this converts the test in the equivalent of the 
#'     standard t test.   
#' @references \url{https://github.com/b0rxa/scmamp}
#' @examples 
#' results <- b_corr_t_test(df= test_benchmark_small, problem = "problem_a", 
#'                          baseline = "algo_1", algorithm = "algo_2")
#' @export
b_corr_t_test <- function(df, problem, baseline, algorithm = NULL, 
                          measure = NULL, compare = NULL, rho = 0.1, 
                          rope = c(-0.01, 0.01), prob = 0.95) {
    result <- data.frame()
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
        result[k, "algorithm"] <- k
        result[k, "left"] <- b_corr$posterior.probabilities[1]
        result[k, "rope"] <- b_corr$posterior.probabilities[2]
        result[k, "right"] <- b_corr$posterior.probabilities[3]
        if (is.null(compare)) {compare <- "better"}
        if (compare == "better") { 
            threshold <- b_corr$posterior.probabilities[1]
        } else if (compare == "equal") {
            threshold <- b_corr$posterior.probabilities[2] + 
                b_corr$posterior.probabilities[1]
        } 
        
        if (is.null(prob)) {
          prob <- 0.95
        }
        if (threshold > prob) {
            result[k, "significant"] <- TRUE
        } else {
            result[k, "significant"] <- FALSE
        }
    }
    output <- get_results(baseline, measure, method = b_corr$method, 
                               data = result, 
                               extra = list(b_corr$additional, 
                                            b_corr$approximate, 
                                            b_corr$parameters, 
                                            b_corr$posterior, 
                                            b_corr$additional$pposterior,
                                            b_corr$additional$qposterior,
                                            b_corr$additional$posterior.df,
                                            b_corr$additional$posterior.mean,
                                            b_corr$additional$posterior.sd))
    return(output)
}

# results <- b_corr_t_test(df= test_benchmark_small, problem = "problem_a",
#                        baseline = "algo_1")
# results

#' @title Bayesian Sign test 
#' @description 
#'     This function implements the Bayesian version of the sign test. The 
#'     performance of one baseline algorithm on one or multiple data sets is 
#'     compared to either one or multiple algorithms.   
#' @param df Input data frame. 
#' @param problem Problem set on which the test should be performed. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. If not defined, every algorithm will be 
#'     tested against baseline. 
#' @param measure Measure column. 
#' @param z_0 Prior pseudo-observation. 
#' @param s Prior pseudo-observation probability. 
#' @param weights A-priori weights. 
#' @param mc_samples Number of samples of the distribution. 
#' @param rope Region of practical equivalence. 
#' @param prob Probability, which the decision that the Baseline is better than 
#'     the algorithm is based on. The default is 0.95. 
#' @return A list containing the following components:
#' \itemize{
#'     \item{code{measure}} A string with the name of the measure column used.
#'     \item{code{method}} A string with the name of the method used.
#'     \item{code{baseline}} A string with the name of the baseline algorithm.
#'     \item{code{posteriror_probabilities}} A data frame with one row for every 
#'         algorithm that is compared to the baseline. The columns show the 
#'         posterior probabilities and whether significance appears.
#' }
#' @details 
#'     The test has first been implemented in rNPBST. For testing over multiple 
#'     datasets, don´t specify the problem set argument in the function. Note 
#'     that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used. 
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @examples
#'     results <- b_sign_test(df= test_benchmark_small, 
#'     problem = "problem_a", baseline = "algo_1", algorithm = "algo_2")
#' @export
b_sign_test <- function(df, problem, baseline, algorithm = NULL, 
                        measure = NULL, compare = NULL, prob = 0.95, 
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
  checkmate::assert_true(check_names(df, problem, baseline, algorithm, 
                                     measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algorithms <- unique(df[["algorithm"]])
  if (!is.null(algorithm)) {
      algorithms <- algorithm
  }
  for (k in algorithms[algorithms != baseline]) {
    # define samples when testing on multiple datasets
    if (is.null(problem)) {
      data_wide <- tidyr::spread(df, algorithm, measure)
      sum_data <- aggregate(data_wide[, c(baseline, k)], 
                            by = list(data_wide[["problem"]]), FUN = mean)
      x <- sum_data[, baseline]
      y <- sum_data[, k]
    } else {
      # define samples when testing on a single dataset
      x <- df[df[["problem"]] == problem 
              & df[["algorithm"]] == baseline, measure]
      y <- df[df[["problem"]] == problem 
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
      threshold <- b_sign$probabilities[1]
    } else if (compare == "equal") {
      threshold <- b_sign$probabilities[2] + 
        b_sign$probabilities[1]
    } 
    if (is.null(prob)) {
      prob <- 0.95
    }
    if (threshold > prob) {
      result[k, "significant"] <- TRUE
    } else {
      result[k, "significant"] <- FALSE
    }
  }
  output <- get_results(baseline, measure, method = b_sign$method, 
                             data = result, 
                             extra = list(b_sign$sample))
  return(output)
}


#' @title Bayesian Signed Rank test 
#' @description 
#'     This function implements the Bayesian version of the signed rank test. 
#'     The performance of one baseline algorithm on one or multiple data sets is 
#'     compared to either one or multiple algorithms.    
#' @param df Input data frame. 
#' @param problem Problem set on which the test should be performed. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. If not defined, every algorithm will be 
#'     tested against baseline. 
#' @param measure Measure column. 
#' @param z_0 Prior pseudo-observation. 
#' @param s Prior pseudo-observation probability. 
#' @param weights A-priori weights. 
#' @param mc_samples Number of samples of the distribution. 
#' @param rope Region of practical equivalence. 
#' @param prob Probability, which the decision that the Baseline is better than 
#'     the algorithm is based on. The default is 0.95. 
#' @return A list containing the following components:
#' \itemize{
#'     \item{code{measure}} A string with the name of the measure column used.
#'     \item{code{method}} A string with the name of the method used.
#'     \item{code{baseline}} A string with the name of the baseline algorithm.
#'     \item{code{posteriror_probabilities}} A data frame with one row for every 
#'         algorithm that is compared to the baseline. The columns show the 
#'         posterior probabilities and whether significance appears.
#' }
#' @details 
#'     The test has first been implemented in rNPBST. For testing over multiple 
#'     datasets, don´t specify the problem set argument in the function. Note 
#'     that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used. 
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @examples 
#'     results <- b_signed_rank_test(df= test_benchmark_small,
#'     baseline = "algo_1", algorithm = "algo_2")
#' @export
b_signed_rank_test <- function(df, problem = NULL, baseline, compare = NULL,
                               algorithm = NULL, measure = NULL, prob = 0.95, 
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
  checkmate::assert_true(check_names(df, problem = NULL, baseline, 
                                     algorithm, measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algorithms <- unique(df[["algorithm"]])
  if (!is.null(algorithm)) {
      algorithms <- algorithm
  }
  for (k in algorithms[algorithms != baseline]) {
    # define samples when testing on multiple datasets
    if (is.null(problem)) {
      data_wide <- tidyr::spread(df, algorithm, measure)
      sum_data <- aggregate(data_wide[, c(baseline, k)], 
                            by = list(data_wide[["problem"]]), FUN = mean)
      x <- sum_data[, baseline]
      y <- sum_data[, k]
    } else {
      # define samples when testing on a single dataset
      x <- df[df[["problem"]] == problem 
              & df[["algorithm"]] == baseline, measure]
      y <- df[df[["problem"]] == problem 
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
      threshold <- b_signed_rank$probabilities[1]
    } else if (compare == "equal") {
      threshold <- b_signed_rank$probabilities[2] + 
        b_signed_rank$probabilities[1]
    } 
    if (is.null(prob)) {
      prob <- 0.95
    }
    if (threshold > prob) {
      result[k, "significanct"] <- TRUE
    } else {
      result[k, "significanct"] <- FALSE
    }
  }
  output <- get_results(baseline, measure, method = b_signed_rank$method, 
                             data = result, 
                             extra = list(b_signed_rank$sample))
  return(output)
}


#' @title Bayesian hierarchical correlated t-test
#' @description 
#'     This function implements a Bayesian hierarchical test. The performance of 
#'     one baseline algorithm on multiple data set is compared to either one or
#'     multiple algorithms.  
#' @param df Input data frame. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. If not defined, every algorithm will be 
#'     tested against baseline. 
#' @param measure Measure column. 
#' @param rho Correlation factor. 
#' @param std.upper Factor to set the upper bound for both sigma_i and sigma_0.
#' @param d0.lower Lower bound for the prior for mu_0. If not provided, 
#'     the smallest observed difference is used.
#' @param d0.upper Upper bound for the prior for mu_0. If not provided, 
#'     the biggest observed difference is used.
#' @param alpha.lower Lower bound for the (uniform) prior for the alpha 
#'     hyperparameter. Default value set at 0.5.
#' @param alpha.upper Upper bound for the (uniform) prior for the alpha 
#'     hyperparameter. Default value set at 0.5.
#' @param beta.lower Lower bound for the (uniform) prior for the beta 
#'     hyperparameter. Default value set at 0.5.
#' @param beta.lower Upper bound for the (uniform) prior for the beta 
#'     hyperparameter. Default value set at 0.5.
#' @param z0 Position of the pseudo-observation associated to the prior 
#'     Dirichlet Process. The default value is set to 0 (inside the rope).
#' @param rope Interval for the difference considered as 'irrelevant'.
#' @param nsim Number of samples (per chain) used to estimate the posterior 
#'     distribution. Note that, by default, half the simulations are used for 
#'     the burn-in.
#' @param nchain Number of MC chains to be simulated. As half the simulations 
#'     are used for the warm-up, the total number of simulations will 
#'     be \code{nchain}*\code{nsim}/2.
#' @param parallel Logical value. If \code{true}, Stan code is executed in 
#'     parallel.
#' @param stan.output.file String containing the base name for the output files 
#'     produced by Stan. If \code{NULL}, no files are stored.
#' @param seed Optional parameter used to fix the random seed.
#' @param prob Probability, which the decision that the Baseline is better than 
#'     the algorithm is based on. The default is 0.95. 
#' @return A list containing the following components:
#' \itemize{
#'     \item{code{measure}} A string with the name of the measure column used.
#'     \item{code{method}} A string with the name of the method used.
#'     \item{code{baseline}} A string with the name of the baseline algorithm.
#'     \item{code{posteriror_probabilities}} A data frame with one row for every 
#'         algorithm that is compared to the baseline. The columns show the 
#'         posterior probabilities and whether significance appears.
#' }
#' @details 
#'     The test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used. The default of rho is 
#'     0.1. 
#' @examples 
#'     results <- b_hierarchical_test(df= test_benchmark_small, 
#'     baseline = "algo_1", algorithm = "algo_2",  rho=0.1, 
#'     rope=c(-0.01, 0.01), nsim=2000,  nchains=5)
#' @references \url{https://github.com/b0rxa/scmamp}
#' @export
b_hierarchical_test <- function(df, baseline, algorithm = NULL,  measure = NULL, 
                                rho = 0.1, compare = NULL, std.upper = 1000,
                                d0.lower = NULL, d0.upper = NULL, prob = 0.95, 
                                alpha.lower = 0.5, alpha.upper = 5, 
                                beta.lower = 0.05, beta.upper = 0.15, 
                                rope = c(-0.01, 0.01), nsim = 2000, 
                                parallel = TRUE, stan.output.file = NULL, 
                                nchains = 8, seed = as.numeric(Sys.time())) {
  result <- data.frame()
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, baseline, algorithm = NULL, 
                                     measure = NULL, problem = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algorithms <- unique(df[["algorithm"]])
  if (!is.null(algorithm)) {
      algorithms <- algorithm
  }
  for (k in algorithms[algorithms != baseline]) {
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
      threshold <- b_hierarchical$posterior.probabilities[1]
    } else if (compare == "equal") {
      threshold <- b_hierarchical$posterior.probabilities[2] + 
        b_hierarchical$posterior.probabilities[1]
    } 
    if (is.null(prob)) {
      prob <- 0.95
    }
    if (threshold > prob) {
      result[k, "significanct"] <- TRUE
    } else {
      result[k, "significanct"] <- FALSE
    }
  }
  output <- get_results(baseline, measure, method = b_hierarchical$method, 
                        data = result, 
                        extra = list(b_hierarchical$additional, 
                                     b_hierarchical$approximate, 
                                     b_hierarchical$parameters, 
                                     b_hierarchical$posterior))
  return(output)
}

