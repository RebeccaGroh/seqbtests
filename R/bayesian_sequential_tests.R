#' @title Sequential Bayesian correlated t test 
#' @description 
#'     This function implements a sequential approach of the Bayesian correlated 
#'     t test to compare the performance of machine learning algorithms to one 
#'     another. Sample size is not fixed in advance, data are evaluated as they 
#'     are collected. Further sampling is stopped in accordance with a 
#'     pre-defined stopping rule as soon as significant results are obtained.  
#' @param problem Problem set on which the test should be performed. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. If not defined, every algorithm will be 
#'     tested against baseline. 
#' @param measure Measure column. 
#' @param compare Defines whether the baseline should be tested for either 
#'     being better ('better') or being just as good ('equal') as the other 
#'     algorithms. If not defined, the default is to test for 'better'.
#' @param rho Correlation factor. 
#' @param rope Region of practical equivalence. 
#' @param max_repls Maximum number of replications that should be build in 
#'     get_replications. Or maximum number of replications in data frame, if 
#'     a data frame that has already been built is being used.  
#' @param rope Region of practical equivalence.
#' @param prob Probability, which the decision that the Baseline is better than 
#'     the algorithm is based on. The default is 0.95. 
#'  @param min_repls The minimum number of replications that is either generated 
#'      or used to perform the first Bayesian test in the sequential approach. 
#'      To prevent an bias through early stopping a minimum number of 5 runs 
#'      (default) is recommended. 
#' @param ... Additional arguments. When already built dataset is used the User 
#'     can define it with code{df}.  
#' @return A list containing the following components:
#' \itemize{
#'     \item{code{measure}} A string with the name of the measure column used.
#'     \item{code{method}} A string with the name of the method used.
#'     \item{code{baseline}} A string with the name of the baseline algorithm.
#'     \item{code{posteriror_probabilities}} A data frame with one row for every 
#'         algorithm that is compared to the baseline. The columns show the 
#'         posterior probabilities and whether significance appears.
#'     \item{code{repls}} Number of the considered replications.
#' }
#' @details 
#'     The basis for this test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used. The default of rho is 
#'     0.1. 
#' @references \url{https://github.com/b0rxa/scmamp}
#' @examples
#'     results <- seq_b_corr_t_test(df = test_benchmark_small, rho=0.1,
#'     problem = "problem_a", baseline = "algo_1", 
#'     compare = "equal", max_repls = 10)
#' @export
seq_b_corr_t_test <- function(problem, baseline, algorithm = NULL, 
  measure = NULL, compare = NULL, rho = 0.1, rope = c(-0.01, 0.01), 
  max_repls = 20, prob = 0.95, min_repls = 5, ...) {
  result <- data.frame()
  for (i in min_repls:max_repls) {
    data <- get_replications(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, problem, baseline, algorithm, 
      measure))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    # define samples
    x <- data[data[["problem"]] == problem 
      & data[["algorithm"]] == baseline, measure]
    algorithms <- unique(data[["algorithm"]])
    if (i == min_repls) {
      liste <- c()
    }
    if (!is.null(algorithm)) {
      algorithms <- algorithm
    }
    algorithms <- setdiff(algorithms, liste)
    for (k in algorithms[algorithms != baseline]) {
      y <- data[data[["problem"]] == problem 
        & data[["algorithm"]] == k, measure]
      # Bayesian correlated t Test
      b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
      test_result <- get_data_frame_seq(k = k, 
        posterior = b_test$posterior.probabilities, repls = i)
      result <- rbind(result, test_result)
      if (is.null(compare)) {compare <- "better"}
      thresholds <- get_threshold(compare = compare, 
        posterior = b_test$posterior.probabilities)
      if (thresholds[1] > prob | thresholds[2] > prob | thresholds[3] > prob) {
        liste <- rbind(liste, k)
      }
    }
  }
  result <- get_rows(result = result)
  result <- get_probabilities(result, compare, prob)
  output <- get_results(baseline, measure, method = b_test$method,
    data = result)
  return(output)
}

# results <- seq_b_corr_t_test(df = test_benchmark_small, rho=0.1,
#                              problem = "problem_b", baseline = "algo_1",
#                              compare = "better", max_repls = 10, min_repls = 5)
# results

#' @title Sequential Bayesian Sign test 
#' @description 
#'     This function implements a sequential approach of the Bayesian version of
#'     the sign test to compare the performance of machine learning algorithms 
#'     to one another. Sample size is not fixed in advance, data are evaluated 
#'     as they are collected. Further sampling is stopped in accordance with a 
#'     pre-defined stopping rule as soon as significant results are obtained. 
#' @param problem Problem set on which the test should be performed. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. If not defined, every algorithm will be 
#'     tested against baseline.  
#' @param measure Measure column. 
#' @param compare Defines whether the baseline should be tested for either 
#'     being better ('better') or being just as good ('equal') as the other 
#'     algorithms. If not defined, the default is to test for 'better'.
#' @param z_0 Prior pseudo-observation. 
#' @param s Prior pseudo-observation probability. 
#' @param weights A-priori weights. 
#' @param mc_samples Number of samples of the distribution. 
#' @param rope Region of practical equivalence. 
#' @param max_repls Maximum number of replications that should be build in 
#'     get_replications. Or maximum number of replications in data frame, if 
#'     a data frame that has already been built is being used.  
#' @param ... Additional arguments. When already built dataset is used the User 
#'     can define it with code{df}.
#' @param prob Probability, which the decision that the Baseline is better than 
#'     the algorithm is based on. The default is 0.95. 
#'  @param min_repls The minimum number of replications that is either generated 
#'      or used to perform the first Bayesian test in the sequential approach. 
#'      To prevent an bias through early stopping a minimum number of 5 runs 
#'      (default) is recommended. 
#' @return A list containing the following components:
#' \itemize{
#'     \item{code{measure}} A string with the name of the measure column used.
#'     \item{code{method}} A string with the name of the method used.
#'     \item{code{baseline}} A string with the name of the baseline algorithm.
#'     \item{code{posteriror_probabilities}} A data frame with one row for every 
#'         algorithm that is compared to the baseline. The columns show the 
#'         posterior probabilities and whether significance appears.
#'     \item{code{repls}} Number of the considered replications.
#' }
#' @details 
#'     The basis for this test has first been implemented in rNPBST. For testing
#'     over multiple datasets, don´t specify the problem set argument in the 
#'     function. Note that if no measure column is defined per default the first
#'     column defined as measure_* in the data frame is used. 
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @examples     
#'     results <- seq_b_sign_test(df = test_benchmark_small, 
#'     baseline = "algo_1", max_repls = 10)
#' @export
seq_b_sign_test <- function(problem = NULL, baseline, algorithm = NULL, 
  measure = NULL, compare = NULL, s = 1, z_0 = 0, rope = c(-0.01, 0.01),
  weights = c(s/2, rep(1, length(x))), mc_samples = 1e+05, max_repls = 20, 
  prob = 0.95, min_repls = 5, ...) {
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the rope 
      (min, max), but the values are not orderd. They will be swapped to follow 
      with the procedure")
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  result <- data.frame()
  for (i in min_repls:max_repls) {
    data <- get_replications(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, problem = NULL, baseline, 
      algorithm, measure))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    ## alle "algorithm" mit k ersetzen? 
    algorithms <- unique(data[["algorithm"]])
    if (i == min_repls) {
      liste <- c()
    }
    if (!is.null(algorithm)) {
      algorithms <- algorithm
    }
    algorithms <- setdiff(algorithms, liste)
    for (k in algorithms[algorithms != baseline]) {
      # define samples when testing on multiple datasets
      if (is.null(problem)) {
        data_wide <- tidyr::spread(data, algorithm, measure)
        sum_data <- aggregate(data_wide[, c(baseline, k)], 
          by = list(data_wide[["problem"]]), FUN = mean)
        x <- sum_data[, baseline]
        y <- sum_data[, k]
      } else {
        # define samples when testing on a single dataset
        x <- data[data[["problem"]] == problem 
          & data[["algorithm"]] == baseline, measure]
        y <- data[data[["problem"]] == problem 
          & data[["algorithm"]] == k, measure]
      }
      n.samples <- mc_samples
      # Bayesian Sign Test
      b_sign <- rNPBST::bayesianSign.test(x, y, s, z_0, rope.min, rope.max, 
        weights, n.samples)
      test_result <- get_data_frame_seq(k = k, 
        posterior = b_sign$probabilities, repls = i)
      result <- rbind(result, test_result)
      if (is.null(compare)) {compare <- "better"}
      thresholds <- get_threshold(compare = compare, 
        posterior = b_sign$probabilities)
      if (thresholds[1] > prob | thresholds[2] > prob | thresholds[3] > prob) {
        liste <- rbind(liste, k)
      }
    }
  }
  result <- get_rows(result = result)
  result <- get_probabilities(result, compare, prob)
  output <- get_results(baseline, measure, method = b_sign$method, 
    data = result)
  return(output)
}

# results_test <- seq_b_sign_test(df = test_benchmark_small,
#                            baseline = "algo_1", max_repls = 10)
# results_test


#' @title Sequential Bayesian Signed Rank test 
#' @description 
#'     This function implements a sequential approach of the Bayesian version of
#'     the signed rank test to compare the performance of machine learning 
#'     algorithms to one another. Sample size is not fixed in advance, data are 
#'     evaluated as they are collected. Further sampling is stopped in 
#'     accordance with a pre-defined stopping rule as soon as significant 
#'     results are obtained. 
#' @param problem Problem set on which the test should be performed. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. If not defined, every algorithm will be 
#'     tested against baseline. 
#' @param measure Measure column. 
#' @param compare Defines whether the baseline should be tested for either 
#'     being better ('better') or being just as good ('equal') as the other 
#'     algorithms. If not defined, the default is to test for 'better'.
#' @param z_0 Prior pseudo-observation. 
#' @param s Prior pseudo-observation probability. 
#' @param weights A-priori weights. 
#' @param mc_samples Number of samples of the distribution. 
#' @param rope Region of practical equivalence. 
#' @param max_repls Maximum number of replications that should be build in 
#'     get_replications. Or maximum number of replications in data frame, if 
#'     a data frame that has already been built is being used.  
#' @param ... Additional arguments. When already built dataset is used the User 
#'     can define it with code{df}.
#' @param prob Probability, which the decision that the Baseline is better than 
#'     the algorithm is based on. The default is 0.95. 
#'  @param min_repls The minimum number of replications that is either generated 
#'      or used to perform the first Bayesian test in the sequential approach. 
#'      To prevent an bias through early stopping a minimum number of 5 runs 
#'      (default) is recommended. 
#' @return A list containing the following components:
#' \itemize{
#'     \item{code{measure}} A string with the name of the measure column used.
#'     \item{code{method}} A string with the name of the method used.
#'     \item{code{baseline}} A string with the name of the baseline algorithm.
#'     \item{code{posteriror_probabilities}} A data frame with one row for every 
#'         algorithm that is compared to the baseline. The columns show the 
#'         posterior probabilities and whether significance appears.
#'     \item{code{repls}} Number of the considered replications.
#' }
#' @details 
#'     The basis for this test has first been implemented in rNPBST. For testing
#'     over multiple datasets, don´t specify the problem set argument in the 
#'     function. Note that if no measure column is defined per default the first
#'     column defined as measure_* in the data frame is used. 
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @examples     
#'     results <- seq_b_signed_rank_test(df = test_benchmark_small, 
#'     baseline = 'algo_1', max_repls = 10)
#' @export
seq_b_signed_rank_test <- function(problem = NULL, baseline, 
  algorithm = NULL, measure = NULL, compare = NULL, s = 0.5, z_0 = 0,
  weights = NULL, mc_samples = 1e+05, rope = c(-0.01, 0.01), max_repls = 20, 
  prob = 0.95, min_repls = 5, ...) {
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the rope 
      (min, max), but the values are not orderd. They will be swapped to follow 
      with the procedure")
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  result <- data.frame()
  for (i in min_repls:max_repls) {
    data <- get_replications(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, problem = NULL, baseline, 
      algorithm, measure))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    ## alle "algorithm" mit k ersetzen? 
    algorithms <- unique(data[["algorithm"]])
    if (i == min_repls) {
      liste <- c()
    }
    if (!is.null(algorithm)) {
      algorithms <- algorithm
    }
    algorithms <- setdiff(algorithms, liste)
    for (k in algorithms[algorithms != baseline]) {
      # define samples when testing on multiple datasets
      if (is.null(problem)) {
        data_wide <- tidyr::spread(data, algorithm, measure)
        sum_data <- aggregate(data_wide[, c(baseline, k)], 
          by = list(data_wide[["problem"]]), FUN = mean)
        x <- sum_data[, baseline]
        y <- sum_data[, k]
      } else {
        # define samples when testing on a single dataset
        x <- data[data[["problem"]] == problem 
          & data[["algorithm"]] == baseline, measure]
        y <- data[data[["problem"]] == problem 
          & data[["algorithm"]] == k, measure]
      }
      mc.samples <- mc_samples
      # Bayesian Sign Test
      b_signed_rank <- rNPBST::bayesianSignedRank.test(x, y, s, z_0, 
        rope.min, rope.max, weights, mc.samples)
      test_result <- get_data_frame_seq(k = k, 
        posterior = b_signed_rank$probabilities, repls = i)
      result <- rbind(result, test_result)
      if (is.null(compare)) {compare <- "better"}
      thresholds <- get_threshold(compare = compare, 
        posterior = b_signed_rank$probabilities)
      if (thresholds[1] > prob | thresholds[2] > prob | thresholds[3] > prob) {
        liste <- rbind(liste, k)
      }
    }
  }
  result <- get_rows(result = result)
  result <- get_probabilities(result, compare, prob)
  output <- get_results(baseline, measure, method = b_signed_rank$method, 
    data = result)
  return(output)
}

# results <- seq_b_signed_rank_test(df = test_benchmark_small,
#                                   baseline = 'algo_1', max_repls = 10, compare = "equal")
# results


#' @title Sequential Bayesian hierarchical correlated t-test
#' @description 
#'     This function implements a sequential approach of the Bayesian 
#'     hierarchical test to compare the performance of machine learning 
#'     algorithms to one another. Sample size is not fixed in advance, data are 
#'     evaluated as they are collected. Further sampling is stopped in 
#'     accordance with a pre-defined stopping rule as soon as significant 
#'     results are obtained. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. If not defined, every algorithm will be 
#'     tested against baseline. 
#' @param measure Measure column. 
#' @param compare Defines whether the baseline should be tested for either 
#'     being better ('better') or being just as good ('equal') as the other 
#'     algorithms. If not defined, the default is to test for 'better'.
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
#' @param max_repls Maximum number of replications that should be build in 
#'     get_replications. Or maximum number of replications in data frame, if 
#'     a data frame that has already been built is being used.  
#' @param rope Region of practical equivalence. 
#' @param ... Additional arguments. When already built dataset is used the User 
#'     can define it with code{df}.
#' @param prob Probability, which the decision that the Baseline is better than 
#'     the algorithm is based on. The default is 0.95. 
#'  @param min_repls The minimum number of replications that is either generated 
#'      or used to perform the first Bayesian test in the sequential approach. 
#'      To prevent an bias through early stopping a minimum number of 5 runs 
#'      (default) is recommended. 
#' @return A list containing the following components:
#' \itemize{
#'     \item{code{measure}} A string with the name of the measure column used.
#'     \item{code{method}} A string with the name of the method used.
#'     \item{code{baseline}} A string with the name of the baseline algorithm.
#'     \item{code{posteriror_probabilities}} A data frame with one row for every 
#'         algorithm that is compared to the baseline. The columns show the 
#'         posterior probabilities and whether significance appears.
#'     \item{code{repls}} Number of the considered replications.
#' }
#' @details 
#'     The basis for this test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used. The default of rho is 
#'     0.1. 
#' @references \url{https://github.com/b0rxa/scmamp}
#' @examples 
#'     results <- seq_b_hierarchical_test(df = test_benchmark_small,
#'     baseline = 'algo_1', max_repls = 10)
#' @export
seq_b_hierarchical_test <- function(baseline, algorithm = NULL, measure = NULL, 
  compare = NULL, rho = 0.1, max_repls = 20, rope = c(-0.01, 0.01), 
  std.upper = 1000, d0.lower = NULL, d0.upper = NULL, alpha.lower = 0.5, 
  alpha.upper = 5, beta.lower = 0.05, beta.upper = 0.15, nsim = 2000, 
  nchains = 8, parallel = TRUE, stan.output.file = NULL, prob = 0.95, 
  seed = as.numeric(Sys.time()), min_repls = 5, adapt_delta = 0.8, 
  max_treedepth = 10, ...) {
  result <- data.frame()
  for (i in min_repls:max_repls) {
    data <- get_replications(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, baseline, algorithm, 
      measure, problem = NULL))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    algorithms <- unique(data[["algorithm"]])
    if (i == min_repls) {
      liste <- c()
    }
    if (!is.null(algorithm)) {
      algorithms <- algorithm
    }
    algorithms <- setdiff(algorithms, liste)
    # define samples
    x.matrix <- data_transformation(data, algo = baseline, measure)
    for (k in algorithms[algorithms != baseline]) {
        y.matrix <- data_transformation(data, algo = k, measure)
      # check numbers in sample
      checkmate::assert_true(get_replications_count(x.matrix, y.matrix))
      # Bayesian correlated t Test
      b_hierarchical <- scmamp::bHierarchicalTest(x.matrix, y.matrix, rho, 
        std.upper, d0.lower, d0.upper, alpha.lower, alpha.upper, beta.lower,
        beta.upper, rope, nsim, nchains, parallel, stan.output.file, seed, 
        control = list(adapt_delta = adapt_delta, 
          max_treedepth = max_treedepth))
      test_result <- get_data_frame_seq(k = k, 
        posterior = b_hierarchical$posterior.probabilities, repls = i)
      result <- rbind(result, test_result)
      if (is.null(compare)) {
        compare <- "better"
      }
      thresholds <- get_threshold(compare = compare, 
        posterior = b_hierarchical$posterior.probabilities)
      if (thresholds[1] > prob | thresholds[2] > prob | thresholds[3] > prob) {
        liste <- rbind(liste, k)
      }
    }
  }
  result <- get_rows(result = result)
  result <- get_probabilities(result, compare, prob)
  output <- get_results(baseline, measure, method = b_hierarchical$method, 
    data = result)
  return(output)
}

# results <- seq_b_hierarchical_test(df = test_benchmark_small,
#   baseline = 'algo_1', max_repls = 10, adapt_delta = 0.9999)
# results









