#' @title Sequential Bayesian correlated t-test 
#' @description 
#'     This function implements a sequential approach of the Bayesian correlated 
#'     t-test to compare the performance of machine learning algorithms to one 
#'     another. Sample size is not fixed in advance, data are evaluated as they 
#'     are collected. Further sampling is stopped in accordance with a 
#'     pre-defined stopping rule.  
#' @param problem (`character`)\cr Problem set used to evaluate the algorithms 
#'     performance. Value in 'problem' column. 
#' @param baseline (`character`)\cr First algorithm. Value in 'algorithm'  
#'     column. 
#' @param algorithm (`character`)\cr Second algorithm. Value in 'algorithm' 
#'     column. If not defined, the baseline is tested against all algorithms 
#'     in the data frame. 
#' @param measure (`character`)\cr Name of the 'measure' column. If not 
#'     defined, the first 'measure' column in the data frame is used. 
#' @param compare (`character`)\cr Defines if one algorithm needs to perform 
#'     better ({\code{better}}) for decisions based on the posterior 
#'     distribution or whether it is sufficient to perform not worse 
#'     ({\code{equal}}). 
#' @param rho (`double`)\cr Correlation factor. Default is 0.1.
#' @param rope (`double`)\cr Region of practical equivalence. Default is 
#'     c(-0.01, 0.01).
#' @param max_repls (`double`)\cr Maximum number of replications. If a complete 
#'     data frame is passed ({\code{max_repls}}) should correspond to the 
#'     maximum number of replications that are built. Default is 20.
#' @param prob (`double`)\cr Threshold probability that decision rely on. 
#'     Default is 0.95. 
#' @param min_repls (`double`)\cr Minimum number of replications that is 
#'     used/generated before an optional stopping rule is activated. Default is 
#'     5.
#' @param ... (any)\cr Additional arguments for ({\code{get_replication}}). 
#'     To pass a complete data frame, set ({\code{df}}) (`character`). 
#' @return (`list`)\cr A list containing the following components:
#' \itemize{
#'     \item{\code{measure}} (`character`)\cr A string with the name of the 
#'         measure column. 
#'     \item{\code{method}} (`character`)\cr A string with the name of the 
#'         method. 
#'     \item{\code{baseline}} (`character`)\cr A string with the name of the 
#'         first algorithm. Value in 'algorithm' column. 
#'     \item{\code{data_frame}} (`list`)\cr  A list containing the following 
#'         components:
#'     \item{\code{algorithm}} (`character`)\cr Second algorithm. Value in 
#'         'algorithm' column. If not defined, the baseline is tested against 
#'         all algorithms in the data frame. 
#'     \item{\code{left}} (`double`)\cr Left probability. 
#'     \item{\code{rope}} (`double`)\cr Rope probability. 
#'     \item{\code{right}} (`double`)\cr Right probability. 
#'     \item{\code{repls}} (`double`)\cr Number of evaluated replications. 
#'     \item{\code{probabilities}} (`character`)\cr Decisions based on posterior 
#'         probabilities and threshold probability. 
#' }
#' @details 
#'     The basis for this test has first been implemented in scmamp.  
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
    data <- get_replication(i, ...)
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
      term_algos <- c()
    }
    if (!is.null(algorithm)) {
      algorithms <- algorithm
    }
    algorithms <- setdiff(algorithms, term_algos)
    for (k in algorithms[algorithms != baseline]) {
      y <- data[data[["problem"]] == problem 
        & data[["algorithm"]] == k, measure]
      # Bayesian correlated t-test
      b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
      test_result <- get_data_frame_seq(k = k, 
        posterior = b_test$posterior.probabilities, repls = i)
      result <- rbind(result, test_result)
      if (is.null(compare)) {compare <- "better"}
      thresholds <- get_threshold(compare = compare, 
        posterior = b_test$posterior.probabilities)
      if (thresholds[1] > prob | thresholds[2] > prob | thresholds[3] > prob) {
        term_algos <- rbind(term_algos, k)
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
#'     pre-defined stopping rule. 
#' @param problem (`character`)\cr Problem set used to evaluate the algorithms 
#'     performance. Value in 'problem' column. 
#' @param baseline (`character`)\cr First algorithm. Value in 'algorithm'  
#'     column. 
#' @param algorithm (`character`)\cr Second algorithm. Value in 'algorithm' 
#'     column. If not defined, the baseline is tested against all algorithms 
#'     in the data frame. 
#' @param measure (`character`)\cr Name of the 'measure' column. If not 
#'     defined, the first 'measure' column in the data frame is used. 
#' @param compare (`character`)\cr Defines if one algorithm needs to perform 
#'     better ({\code{better}}) for decisions based on the posterior 
#'     distribution or whether it is sufficient to perform not worse 
#'     ({\code{equal}}). 
#' @param rope (`double`)\cr Region of practical equivalence. Default is 
#'     c(-0.01, 0.01).
#' @param max_repls (`double`)\cr Maximum number of replications. If a complete 
#'     data frame is passed ({\code{max_repls}}) should correspond to the 
#'     maximum number of replications that are built. Default is 20.
#' @param prob (`double`)\cr Threshold probability that decision rely on. 
#'     Default is 0.95. 
#' @param min_repls (`double`)\cr Minimum number of replications that is 
#'     used/generated before an optional stopping rule is activated. Default is 
#'     5.
#' @param s (`double`)\cr Scale parameter of the prior Dirichlet Process. 
#'     Default is 0.5
#' @param z_0 (`double`)\cr Position of the pseudo-observation associated to 
#'     the prior Dirichlet Process. Default is 0. 
#' @param weights (`any`)\cr A prior weights.  
#' @param mc_samples (`double`)\cr Number of samples used to estimate the 
#'     posterior probability distribution. 
#' @param ... (any)\cr Additional arguments for ({\code{get_replication}}). 
#'     To pass a complete data frame, set ({\code{df}}) (`character`). 
#' @return (`list`)\cr A list containing the following components:
#' \itemize{
#'     \item{\code{measure}} (`character`)\cr A string with the name of the 
#'         measure column. 
#'     \item{\code{method}} (`character`)\cr A string with the name of the 
#'         method. 
#'     \item{\code{baseline}} (`character`)\cr A string with the name of the 
#'         first algorithm. Value in 'algorithm' column. 
#'     \item{\code{data_frame}} (`list`)\cr  A list containing the following 
#'         components:
#'     \item{\code{algorithm}} (`character`)\cr Second algorithm. Value in 
#'         'algorithm' column. If not defined, the baseline is tested against 
#'         all algorithms in the data frame. 
#'     \item{\code{left}} (`double`)\cr Left probability. 
#'     \item{\code{rope}} (`double`)\cr Rope probability. 
#'     \item{\code{right}} (`double`)\cr Right probability. 
#'     \item{\code{repls}} (`double`)\cr Number of evaluated replications. 
#'     \item{\code{probabilities}} (`character`)\cr Decisions based on posterior 
#'         probabilities and threshold probability. 
#' }
#' @details 
#'     The basis for this test has first been implemented in rNPBST. For testing
#'     over multiple datasets, don´t specify the problem set argument in the 
#'     function.
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @examples     
#'     results <- seq_b_sign_test(df = test_benchmark_small, 
#'     baseline = "algo_1", algorithm = "algo_2", max_repls = 10)
#' @export
seq_b_sign_test <- function(problem = NULL, baseline, algorithm = NULL, 
  measure = NULL, compare = NULL, s = 0.5, z_0 = 0, rope = c(-0.01, 0.01),
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
    data <- get_replication(i, ...)
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
      term_algos <- c()
    }
    if (!is.null(algorithm)) {
      algorithms <- algorithm
    }
    algorithms <- setdiff(algorithms, term_algos)
    for (k in algorithms[algorithms != baseline]) {
      # define samples when testing on multiple datasets
      if (is.null(problem)) {
        data_wide <- tidyr::spread(data, algorithm, measure)
        sum_data <- stats::aggregate(data_wide[, c(baseline, k)], 
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
        term_algos <- rbind(term_algos, k)
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
#'     accordance with a pre-defined stopping rule. 
#' @param problem (`character`)\cr Problem set used to evaluate the algorithms 
#'     performance. Value in 'problem' column. 
#' @param baseline (`character`)\cr First algorithm. Value in 'algorithm'  
#'     column. 
#' @param algorithm (`character`)\cr Second algorithm. Value in 'algorithm' 
#'     column. If not defined, the baseline is tested against all algorithms 
#'     in the data frame. 
#' @param measure (`character`)\cr Name of the 'measure' column. If not 
#'     defined, the first 'measure' column in the data frame is used. 
#' @param compare (`character`)\cr Defines if one algorithm needs to perform 
#'     better ({\code{better}}) for decisions based on the posterior 
#'     distribution or whether it is sufficient to perform not worse 
#'     ({\code{equal}}). 
#' @param rope (`double`)\cr Region of practical equivalence. Default is 
#'     c(-0.01, 0.01).
#' @param max_repls (`double`)\cr Maximum number of replications. If a complete 
#'     data frame is passed ({\code{max_repls}}) should correspond to the 
#'     maximum number of replications that are built. Default is 20.
#' @param prob (`double`)\cr Threshold probability that decision rely on. 
#'     Default is 0.95. 
#' @param min_repls (`double`)\cr Minimum number of replications that is 
#'     used/generated before an optional stopping rule is activated. Default is 
#'     5.
#' @param s (`double`)\cr Scale parameter of the prior Dirichlet Process. 
#'     Default is 0.5
#' @param z_0 (`double`)\cr Position of the pseudo-observation associated to 
#'     the prior Dirichlet Process. Default is 0. 
#' @param weights (`any`)\cr A prior weights.  
#' @param mc_samples (`double`)\cr Number of samples used to estimate the 
#'     posterior probability distribution. 
#' @param ... (any)\cr Additional arguments for ({\code{get_replication}}). 
#'     To pass a complete data frame, set ({\code{df}}) (`character`). 
#' @return (`list`)\cr A list containing the following components:
#' \itemize{
#'     \item{\code{measure}} (`character`)\cr A string with the name of the 
#'         measure column. 
#'     \item{\code{method}} (`character`)\cr A string with the name of the 
#'         method. 
#'     \item{\code{baseline}} (`character`)\cr A string with the name of the 
#'         first algorithm. Value in 'algorithm' column. 
#'     \item{\code{data_frame}} (`list`)\cr  A list containing the following 
#'         components:
#'     \item{\code{algorithm}} (`character`)\cr Second algorithm. Value in 
#'         'algorithm' column. If not defined, the baseline is tested against 
#'         all algorithms in the data frame. 
#'     \item{\code{left}} (`double`)\cr Left probability. 
#'     \item{\code{rope}} (`double`)\cr Rope probability. 
#'     \item{\code{right}} (`double`)\cr Right probability. 
#'     \item{\code{repls}} (`double`)\cr Number of evaluated replications. 
#'     \item{\code{probabilities}} (`character`)\cr Decisions based on posterior 
#'         probabilities and threshold probability. 
#' }
#' @details 
#'     The basis for this test has first been implemented in rNPBST. For testing
#'     over multiple datasets, don´t specify the problem set argument in the 
#'     function.
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @examples     
#'     \dontrun{
#'       results <- seq_b_signed_rank_test(df = test_benchmark_small, 
#'       baseline = 'algo_1', algorithm = "algo_2", max_repls = 10)
#'     }
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
    data <- get_replication(i, ...)
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
      term_algos <- c()
    }
    if (!is.null(algorithm)) {
      algorithms <- algorithm
    }
    algorithms <- setdiff(algorithms, term_algos)
    for (k in algorithms[algorithms != baseline]) {
      # define samples when testing on multiple datasets
      if (is.null(problem)) {
        data_wide <- tidyr::spread(data, algorithm, measure)
        sum_data <- stats::aggregate(data_wide[, c(baseline, k)], 
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
        term_algos <- rbind(term_algos, k)
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
#'     accordance with a pre-defined stopping rule. 
#' @param baseline (`character`)\cr First algorithm. Value in 'algorithm'  
#'     column. 
#' @param algorithm (`character`)\cr Second algorithm. Value in 'algorithm' 
#'     column. If not defined, the baseline is tested against all algorithms 
#'     in the data frame. 
#' @param measure (`character`)\cr Name of the 'measure' column. If not 
#'     defined, the first 'measure' column in the data frame is used. 
#' @param compare (`character`)\cr Defines if one algorithm needs to perform 
#'     better ({\code{better}}) for decisions based on the posterior 
#'     distribution or whether it is sufficient to perform not worse 
#'     ({\code{equal}}). 
#' @param rho (`double`)\cr Correlation factor. Default is 0.1.
#' @param rope (`double`)\cr Region of practical equivalence. Default is 
#'     c(-0.01, 0.01).
#' @param max_repls (`double`)\cr Maximum number of replications. If a complete 
#'     data frame is passed ({\code{max_repls}}) should correspond to the 
#'     maximum number of replications that are built. Default is 20.
#' @param prob (`double`)\cr Threshold probability that decision rely on. 
#'     Default is 0.95. 
#' @param min_repls (`double`)\cr Minimum number of replications that is 
#'     used/generated before an optional stopping rule is activated. Default is 
#'     5.
#' @param std.upper (`double`)\cr Factor to set the upper bound for both sigma_i 
#'     and sigma_0. Default is 1000. 
#' @param d0.lower (`any`)\cr Lower bound for the prior for mu_0. If not 
#'     provided, the smallest observed difference is used.
#' @param d0.upper (`any`)\cr Upper bound for the prior for mu_0. If not 
#'     provided, the biggest observed difference is used.
#' @param alpha.lower (`double`)\cr Lower bound for the (uniform) prior for the 
#'     alpha hyperparameter. Default is 0.5.
#' @param alpha.upper (`double`)\cr Upper bound for the (uniform) prior for the 
#'     alpha hyperparameter. Default is 0.5.
#' @param beta.lower (`double`)\cr Lower bound for the (uniform) prior for the  
#'     beta hyperparameter. Default is 0.5.
#' @param beta.upper (`double`)\cr Upper bound for the (uniform) prior for the  
#'     beta hyperparameter. Default is 0.5.
#' @param nsim (`double`)\cr Number of samples (per chain) used to estimate the 
#'     posterior distribution. Note that, by default, half the simulations are 
#'     used for the burn-in.
#' @param nchains (`double`)\cr Number of MC chains to be simulated. As half the 
#'     simulations are used for the warm-up, the total number of simulations  
#'     will be \code{nchain}*\code{nsim}/2.
#' @param parallel (`logical`)\cr If \code{true}, Stan code is executed in 
#'     parallel.
#' @param stan.output.file (`character`)\cr String containing the base name for 
#'     the output files produced by Stan. If \code{NULL}, no files are stored.
#' @param seed (`double`)\cr Optional parameter used to fix the random seed.
#' @param adapt_delta (`double`)\cr Average proposal acceptance probability 
#'     during Stan’s adaptation period. 
#' @param max_treedepth (`double`)\cr  Maximum treedepth parameter. 
#' @param ... (any)\cr Additional arguments for ({\code{get_replication}}). 
#'     To pass a complete data frame, set ({\code{df}}) (`character`). 
#' @return (`list`)\cr A list containing the following components:
#' \itemize{
#'     \item{\code{measure}} (`character`)\cr A string with the name of the 
#'         measure column. 
#'     \item{\code{method}} (`character`)\cr A string with the name of the 
#'         method. 
#'     \item{\code{baseline}} (`character`)\cr A string with the name of the 
#'         first algorithm. Value in 'algorithm' column. 
#'     \item{\code{data_frame}} (`list`)\cr  A list containing the following 
#'         components:
#'     \item{\code{algorithm}} (`character`)\cr Second algorithm. Value in 
#'         'algorithm' column. If not defined, the baseline is tested against 
#'         all algorithms in the data frame. 
#'     \item{\code{left}} (`double`)\cr Left probability. 
#'     \item{\code{rope}} (`double`)\cr Rope probability. 
#'     \item{\code{right}} (`double`)\cr Right probability. 
#'     \item{\code{repls}} (`double`)\cr Number of evaluated replications. 
#'     \item{\code{probabilities}} (`character`)\cr Decisions based on posterior 
#'         probabilities and threshold probability. 
#' }
#' @examples 
#'     \dontrun{
#'     results <- seq_b_hierarchical_test(df = test_benchmark_small, 
#'       baseline = "algo_1", algorithm = "algo_3", max_repls = 10, 
#'       min_repls = 8)
#'     }
#' @details 
#'     The basis for this test has first been implemented in scmamp. 
#' @references \url{https://github.com/b0rxa/scmamp}
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
    data <- get_replication(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, baseline, algorithm, 
      measure, problem = NULL))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    algorithms <- unique(data[["algorithm"]])
    if (i == min_repls) {
      term_algos <- c()
    }
    if (!is.null(algorithm)) {
      algorithms <- algorithm
    }
    algorithms <- setdiff(algorithms, term_algos)
    # define samples
    x.matrix <- data_transformation(data, algo = baseline, measure)
    for (k in algorithms[algorithms != baseline]) {
        y.matrix <- data_transformation(data, algo = k, measure)
      # check numbers in sample
      checkmate::assert_true(get_replication_count(x.matrix, y.matrix))
      # Bayesian correlated t-test
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
        term_algos <- rbind(term_algos, k)
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








