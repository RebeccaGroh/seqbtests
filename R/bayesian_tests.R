#' @title Bayesian correlated t test 
#' @description 
#'     This function implements the Bayesian version of the correlated t-test. 
#'     The performance of one baseline algorithm on one data set is compared to 
#'     either one or multiple algorithms.  
#' @param df (`list`)\cr Data frame containing the performane measure. 
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
#' @param prob (`double`)\cr Threshold probability that decision rely on. 
#'     Default is 0.95. 
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
#'     \item{\code{probabilities}} (`character`)\cr Decisions based on posterior 
#'         probabilities and threshold probability. 
#' }
#' @details
#'     The test has first been implemented in scmamp.
#'     If rho equals 0 this converts the test in the equivalent of the 
#'     standard t test.   
#' @references \url{https://github.com/b0rxa/scmamp}
#' @examples 
#' results <- b_corr_t_test(df= test_benchmark_small, problem = "problem_a", 
#'                          baseline = "algo_1", algorithm = "algo_2")
#' @export
b_corr_t_test <- function(df, problem, baseline, algorithm = NULL, 
    measure = NULL, compare = NULL, rho = 0.1, rope = c(-0.01, 0.01), 
    prob = 0.95) {
  result <- data.frame()
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problem, baseline, algorithm, measure))
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
    y <- df[df[["problem"]] == problem & df[["algorithm"]] == k, measure]
    # Bayesian correlated t Test
    b_corr <- scmamp::bCorrelatedTtest(x, y, rho, rope)
    # results
    test_result <- get_data_frame(k = k, 
      posterior = b_corr$posterior.probabilities)
    result <- rbind(result, test_result)
  }
  if (is.null(compare)) {compare <- "better"}
  result <- get_probabilities(result, compare, prob)
  output <- get_results(baseline, measure, method = b_corr$method, 
    data = result, extra = get_extras_scmamp(b_corr))
  return(output)
}

# results <- b_corr_t_test(df= test_benchmark_small, problem = "problem_a",
#                        baseline = "algo_1", compare = "equal")
# results


#' @title Bayesian Sign test 
#' @description 
#'     This function implements the Bayesian version of the sign test. The 
#'     performance of one baseline algorithm on one or multiple data sets is 
#'     compared to either one or multiple algorithms.   
#' @param df (`list`)\cr Data frame containing the performane measure. 
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
#' @param prob (`double`)\cr Threshold probability that decision rely on. 
#'     Default is 0.95. 
#' @param s (`double`)\cr Scale parameter of the prior Dirichlet Process. 
#'     Default is 0.5
#' @param z_0 (`double`)\cr Position of the pseudo-observation associated to 
#'     the prior Dirichlet Process. Default is 0. 
#' @param weights (`any`)\cr A prior weights.  
#' @param mc_samples (`double`)\cr Number of samples used to estimate the 
#'     posterior probability distribution. 
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
#'     \item{\code{probabilities}} (`character`)\cr Decisions based on posterior 
#'         probabilities and threshold probability. 
#' }
#' @details 
#'     The test has first been implemented in rNPBST. 
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @examples
#'     results <- b_sign_test(df= test_benchmark_small, 
#'     problem = "problem_a", baseline = "algo_1", algorithm = "algo_2")
#' @export
b_sign_test <- function(df, problem, baseline, algorithm = NULL, 
  measure = NULL, compare = NULL, prob = 0.95, s = 1, z_0 = 0, 
  weights = c(s/2, rep(1, length(x))), mc_samples = 1e+05, 
  rope = c(-0.01, 0.01)) {
  result <- data.frame()
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the 
      rope (min, max), but the values are not orderd. They will be swapped to 
      follow with the procedure")
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problem = NULL, baseline, algorithm, measure))
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
      sum_data <- stats::aggregate(data_wide[, c(baseline, k)], 
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
    test_result <- get_data_frame(k = k, posterior =  b_sign$probabilities)
    result <- rbind(result, test_result)
  }
  if (is.null(compare)) {compare <- "better"}
  result <- get_probabilities(result, compare, prob)
  output <- get_results(baseline, measure, method = b_sign$method, 
    data = result, extra = list(b_sign$sample))
  return(output)
}

# results <- b_sign_test(df= test_benchmark_small,
#                        baseline = "algo_1", compare = "equal")
# results

#' @title Bayesian Signed Rank test 
#' @description 
#'     This function implements the Bayesian version of the signed rank test. 
#'     The performance of one baseline algorithm on one or multiple data sets is 
#'     compared to either one or multiple algorithms.    
#' @param df (`list`)\cr Data frame containing the performane measure. 
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
#' @param prob (`double`)\cr Threshold probability that decision rely on. 
#'     Default is 0.95. 
#' @param s (`double`)\cr Scale parameter of the prior Dirichlet Process. 
#'     Default is 0.5
#' @param z_0 (`double`)\cr Position of the pseudo-observation associated to 
#'     the prior Dirichlet Process. Default is 0. 
#' @param weights (`any`)\cr A prior weights.  
#' @param mc_samples (`double`)\cr Number of samples used to estimate the 
#'     posterior probability distribution. 
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
#'     \item{\code{probabilities}} (`character`)\cr Decisions based on posterior 
#'         probabilities and threshold probability. 
#' }
#' @details 
#'     The test has first been implemented in rNPBST. 
#' @references \url{https://github.com/JacintoCC/rNPBST}
#' @examples 
#'     results <- b_signed_rank_test(df= test_benchmark_small,
#'     baseline = "algo_1", algorithm = "algo_2")
#' @export
b_signed_rank_test <- function(df, problem = NULL, baseline, compare = NULL,
  algorithm = NULL, measure = NULL, prob = 0.95, s = 0.5, z_0 = 0, 
  weights = NULL, mc_samples = 1e+05, rope = c(-0.01, 0.01)) {
  result <- data.frame()
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the rope 
      (min, max), but the values are not orderd. They will be swapped to follow 
      with the procedure")
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problem  = NULL, baseline, algorithm, measure))
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
      sum_data <- stats::aggregate(data_wide[, c(baseline, k)], 
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
      rope.min, rope.max, weights, mc.samples)
    # results
    test_result <- get_data_frame(k = k, 
      posterior = b_signed_rank$probabilities)
    result <- rbind(result, test_result)
  }
  if (is.null(compare)) {compare <- "better"}
  result <- get_probabilities(result, compare, prob)
  output <- get_results(baseline, measure, method = b_signed_rank$method, 
    data = result, extra = list(b_signed_rank$sample))
  return(output)
}

# results <- b_signed_rank_test(df= test_benchmark_small,
#                               baseline = "algo_1", compare = "equal")
# results

#' @title Bayesian hierarchical correlated t-test
#' @description 
#'     This function implements a Bayesian hierarchical test. The performance of 
#'     one baseline algorithm on multiple data set is compared to either one or
#'     multiple algorithms.  
#' @param df (`list`)\cr Data frame containing the performane measure. 
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
#' @param rho (`double`)\cr Correlation factor. Default is 0.1.
#' @param prob (`double`)\cr Threshold probability that decision rely on. 
#'     Default is 0.95. 
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
#'     \item{\code{probabilities}} (`character`)\cr Decisions based on posterior 
#'         probabilities and threshold probability. 
#' }
#' @examples 
#'     results <- b_hierarchical_test(df= test_benchmark_small, 
#'     baseline = "algo_1", algorithm = "algo_3",  rho=0.1, 
#'     rope=c(-0.01, 0.01), nsim=2000,  nchains=5)
#' @details 
#'     The test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used. The default of rho is 
#'     0.1. 
#' @references \url{https://github.com/b0rxa/scmamp}
#' @export
b_hierarchical_test <- function(df, baseline, algorithm = NULL,  measure = NULL, 
  rho = 0.1, compare = NULL, std.upper = 1000, d0.lower = NULL, d0.upper = NULL, 
  prob = 0.95, alpha.lower = 0.5, alpha.upper = 5, beta.lower = 0.05, 
  beta.upper = 0.15, rope = c(-0.01, 0.01), nsim = 2000, parallel = TRUE, 
  stan.output.file = NULL, nchains = 8, seed = as.numeric(Sys.time()), 
  adapt_delta = 0.8, max_treedepth = 10) {
  result <- data.frame()
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, baseline, algorithm, measure, 
    problem = NULL))
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
    checkmate::assert_true(get_replication_count(x.matrix, y.matrix))
    # Bayesian correlated t Test
    b_hierarchical <- scmamp::bHierarchicalTest(x.matrix, y.matrix, rho, 
      std.upper, d0.lower, d0.upper, alpha.lower, alpha.upper, beta.lower, 
      beta.upper, rope, nsim, nchains, parallel, stan.output.file, seed, 
      control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth))
    # results
    test_result <- get_data_frame(k = k, 
      posterior = b_hierarchical$posterior.probabilities)
    result <- rbind(result, test_result)
  }
  if (is.null(compare)) {compare <- "better"}
  result <- get_probabilities(result, compare, prob)
  output <- get_results(baseline, measure, method = b_hierarchical$method, 
    data = result, get_extras_scmamp(b_hierarchical))
  return(output)
}

# results <- b_hierarchical_test(df= test_benchmark_small, baseline = "algo_1",
#   rho=0.1, rope=c(-0.01, 0.01), nsim=2000,  nchains=5)
# results


