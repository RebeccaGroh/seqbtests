#' @title Correlated t test 
#' @description This function implements a two-sided t test for paired samples. 
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
#' @param rho (`double`)\cr Correlation factor. Default is 0.1. (For the case of 
#'     cross validated results, the heuristic to set the correlation is size of 
#'     test set divided by total size of the dataset.) 
#' @return A list containing the following components: 
#' \itemize{
#'     \item{\code{measure}} (`character`)\cr A string with the name of the 
#'         measure column. 
#'     \item{\code{method}} (`character`)\cr A string with the name of the 
#'         method. 
#'     \item{\code{statistic}} (`double`)\cr The value of the statistic used in 
#'         the test.
#'     \item{\code{p_value}} (`double`)\cr The p-value for the test.
#' }
#' @details  
#'     The test has first been implemented in scmamp. 
#' @references \url{https://github.com/b0rxa/scmamp}
#' @examples
#'     results <- corr_t_test(df= test_benchmark_small, 
#'     problem = "problem_a", baseline = "algo_1", algorithm = "algo_2")
#' @export
corr_t_test <- function(df, problem, baseline, algorithm = NULL, 
  measure = NULL, rho = 0.01) {
  result <- data.frame()
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problem, baseline, 
    algorithm = NULL, measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  # define samples
  x <- df[df[["problem"]] == problem & df[["algorithm"]] == baseline, measure]
    algorithms <- unique(df[["algorithm"]])
  if (!is.null(algorithm)) {
    algorithms <- algorithm
  }
  for (k in algorithms[algorithms != baseline]) {
    y <- df[df[["problem"]] == problem & df[["algorithm"]] == k, measure]
    # Correlated t Test
    corr_test <- scmamp::correlatedTtest(x, y, rho, alternative = "two.sided")
    # results 
    result_test <- get_data_frame_htest(k = k, p_value = corr_test$p.value, 
      test = "t = ", statistic = corr_test$statistic)
    result <- rbind(result, result_test)
  }
  output <- get_results_htest(baseline = baseline, measure = measure, 
    method = corr_test$method, data = result)
  class(output) <- "h_test"
  return(output)
}
# results <- corr_t_test(df= test_benchmark_small, 
#                        problem = "problem_a", baseline = "algo_1")
# results

#' @title Friedman's test 
#' @description This function implements the Friedman's test for multiple 
#'     comparisons. A non-parametric statistical test to detect differences in 
#'     in algorithms performances over multiple datasets. 
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @param measure (`character`)\cr Name of the 'measure' column. If not 
#'     defined, the first 'measure' column in the data frame is used. 
#' @return A list containing the following components: 
#' \itemize{
#'     \item{\code{measure}} (`character`)\cr A string with the name of the 
#'         measure column. 
#'     \item{\code{method}} (`character`)\cr A string with the name of the 
#'         method. 
#'     \item{\code{statistic}} (`double`)\cr The value of the statistic used in 
#'         the test.
#'     \item{\code{p_value}} (`double`)\cr The p-value for the test.
#' }
#' @details  
#'     The test has first been implemented in scmamp. 
#' @references \url{https://github.com/b0rxa/scmamp}
#' @examples 
#'     results <- friedman_test(test_benchmark) 
#' @export
friedman_test <- function(df, measure = NULL) {
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algo_names <- unique(df$algorithm)
  data_wide <- tidyr::spread(df, algorithm, measure)
  sum_data <- stats::aggregate(data_wide[, algo_names], 
    by = list(data_wide[["problem"]]), FUN = mean)
  # define dataset
  data <- data.frame(sum_data[, -1], row.names = sum_data[, 1])
  # Friedman Test
  f_test <- scmamp::friedmanTest(data)
  # results 
  result <- get_data_frame_htest_small(p_value = f_test$p.value, 
    test = "Friedman's chi-squared = ", statistic = f_test$statistic)
  output <- get_results_htest(measure = measure, method = f_test$method, 
    data = result)
  class(output) <- "h_test_small"
  return(output)
}
# results <- friedman_test(test_benchmark)
# results

#' @title Wilcoxon signed-rank test 
#' @description 
#'     This function implements the paired Wilcoxon signed-rank test. A 
#'     non-parametric statistical hypothesis test to compare the means of two 
#'     paired samples. 
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
#' @return A list containing the following components: 
#' \itemize{
#'     \item{\code{measure}} (`character`)\cr A string with the name of the 
#'         measure column. 
#'     \item{\code{method}} (`character`)\cr A string with the name of the 
#'         method. 
#'     \item{\code{statistic}} (`double`)\cr The value of the statistic used in 
#'         the test.
#'     \item{\code{p_value}} (`double`)\cr The p-value for the test.
#' }
#' @details  
#'     The test has first been implemented in scmamp. 
#' @references \url{https://github.com/b0rxa/scmamp}
#' @examples 
#'     results <- wilcoxon_signed_test(df = test_benchmark, baseline = "algo_1",
#'     algorithm = "algo_2", problem = "problem_a")  
#' @export
wilcoxon_signed_test <- function(df, problem, baseline, algorithm = NULL, 
  measure = NULL) {
  result <- data.frame()
  checkmate::assert_true(check_names(df, problem, baseline, 
    algorithm = NULL, measure = NULL))
  checkmate::assert_true(check_names(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  # define samples
  x <- df[df[["problem"]] == problem & df[["algorithm"]] == baseline, measure]
  algorithms <- unique(df[["algorithm"]])
  if (!is.null(algorithm)) {
    algorithms <- algorithm
  }
  for (k in algorithms[algorithms != baseline]) {
    y <- df[df[["problem"]] == problem & df[["algorithm"]] == k, measure]
    # Wilcoxon signed rank test
    w_test <- scmamp::wilcoxonSignedTest(x, y)
    # results
    result_test <- get_data_frame_htest(k = k, p_value = w_test$p.value, 
      test = "t = ", statistic = w_test$statistic)
    result <- rbind(result, result_test)
  }
  output <- get_results_htest(baseline = baseline, measure = measure, 
    method = w_test$method, data = result)
  class(output) <- "h_test"
  return(output)
}

# results <- wilcoxon_signed_test(df = test_benchmark, baseline = "algo_1",
#                                 problem = "problem_a") 
# results
