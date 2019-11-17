#' @title Nemenyi post hoc test 
#' @description This function implements the Nemenyi post hoc test. 
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @param measure (`character`)\cr Name of the 'measure' column. If not 
#'     defined, the first 'measure' column in the data frame is used.
#' @param alpha (`double`)\cr Significance level to get the critical difference.
#' @return A list containing the following components: 
#' \itemize{
#'     \item{\code{measure}} (`character`)\cr A string with the name of the 
#'         measure column. 
#'     \item{\code{method}} (`character`)\cr A string with the name of the 
#'         method. 
#'     \item{\code{statistic}} (`double`)\cr The value of the statistic used in 
#'         the test.
#'     \item{\code{p_value}} (`double`)\cr The p-value for the test.
#'     \item{\code{diff_matrix}} (`matrix`)\cr A matrix with all the pair wise
#'         differences of average rankings.
#' }
#' @details  
#'     The test has first been implemented in scmamp. 
#' @references \url{https://github.com/b0rxa/scmamp}
#' @examples 
#'     nemenyi_test(test_benchmark)
#' @export
nemenyi_test <- function(df, measure = NULL, alpha = 0.05) {
  result <- data.frame()
  checkmate::assert_true(check_names(df, measure = NULL))
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algo_names <- unique(df[["algorithm"]])
  data_wide <- tidyr::spread(df, algorithm, measure)
  sum_data <- stats::aggregate(data_wide[, algo_names], 
    by = list(data_wide[["problem"]]), FUN = mean)
  # define dataset
  data <- data.frame(sum_data[, -1], row.names = sum_data[, 1])
  # Nemenyi post hoc test
  n_post <- scmamp::nemenyiTest(data, alpha)
  # return results 
  result[1, "test"] <- "Critical difference = "
  result[1, "statistic"] <- n_post$statistic
  output <- get_results_htest(measure = measure, method = n_post$method, 
    matrix = n_post$diff.matrix, data = result)
  class(output) <- "nemenyi"
  return(output)
}


#' @title Friedman's post hoc test 
#' @description This function implements a Friedman post hoc test. It computes 
#' the raw p-values for the post hoc based on Friedman's test. 
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @param measure (`character`)\cr Name of the 'measure' column. If not 
#'     defined, the first 'measure' column in the data frame is used.
#' @param control (`character`)\cr The name of the control algorithm. If this 
#'     parameter is not provided, all algorithms are compared against each 
#'     other. 
#' @return A list containing the following components: 
#' \itemize{
#'     \item{\code{measure}} (`character`)\cr A string with the name of the 
#'         measure column. 
#'     \item{\code{method}} (`character`)\cr A string with the name of the 
#'         method. 
#'     \item{code{matrix}} (`matrix`)\cr A matrix with all the pair wise raw p-values. 
#' }
#' @details  
#'     The test has first been implemented in scmamp. 
#' @references \url{https://github.com/b0rxa/scmamp}
#' @examples 
#'     friedman_post(test_benchmark)
#' @export
friedman_post <- function(df, measure = NULL, control = NULL) {
  checkmate::assert_true(check_names(df, measure = NULL))
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algo_names <- unique(df[["algorithm"]])
  data_wide <- tidyr::spread(df, algorithm, measure)
  sum_data <- stats::aggregate(data_wide[, algo_names], 
    by = list(data_wide[["problem"]]), FUN = mean)
  # define dataset
  data <- data.frame(sum_data[, -1], row.names = sum_data[, 1])
  # Friedman post hoc test
  f_post <- scmamp::friedmanPost(data, control)
  # return results 
  output <- get_results_htest(measure = measure, data = f_post, 
    method = "Friedman post hoc test")
  class(output) <- "h_test_small"
  return(output)
}

