#' @title Nemenyi post hoc test 
#' @description This function implements the Nemenyi post hoc test. 
#' @param df Input data frame. 
#' @param measure Measure column. 
#' @param alpha Significance level.
#' @return A list containing the following components: 
#' \itemize{
#'  \item{code{measure}} A string with the name of the measure column used.
#' \item{code{method}} A string with the name of the method used.
#' \item{code{statistic}} The value of the statistic used in the test.
#' \item{code{p_value}} The p-value for the test.
#' \item{code{diff_matrix}} A matrix with all the pair wise differences of 
#'     average rankings.
#' \item{code{significance}} A matrix with all the pair wise comparisons showing 
#'     if there are significant differences among the algorithms.
#' }
#' @details  
#'     The test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @example 
#'     nemenyi_test(test_benchmark)
#' @export
nemenyi_test <- function(df, measure = NULL, alpha = 0.05) {
    checkmate::assert_true(check_names(df, measure = NULL))
    checkmate::assert_true(check_structure(df))
    if (is.null(measure)) {
        measure <- get_measure_columns(df)[1]
    }
    algo_names <- unique(df[["algorithm"]])
    data_wide <- tidyr::spread(df, algorithm, measure)
    sum_data <- aggregate(data_wide[, algo_names], 
                          by = list(data_wide[["problem"]]), FUN = mean)
    # define dataset
    data <- data.frame(sum_data[, -1], row.names = sum_data[, 1])
    # Nemenyi post hoc test
    n_post <- scmamp::nemenyiTest(data, alpha)
    result <- list()
    result$measure <- measure
    result$method <- n_post$method
    result$diff_matrix <- n_post$diff.matrix
    result$statistic <- n_post$statistic
    result$p_value <- n_post$p.value
    result$significance <- abs(n_post$diff.matrix) > n_post$statistic
    return(result)
}



#' @title Friedman's post hoc test 
#' @description This function implements a Friedman post hoc test. It computes 
#' the raw p-values for the post hoc based on Friedman's test. 
#' @param df Input data frame. 
#' @param measure Measure column.
#' @param control The name of the control algorithm. If this parameter is not 
#' provided, all algorithms are compared against each other. 
#' @return A list containing the following components: 
#' \itemize{
#'  \item{code{measure}} A string with the name of the measure column used.
#' \item{code{method}} A string with the name of the method used.
#' \item{code{matrix}}  A matrix with all the pair wise raw p-values. 
#' @details  
#'     The test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @example 
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
    sum_data <- aggregate(data_wide[, algo_names], 
                          by = list(data_wide[["problem"]]), FUN = mean)
    # define dataset
    data <- data.frame(sum_data[, -1], row.names = sum_data[, 1])
    # Friedman post hoc test
    f_post <- scmamp::friedmanPost(data, control)
    result <- list()
    result$measure <- measure
    result$method <- "Friedman post hoc test"
    result$matrix <- f_post
    return(result)
}

