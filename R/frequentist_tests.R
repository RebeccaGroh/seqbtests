#' @title Correlated t test 
#' @description This function implements a two-sided t test for paired samples. 
#' @param df Input data frame. 
#' @param problemset Problem set on which the test should be performed. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. 
#' @param measure Measure column. 
#' @param rho Correlation factor. (For the case of cross validated results, the 
#'     heuristic to set the correlation is size of test set divided by total 
#'     size of the dataset.) 
#' @return A list containing the following components: 
#' \itemize{
#'  \item{code{measure}} A string with the name of the measure column used.
#' \item{code{method}} A string with the name of the method used.
#' \item{code{statistic}} The value of the statistic used in the test.
#' \item{code{p_value}} The p-value for the test.
#' }
#' @details  
#'     The test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @example 
#'     results <- corr_t_test(df= test_benchmark_small, 
#'     problemset = "problem_a", baseline = "algo_1", algorithm = "algo_2")
#' @export
corr_t_test <- function(df, problemset, baseline, algorithm, measure = NULL, 
                        rho = 0.01) {
    checkmate::assert_true(check_structure(df))
    checkmate::assert_true(check_names(df, problemset, baseline, 
                                       algorithm, measure = NULL))
    if (is.null(measure)) {
        measure <- get_measure_columns(df)[1]
    }
    # define samples
    x <- df[df[["problem"]] == problemset 
            & df[["algorithm"]] == baseline, measure]
    y <- df[df[["problem"]] == problemset 
            & df[["algorithm"]] == algorithm, measure]
    # Correlated t Test
    corr_test <- scmamp::correlatedTtest(x, y, rho, alternative = "two.sided")
    ## return results
    result <- list()
    result$measure <- measure
    test <- list(method = corr_test$method, statistic = corr_test$statistic, 
                 p.value = corr_test$p.value)
    class(test) <- "htest"
    result$teststatistic <- test
    return(result)
}


#' @title Friedman's test 
#' @description This function implements the Friedman's test for multiple 
#'     comparisons. A non-parametric statistical test to detect differences in 
#'     in algorithms performances over multiple datasets. 
#' @param df Input data frame.
#' @param measure Measure column. 
#' @return A list containing the following components: 
#' \itemize{
#'  \item{code{measure}} A string with the name of the measure column used.
#' \item{code{method}} A string with the name of the method used.
#' \item{code{statistic}} The value of the statistic used in the test.
#' \item{code{p_value}} The p-value for the test.
#' }
#' @details  
#'     The test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @example 
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
    sum_data <- aggregate(data_wide[, algo_names], 
                          by = list(data_wide[["problem"]]), FUN = mean)
    # define dataset
    data <- data.frame(sum_data[, -1], row.names = sum_data[, 1])
    # Friedman Test
    f_test <- scmamp::friedmanTest(data)
    ## return results
    result <- list()
    result$measure <- measure
    result$method <- f_test$method
    result$statistic <- f_test$statistic
    result$p_value <- f_test$p.value
    return(result)
}


#' @title Wilcoxon signed-rank test 
#' @description 
#'     This function implements the paired Wilcoxon signed-rank test. A 
#'     non-parametric statistical hypothesis test to compare the means of two 
#'     paired samples. 
#' @param df Input data frame. 
#' @param problemset Problem set on which the test should be performed. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. 
#' @param measure Measure column. 
#' @return A list containing the following components: 
#' \itemize{
#'  \item{code{measure}} A string with the name of the measure column used.
#' \item{code{method}} A string with the name of the method used.
#' \item{code{statistic}} The value of the statistic used in the test.
#' \item{code{p_value}} The p-value for the test.
#' }
#' @details  
#'     The test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @example 
#'     results <- wilcoxon_signed_test(df = test_benchmark, baseline = "algo_1",
#'     algorithm = "algo_2", problemset = "problem_a")  
#' @export
wilcoxon_signed_test <- function(df, problemset, baseline, algorithm, 
                                 measure = NULL) {
    checkmate::assert_true(check_names(df, problemset, baseline, algorithm, 
                                       measure = NULL))
    checkmate::assert_true(check_names(df))
    if (is.null(measure)) {
        measure <- get_measure_columns(df)[1]
    }
    # define samples
    x <- df[df[["problem"]] == problemset 
            & df[["algorithm"]] == baseline, measure]
    y <- df[df[["problem"]] == problemset 
            & df[["algorithm"]] == algorithm, measure]
    # Wilcoxon signed rank test
    w_test <- scmamp::wilcoxonSignedTest(x, y)
    ## return results return results
    result <- list()
    result$measure <- measure
    result$method <- w_test$method
    result$statistic <- w_test$statistic
    result$p_value <- w_test$p.value
    return(result)
}

