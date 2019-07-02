#' @title Wilcoxon signed-rank test 
#' @description This function implements the paired Wilcoxon signed-rank test. 
#' A non-parametric statistical hypothesis test sed to compare the means of two 
#' paired samples. 
#' @param df input data frame
#' @param problemset 
#' @param learner_a First algorithm 
#' @param learner_b Second algorithm 
#' @param measure Measure column (default: first measure column in data frame)
#' @return A list containing the following components: \code{measure}, used measure column; \code{method}, a character string indicating what type of test was performed; \code{statistic}, the value of the statistic used in the test and \code{p.value}, the p-value for the test
#' @details The test has first been implemented in scmamp
#' @references ??
#' @export
wilcoxon_signed_test <- function(df, problemset, learner_a, learner_b, measure =NULL) {
  checkmate::assert_true(check_names(data = df, problemset, learner_a, learner_b = NULL, measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  # define samples 
  x <- df[df[["problem"]] == problemset & df[["algorithm"]] == learner_a, measure]
  y <- df[df[["problem"]] == problemset & df[["algorithm"]] == learner_b, measure]
  # Wilcoxon signed rank test 
  w_test <- scmamp::wilcoxonSignedTest (x, y)
  ## return results 
  result <- list()
  result$measure <- measure
  test <- list(method = w_test$method, statistic = w_test$statistic, p.value = w_test$p.value)
  class(test) <- "htest"
  result$teststatistic <- test 
  return(result)
}


#' @title Correlated t Test 
#' @description This function implements the t-Test for paired samples. 
#' @param df input data frame
#' @param problemset 
#' @param learner_a First algorithm 
#' @param learner_b Second algorithm 
#' @param measure Measure column (default: first measure column in data frame)
#' @param rho Correlation factor (for the case of cross validated results, the 
#' heuristic to set the correlation is size of test set divided by total size of the dataset) 
#' @param alternative a character string specifying the alternative hypothesis, 
#' must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @return A list containing the following components: \code{measure}, used measure column; \code{method}, a character string indicating what type of test was performed; \code{statistic}, the value of the statistic used in the test and \code{p.value}, the p-value for the test
#' @details The test has first been implemented in scmamp
#' @references ??
#' @export
corr_t_test <- function(df, problemset, learner_a, learner_b, measure =NULL, rho = 0.01, alternative="two.sided") {
  checkmate::assert_true(check_names(data = df, problemset, learner_a, learner_b = NULL, measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  # define samples 
  x <- df[df[["problem"]] == problemset & df[["algorithm"]] == learner_a, measure]
  y <- df[df[["problem"]] == problemset & df[["algorithm"]] == learner_b, measure]
  # Correlated t Test 
  corr_test <- scmamp::correlatedTtest(x,y, rho, alternative="two.sided")
  ## return results 
  result <- list()
  result$measure <- measure
  test <- list(method = corr_test$method, statistic = corr_test$statistic, p.value = corr_test$p.value)
  class(test) <- "htest"
  result$teststatistic <- test 
  return(result)
}




## testen 
## mit measure 
results <- corr_t_test(df = benchmark_test_no_pars, problemset = "Adiac", learner_a = "classif.xgboost", learner_b = "classif.ksvm", measure = "measure_ber")
## ohne measure 
results <- corr_t_test(df = benchmark_test_no_pars, problemset = "Adiac", learner_a = "classif.xgboost", learner_b = "classif.ksvm")
results  
