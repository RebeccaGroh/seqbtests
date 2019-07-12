#' @title Wilcoxon signed-rank test 
#' @description 
#' This function implements the paired Wilcoxon signed-rank test. A 
#' non-parametric statistical hypothesis test sed to compare the means of two 
#' paired samples. 
#' @param df Input data frame. 
#' @param problemset Problemset on which the test should be performed. 
#' @param learner_a First algorithm.
#' @param learner_b Second algorithm. 
#' @param measure Measure column. 
#' @return A list containing the following components: 
#' \item{code{measure}}{a string with the name of the measure column used}
#' \item{code{method}}{a string with the name of the method used}
#' \item{code{statistic}}{the value of the statistic used in the test}
#' \item{code{p_value}}{the p-value for the test}
#' @details 
#' The test has first been implemented in scmamp.
#' Note that the default value for measure is the first measure column in the 
#' data frame.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @export
wilcoxon_signed_test <- function(df, problemset, learner_a, 
                                 learner_b, measure =NULL) {
  checkmate::assert_true(check_names(data = df, problemset, 
                                     learner_a, learner_b = NULL, 
                                     measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  # define samples 
  x <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_a, measure]
  y <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_b, measure]
  # Wilcoxon signed rank test 
  w_test <- scmamp::wilcoxonSignedTest (x, y)
  ## return results 
  result <- list()
  result$measure <- measure
  test <- list(method = w_test$method, 
               statistic = w_test$statistic, 
               p_value = w_test$p.value)
  class(test) <- "htest"
  result$teststatistic <- test 
  return(result)
}


#' @title Correlated t Test 
#' @description This function implements the t-Test for paired samples. 
#' @param df Input data frame. 
#' @param problemset Problemset on which the test should be performed. 
#' @param learner_a First algorithm.
#' @param learner_b Second algorithm. 
#' @param measure Measure column. 
#' @param rho Correlation factor. (for the case of cross validated results, the 
#' heuristic to set the correlation is size of test set divided by total size 
#' of the dataset) 
#' @param alternative a character string specifying the alternative hypothesis, 
#' must be one of "two.sided" (default), "greater" or "less". 
#' You can specify just the initial letter.
#' @return A list containing the following components: 
#' \item{code{measure}}{a string with the name of the measure column used}
#' \item{code{method}}{a string with the name of the method used}
#' \item{code{statistic}}{the value of the statistic used in the test}
#' \item{code{p_value}}{the p-value for the test}
#' @details The test has first been implemented in scmamp.
#' Note that the default value for measure is the first measure column in the 
#' data frame.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @export
corr_t_test <- function(df, problemset, learner_a, learner_b, measure =NULL, 
                        rho = 0.01, alternative="two.sided") {
  checkmate::assert_true(check_names(data = df, problemset, learner_a, 
                                     learner_b = NULL, measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  # define samples 
  x <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_a, measure]
  y <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_b, measure]
  # Correlated t Test 
  corr_test <- scmamp::correlatedTtest(x,y, rho, alternative="two.sided")
  ## return results 
  result <- list()
  result$measure <- measure
  test <- list(method = corr_test$method, 
               statistic = corr_test$statistic, 
               p.value = corr_test$p.value)
  class(test) <- "htest"
  result$teststatistic <- test 
  return(result)
}


#-------------------------------------------------------------------------------
## Problem: bei vielen von den Tests wird als Argument nur "data" angegeben. 
# Herausfinden, wie dieser Datensatz aufgebaut sein sollte und versuchen diesen 
# innerhalb der Funktion nach dem selben Konzept aufzubauen, wie das auch in den 
# anderen Funktionen gemacht wird. 
# bei scmamp kann man in der Vignette nach Beispielen schauen. Da müsste angegeben 
# werden wie der Datensatz aussieht. Die arbeiten einfach mit dem Datensatz data.gh.2008
# ohne jegliche Anpassung 
# Aufbau: Datensätze in den Zeilen und Algorithmen in den Spalten
# in den Zellen selbst sind dann die Werte angegeben 
# kann man ähnlichen Code verwenden wie um die Matrizen bei den bayesianischen 
# Tests zu bauen 

#' @title Friedman's test 
#' 

friedman_test <- function()





## testen 
## mit measure 
results <- corr_t_test(df = benchmark_test_no_pars, problemset = "Adiac", learner_a = "classif.xgboost", learner_b = "classif.ksvm", measure = "measure_ber")
## ohne measure 
results <- corr_t_test(df = benchmark_test_no_pars, problemset = "Adiac", learner_a = "classif.xgboost", learner_b = "classif.ksvm")
results  
