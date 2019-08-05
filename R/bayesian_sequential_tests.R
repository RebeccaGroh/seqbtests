#' @title Sequential bayesian correlated t test 
#' @description 
#' This function implements a sequential approach to compare the performance of 
#' machine learning algorithms to one another. Sample size is not fixed in 
#' adavnce, data are evaluated as they are collected. Further sampling is 
#' stopped in accordance with a pre-defined stopping rule as soon as significant 
#' results are obtained.  
#' @param problemset Problemset on which the test should be performed. 
#' @param baseline First algorithm.
#' @param learner_b Second algorithm. If not defined, every algorithm will be 
#' tested against baseline. 
#' @param measure Measure column. 
#' @param test Defines whether the performances should be tested for either 
#' being better ('better') or being just as good ('equal').
#' @param rho Correlation factor. 
#' @param rope Region of practical equivalence. 
#' @param max_repls maximum number of replications that should be build in 
#' get_replications, or maximum number of replications in data frame, if 
#' complete data frame is being used.  
#' @param rope Region of practical equivalence. 
#' @return A data frame with one row for each considered algorithm that is 
#' tested against the baseline, containing the following components:
#' \item{code{measure}}{a string with the name of the measure column used}
#' \item{code{method}}{a string with the name of the method used}
#' \item{code{posteriror_probabilities}}{a vector with the left, rope and right 
#' probabilities}
#' \item{code{repls}}{number of the considered replications}
#' @details The basics of the test have first been implemented in scmamp. 
#' Note that the default value for measure is the first measure column in the 
#' data frame. The default of rho is 0.1. If rho equals 0 this converts the test 
#' in the equivalent of the standard t test. 
#' @example results <- seq_b_corr_t_test(test = "equal", df = test_benchmark_small, problemset = "problem_a", baseline = "algo_1", max_repls = 10, rho=0.1, rope=c(-0.01, 0.01))
#' @export
seq_b_corr_t_test <- function(problemset, baseline, learner_b = NULL, measure =NULL, test = NULL, rho = 0.1, rope = c(-0.01, 0.01), max_repls = 20,  ...) {
  result = data.frame()
  for (i in 2:max_repls) {
    data <- get_replications(i,...)
    ## check if passed names, define columns in dataset 
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, problemset, baseline, learner_b = NULL, 
                                       measure = NULL, parameter_algorithm = NULL))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    } 
   # define samples 
    x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]
    algorithms <- unique(data[["algorithm"]])
    for (k in algorithms[algorithms!=baseline]) {
      if (!is.null(learner_b)) {
        k <- learner_b
        y <- data[data[["problem"]] == problemset & data[["algorithm"]] == k, measure]
      } else {
        y <- data[data[["problem"]] == problemset & data[["algorithm"]] == k, measure]
      }
      # Bayesian correlated t Test 
      b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
      if (b_test$posterior.probabilities[3] > 0.95) {
        break
      }
      result[k, "baseline"] <- baseline  
      result[k, "method"] <- b_test$method  
      result[k, "measure"] <- measure  
      result[k, "left"] <- b_test$posterior.probabilities[1]  
      result[k, "rope"] <- b_test$posterior.probabilities[2]  
      result[k, "right"] <- b_test$posterior.probabilities[3]  
      result[k, "repls"] <- i
    } 
  }
  return(result)
}

#------------------------------------------------------------------------------
# erst später einbauen, wenn das richtige Ergebnis angezeigt wird
#if (is.null(test)) {     ## test for better 
#  threshold <- b_test$posterior.probabilities[3]
#} else if (test == "equal") {
#  threshold <- b_test$posterior.probabilities[2] + b_test$posterior.probabilities[3]
#} else {
#  threshold <- b_test$posterior.probabilities[3]
#}



