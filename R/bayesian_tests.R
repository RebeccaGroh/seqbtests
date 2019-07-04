#' @title Bayesian correlated T test 
#' @description This function implements the Bayesian version of the correlated t-test. 
#' @param df input data frame
#' @param problemset 
#' @param learner_a First algorithm 
#' @param learner_b Second algorithm 
#' @param measure Measure column (default: first measure column in data frame)
#' @return A list containing the following components:
#' @details The test has first been implemented in scmamp
#' @references \url{https://github.com/b0rxa/scmamp}
#' @export
b_corr_t_test <- function(df, problemset, learner_a, learner_b, measure =NULL, rho = 0.1, rope = c(-0.01, 0.01)) {
  requireNamespace("scmamp", quietly = TRUE)
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  # define samples 
  x <- df[df[["problem"]] == problemset & df[["algorithm"]] == learner_a, measure]
  y <- df[df[["problem"]] == problemset & df[["algorithm"]] == learner_b, measure]
  # Bayesian correlated t Test 
  b_corr <- scmamp::bCorrelatedTtest(x, y, rho, rope)
  result <- list()
  result$measure <- measure
  result$method <- b_corr$method
  result$posteriror_probabilities <- b_corr$posterior.probabilities
  return(result)
}




#' @title Bayesian Sign test 
#' 
b_sign_test <- function(df, problemset, learner_a, learner_b, measure = NULL, 
                        s = 1, z_0 = 0,
                        rope.min = -0.01, rope.max = 0.01,
                        weights = c(s/2, rep(1, length(x))),
                        n.samples = 100000) {
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  x <- df[df[["problem"]] == problemset & df[["algorithm"]] == learner_a, measure]
  y <- df[df[["problem"]] == problemset & df[["algorithm"]] == learner_b, measure]
  b_sign <- rNPBST::
}


## testen 
## mit measure 
results <- b_corr_t_test(df = benchmark_test_full, problemset = "Adiac", learner_a = "classif.xgboost", learner_b = "classif.ksvm", measure = "measure_ber", rho=0.1, rope=c(-0.01, 0.01))
## ohne measure 
results <- b_corr_t_test(df = benchmark_test_full, problemset = "Adiac", learner_a = "classif.xgboost", learner_b = "classif.ksvm", rho=0.1, rope=c(-0.01, 0.01))
results

