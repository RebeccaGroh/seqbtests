context("bayesian_tests")

#-------------------------------------------------------------------------------
# check if b_corr_t_test() 
# was soll überprüft werden: 
# wie sieht der output aus --> soll Liste sein 
# im output sollte method angegeben sein mit: "Bayesian correlated t-test 




#---------------------------------------------------------------------------test
b_corr_t_test <- function(df, problemset, learner_a, learner_b, 
                          measure = NULL, parameter_algorithm = NULL, 
                          rho = 0.1, rope = c(-0.01, 0.01)) {
  requireNamespace("scmamp", quietly = TRUE)
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  ## oder der User gibt direkt den richtigen Namen für learner_a/_b an. 
  if (!is.null(parameter_algorithm)){
    learner_a <- paste_algo_pars(algorithm = learner_a, parameter_algorithm)
    learner_b <- paste_algo_pars(algorithm = learner_b, parameter_algorithm)
    df[["algorithm"]] <- paste_algo_pars(algorithm = df[["algorithm"]], 
                                         parameter_algorithm = df[["parameter_algorithm"]])
  }
  # define samples 
  x <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_a, measure]
  y <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_b, measure]
  # check numbers in sample
  checkmate::assert_true(get_replications_count(x, y))
  # Bayesian correlated t Test 
  b_corr <- scmamp::bCorrelatedTtest(x, y, rho, rope)
  result <- list()
  result$measure <- measure
  result$method <- b_corr$method
  result$posteriror_probabilities <- b_corr$posterior.probabilities
  # needed for plotting 
  #result$approximate <- b_corr$approximate
  #result$posterior <- b_corr$posterior
  #result$additional <- b_corr$additional
  #result$parameters <- b_corr$parameters
  return(result)
}

test <- b_corr_t_test(df= test_benchmark_small, problemset = "problem_a",
                      learner_a = "algo_1", learner_b = "algo_2")
test
is.list(test)




# testen ob die angegebenen Namen wirklich in dem Datensatz enthalten sind funktioniert nicht!! 
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
