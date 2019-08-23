
b_corr_t_test <- function(df, problemset, baseline, learner_b = NULL, measure = NULL,
                          compare = NULL, rho = 0.1, rope = c(-0.01, 0.01)) {
  result <- data.frame()
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problemset, baseline, learner_b = NULL, 
                                     measure = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  # define samples
  x <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == baseline, measure]
  algorithms <- unique(df[["algorithm"]])
  for (k in algorithms[algorithms != baseline]) {
    if (!is.null(learner_b)) {
      k <- learner_b
      y <- df[df[["problem"]] == problemset 
              & df[["algorithm"]] == k, measure]
    } else {
      y <- df[df[["problem"]] == problemset 
              & df[["algorithm"]] == k, measure]
    }
    # Bayesian correlated t Test
    b_corr <- scmamp::bCorrelatedTtest(x, y, rho, rope)
    # results
    #result[k, "baseline"] <- baseline
    #result[k, "method"] <- b_corr$method
    #result[k, "measure"] <- measure
    result[k, "algorithm"] <- k
    result[k, "left"] <- b_corr$posterior.probabilities[1]
    result[k, "rope"] <- b_corr$posterior.probabilities[2]
    result[k, "right"] <- b_corr$posterior.probabilities[3]
    if (is.null(compare)) {compare <- "better"}
    if (compare == "better") { 
      threshold <- b_corr$posterior.probabilities[3]
    } else if (compare == "equal") {
      threshold <- b_corr$posterior.probabilities[2] + 
        b_corr$posterior.probabilities[3]
    } 
    if (threshold > 0.95) {
      result[k, "significance_appears"] <- TRUE
    } else {
      result[k, "significance_appears"] <- FALSE
    }
    #output <- get_test_results(baseline, method = b_corr$method, measure, data = result)
    #class(output) <- "btest"
    row.names(result) <- NULL
    btest_list <- list(baseline = baseline, method = b_corr$method, measure = measure, data = result)
    output <- get_test_results(btest_list)
    #class(btest_list) <- "btest"
  }
  return(output)
}

#results <- b_corr_t_test(df = test_benchmark_small, rho=0.1, rope=c(-0.01, 0.01),
#                         problemset = 'problem_e', baseline = 'algo_1')
#results

#------------------------------------------------------------------------------#

#get_test_results <- function(test_liste) {
#  format <- list(baseline = test_liste$baseline, 
#                 measure = test_liste$measure, 
#                 method = test_liste$method, 
#                 data = test_liste$data)
#  class(format) <- "btest"
#  format
#}




#btest_table <- function(test) {
#  UseMethod("btest_table")
#}

#btest_table.btest <- function(list) {
#  ## hier muss die Form festgelegt werden 
#  #print("Result of", list$method, "\n")
#  format <- list(x <- list$baseline,
#                 y <- list$method, 
#                 z <- list$measure, 
#                 xx <- list$data)
#  class(format) <- "btest"
#  #return(format)
#}

## Funktion die die Ergebnisse später aufruft: 
#get_test_results <- function(baseline, method, measure, data, extra = NULL) {
#  output <- list(baseline = baseline, 
#                 methode = method, 
#                 measure = measure, 
#                 data = data, 
#                 extra = extra)
#  class(output) = "btest"
#}

## In dem test muss dann die Klasse aber nicht mehr angegeben werden 

