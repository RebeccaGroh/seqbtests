
results <- seq_b_corr_t_test(df = test_benchmark_small, problemset = "problem_a", baseline = "algo_1", max_repls = 10, learner_b = "algo_2", rho=0.1, rope=c(-0.01, 0.01))

df = test_benchmark_small
problemset = "problem_a"
baseline = "algo_1"
max_repls = 10
learner_b = "algo_4"
measure <- c()

j = 11




data <- get_replications(i = j,df)  # funktioniert es nachher wenn ich bei beiden Funktionen ... angebe? 

if (is.null(measure)) {
  measure <- get_measure_columns(data)[1]
} 
x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]  # ein Wert für j= 1 
algorithms <- unique(data[["algorithm"]])

 
for (k in algorithms[algorithms!=baseline]) {
  if (!is.null(learner_b)) {
    k <- learner_b
    y <- data[data[["problem"]] == problemset & data[["algorithm"]] == k, measure]
  } else {
    y <- data[data[["problem"]] == problemset & data[["algorithm"]] == k, measure]
  }
} ## hier wird k immer mit algo_5 angegeben. Vielleicht weil die Schleife immer komplett 
## durchläuft und am schluss wird immer algo_5 verwendet --> prüfen wie das nachher innerhalb der Funktion ist 
rho = 0.1
rope = c(-0.01, 0.01)

b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
b_test
#-------------------------------------------------------------------------------
seq_b_corr_t_test <- function(problemset, baseline, learner_b = NULL, max_repls = 20, measure =NULL, test = NULL, rho = 0.1, rope = c(-0.01, 0.01), ...) {
  result = data.frame()
  max_repls <- max_repls
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


results <- seq_b_corr_t_test(test = "better", df = test_benchmark_small, problemset = "problem_a", baseline = "algo_1", max_repls = 10, rho=0.1, rope=c(-0.01, 0.01))
results
#-------------------------------------------------------------------------------


if (is.null(test)) {     ## test for better 
  threshold <- b_test$posterior.probabilities[3]
} else if (test == "better") {
  threshold <- b_test$posterior.probabilities[3]
} else {
  threshold <- b_test$posterior.probabilities[2] + b_test$posterior.probabilities[3]
}
if (threshold > 0.95) {
  break
}

#-------------------------------------------------------------------------------
df = test_benchmark_small
problemset = "problem_a"
baseline = "algo_1"
max_repls = 10
learner_b = "algo_5"
measure <- c()

j = 4
data <- get_replications(i = j,df)  # funktioniert es nachher wenn ich bei beiden Funktionen ... angebe? 

if (is.null(measure)) {
  measure <- get_measure_columns(data)[1]
} 
x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]  # ein Wert für j= 1 
y <- data[data[["problem"]] == problemset & data[["algorithm"]] == learner_b, measure]


rho = 0.1
rope = c(-0.01, 0.01)

b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
b_test
isTRUE(b_test$posterior.probabilities[3] > 0.95)
