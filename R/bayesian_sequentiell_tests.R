
################################################################################
## adaptive Tests 
# sequentielles Testen: nacheinander werden die Replikationen miteinander verglichen 
# und es wird abgebrochen, wenn Sifgnifikanz vorliegt. 
## als zweites sollte einfach ein Algortihmus als Baseline festgelegt werden und 
## gegen diese Baseline werden alle anderen Algorithmen getestet 
## abgebrochen wird entweder wenn Signfikanz erreicht wurde oder wenn keine 
## Signfikanz erreicht wird, werden alle Replikationen gegeneinander getestet
## zus?tzlich sollte auch noch festgelegt werden k?nnen, ob man testen will: 
## besser als oder mindestens genauso gut (einmal wird nur right.prob betrachtet und 
## das andere mal werden right.prob und rope.prob zusammen betrachtet)

## Test der funktioniert 
## nach Signfikanz wird mit den Replikationen abgebrochen. Alle Algorithmen werden 
## gegen die Baseline getestet

b_correlated_t_test <- function(df, problemset, baseline, learner_b = NULL, replications = 10, measure =NULL, rho = 0.1, rope = c(-0.01, 0.01)) {
  checkmate::assert_true(require(scmamp))
  if (!is.null(measure)) {
    measure <- measure 
    #} else if (measure = list){## falls eine Liste angegeben wird
    # measure = measure  
  } else {
    measure <- get_measure_columns(df)[1]
  } 
  ## number of replications 
  result = data.frame()
  for (i in 2:length(unique(df[["replications"]]))) {
    # define samples 
    x <- df[df[["problem"]] == problemset & df[["algorithm"]] == baseline, measure]
    for (j in unique(df[["algorithm"]])) {
      if (!is.null(learner_b)) {
        j <- learner_b
        y <- df[df[["problem"]] == problemset & df[["algorithm"]] == j, measure]
      } else {
        y <- df[df[["problem"]] == problemset & df[["algorithm"]] == j, measure]
      }
      # Bayesian correlated t Test 
      b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
      if (b_test$posterior.probabilities[3] > 0.95) {
        break
      }
      #result <- list()
      result[j, "method"] <- b_test$method  
      result[j, "measure"] <- measure  
      result[j, "left"] <- b_test$posterior.probabilities[1]  
      result[j, "rope"] <- b_test$posterior.probabilities[2]  
      result[j, "right"] <- b_test$posterior.probabilities[3]  
      result[j, "repls"] <- i  
      result[j, "baseline"] <- baseline  
    }
  }
  return(result)
} 

## learner_b wird angegeben 
results <- b_correlated_t_test(df = benchmark_test_no_pars, problemset = "Car", baseline = "classif.xgboost", learner_b = "classif.ksvm", rho=0.1, rope=c(-0.01, 0.01))
## learner_b wird nicht angegeben 
results <- b_correlated_t_test(df = benchmark_test_no_pars, problemset = "Car", baseline = "classif.xgboost", rho=0.1, rope=c(-0.01, 0.01))
results




################################################################################
################################################################################




### das hier ist der richtige Code mit dem gearbeitet werden sollte 
## Test pr?ft alle Replikationen und testet alle Algorithmen gegen die Baseline
## aber die Baseline wird dabei nicht mehr gegen sich selbst getestet 
b_correlated_t_test <- function(df, problemset, baseline, learner_b = NULL, measure =NULL, rho = 0.1, rope = c(-0.01, 0.01)) {
  checkmate::assert_true(require(scmamp))
  if (!is.null(measure)) {
    measure <- measure 
    #} else if (measure = list){## falls eine Liste angegeben wird
    # measure = measure  
  } else {
    measure <- get_measure_columns(df)[1]
  } 
  ## number of replications 
  result = data.frame()
  algorithms <- unique(df[["algorithm"]])
  for (i in 2:length(unique(df[["replications"]]))) {
    # define samples 
    x <- df[df[["problem"]] == problemset & df[["algorithm"]] == baseline, measure]
    for (j in algorithms[algorithms!=baseline]) {
      if (!is.null(learner_b)) {
        j <- learner_b
        y <- df[df[["problem"]] == problemset & df[["algorithm"]] == j, measure]
      } else {
        y <- df[df[["problem"]] == problemset & df[["algorithm"]] == j, measure]
      }
      # Bayesian correlated t Test 
      b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
      if (b_test$posterior.probabilities[3] > 0.95) {
        break
      }
      #result <- list()
      result[j, "method"] <- b_test$method  
      result[j, "measure"] <- measure  
      result[j, "left"] <- b_test$posterior.probabilities[1]  
      result[j, "rope"] <- b_test$posterior.probabilities[2]  
      result[j, "right"] <- b_test$posterior.probabilities[3]  
      result[j, "repls"] <- i  
      result[j, "baseline"] <- baseline  
    }
  }
  return(result)
}

## learner_b wird angegeben 
results <- b_correlated_t_test(df = benchmark_test_no_pars, problemset = "Car", baseline = "classif.xgboost", learner_b = "classif.ksvm", rho=0.1, rope=c(-0.01, 0.01))
## learner_b wird nicht angegeben 
results <- b_correlated_t_test(df = benchmark_test_no_pars, problemset = "Car", baseline = "classif.xgboost", rho=0.1, rope=c(-0.01, 0.01))
results



################################################################################
################################################################################



## n?chste Schritte 
## User kann festlegen ob equal oder better 
## wenn User keine Angabe macht, wird better getestet 
seq_b_corr_t_test <- function(df, problemset, baseline, learner_b = NULL, measure = NULL, test = NULL, rho = 0.1, rope = c(-0.01, 0.01)) {
  checkmate::assert_true(require(scmamp))
  if (!is.null(measure)) {
    measure <- measure 
    #} else if (measure = list){## falls eine Liste angegeben wird
    # measure = measure  
  } else {
    measure <- get_measure_columns(df)[1]
  } 
  ## number of replications 
  result = data.frame()
  algorithms <- unique(df[["algorithm"]])
  for (i in 2:length(unique(df[["replications"]]))) {
    # define samples 
    x <- df[df[["problem"]] == problemset & df[["algorithm"]] == baseline, measure]
    for (j in algorithms[algorithms!=baseline]) {
      if (!is.null(learner_b)) {
        j <- learner_b
        y <- df[df[["problem"]] == problemset & df[["algorithm"]] == j, measure]
      } else {
        y <- df[df[["problem"]] == problemset & df[["algorithm"]] == j, measure]
      }
      # Bayesian correlated t Test 
      b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
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
      #result <- list()
      result[j, "method"] <- b_test$method  
      result[j, "measure"] <- measure  
      result[j, "left"] <- b_test$posterior.probabilities[1]  
      result[j, "rope"] <- b_test$posterior.probabilities[2]  
      result[j, "right"] <- b_test$posterior.probabilities[3]  
      result[j, "repls"] <- i  
      result[j, "baseline"] <- baseline  
    }
  }
  return(result)
}

## verwendete sample werden nicht mit repls eingschr?nkt 
## learner_b wird angegeben 
results <- seq_b_corr_t_test(df = benchmark_test_no_pars, problemset = "Car", baseline = "classif.xgboost", learner_b = "classif.ranger.pow", rho=0.1, rope=c(-0.01, 0.01))
## learner_b wird nicht angegeben 
results <- seq_b_corr_t_test(df = benchmark_test_no_pars, problemset = "Car", baseline = "classif.xgboost", rho=0.1, rope=c(-0.01, 0.01))
results

################################################################################
### Funktion funktioniert, Problem ist aber, dass die Classifier nicht in der LIste 
## angezeigt werden, wenn schon fr?her Signifikanz erreicht wurde


#-------------------------------------------------------------------------------


seq_b_corr_t_test <- function(problemset, baseline, learner_b = NULL, max_repls = 20, measure =NULL, test = NULL, rho = 0.1, rope = c(-0.01, 0.01), ...) {
  result = data.frame()
  for (i in 1:max_repls) {
    data <- get_replications(i = i, ...)
    ## check if passed names, define columns in dataset 
    checkmate::assert_true(check_names(data, problemset, baseline, learner_b = NULL, measure = NULL))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    } 
    # define samples 
    x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]
    algorithms <- unique(data[["algorithm"]])
    for (j in algorithms[algorithms!=baseline]) {
      if (!is.null(learner_b)) {
        j <- learner_b
        y <- data[data[["problem"]] == problemset & data[["algorithm"]] == j, measure]
      } else {
        y <- data[data[["problem"]] == problemset & data[["algorithm"]] == j, measure]
      }
      # Bayesian correlated t Test 
      b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
      if (is.null(test)) {     ## test for better 
        threshold <- b_test$posterior.probabilities[3]
      } else if (test == "better") {
        threshold <- b_test$posterior.probabilities[3]
      } else {
        threshold <- b_test$posterior.probabilities[2] + b_test$posterior.probabilities[3]
      }
      if (threshold > 0.99) {
        break
      }
      result[j, "method"] <- b_test$method  
      result[j, "measure"] <- measure  
      result[j, "left"] <- b_test$posterior.probabilities[1]  
      result[j, "rope"] <- b_test$posterior.probabilities[2]  
      result[j, "right"] <- b_test$posterior.probabilities[3]  
      result[j, "repls"] <- i  
      result[j, "baseline"] <- baseline  
    }
  }
  return(result)
}

## verwendete sample werden nicht mit repls eingschr?nkt 
## learner_b wird angegeben 
results <- seq_b_corr_t_test(df = benchmark_test_no_pars, problemset = "Adiac", baseline = "classif.xgboost", max_repls = 10, learner_b = "classif.ksvm", rho=0.1, rope=c(-0.01, 0.01))
## learner_b wird nicht angegeben 
results <- seq_b_corr_t_test(df = benchmark_test_no_pars, problemset = "Adiac", baseline = "classif.xgboost", test ="better", max_repls = 10, rho=0.1, rope=c(-0.01, 0.01))
results

## Problem: 
# Algorithmus bricht eine Runde zu früh ab, eigentlich müsste noch eine Replikation 
# mehr betrachtet werden, bevor abgebrochen wird --> woran liegt das, wie kann man es beheben?


### Funktion die testet, ob Algorithm, problemset etc. wirklich im Datensatz enthalten sind

check_names <- function(data, problemset, baseline, learner_b, measure) {
  checkmate::assert_true(problemset %in% names(data))
  checkmate::assert_true(baseline %in% names(data))
  checkmate::assert_true(learner_b %in% names(data))
  checkmate::assert_true(measure %in% names(data))
}
## sollte noch in die Funktion eingebaut werden, funktioniert aktuell aber nicht
checkmate::assert_true(check_names(...))
check
## was ist mit den anderen Funktionen die die Struktur des Datensatzes überprüfen, 
## eigentlich könnte man die auch alle hier einbauen, dann müssen sie nicht explizit 
## aufgerufen werden, aber der Test kann nur durchgeführt werden, wenn der Datensatz
## in der passenden Form vorliegt
## Dann sollte zusätzlich auch noch überprüft werden, dass die Anzahl der Replkationen 
## für alle Gruppen stimmt, weil das aktuell noch nicht der Fall ist! 
## und es sollte in allen Funktionen einheitlich beim Aufrufen keine " " nötig sein 