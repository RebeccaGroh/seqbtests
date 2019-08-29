#------------------------------------------------------------------------------#
#------------------------ test all against each other -------------------------#

## alle Algorithmen sollen gegeneinader getestet werden: 
# dafür muss keine Baseline und kein learner gewählt werden, sondern es werden 
# alle einfach so verwendet 

# zunächst alle werden gegeneinander getestet auch mit Wiederholungen 
seq_b_corr_t_test <- function(problemset, baseline = NULL, learner_b = NULL, 
                              measure = NULL, compare = NULL, rho = 0.1, 
                              rope = c(-0.01, 0.01), max_repls = 20, ...) {
  result_all <- data.frame()
  result <- data.frame()
  algos <- unique(data[["algorithm"]])
  for (f in algos) {
    baseline <- f
    for (i in 2:max_repls) {
      data <- get_replications(i, ...)
      ## check if passed names, define columns in dataset
      checkmate::assert_true(check_structure(df = data))
      checkmate::assert_true(check_names(df = data, problemset, baseline, 
                                         learner_b = NULL, measure = NULL, 
                                         parameter_algorithm = NULL))
      if (is.null(measure)) {
        measure <- get_measure_columns(data)[1]
      }
      # define samples
      x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]
      algorithms <- unique(data[["algorithm"]])
      if (i == 2) {
        liste <- c()
      }
      algorithms <- setdiff(algorithms, liste)
      for (k in algorithms[algorithms != baseline]) {
        if (!is.null(learner_b)) {
          k <- learner_b
          y <- data[data[["problem"]] == problemset 
                    & data[["algorithm"]] == k, measure]
        } else {
          y <- data[data[["problem"]] == problemset 
                    & data[["algorithm"]] == k, measure]
        }
        # Bayesian correlated t Test
        b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
        #if (b_test$posterior.probabilities[3] > 0.95) {
        #  break
        #}
        result[k, "baseline"] <- baseline
        result[k, "method"] <- b_test$method
        result[k, "measure"] <- measure
        result[k, "left"] <- b_test$posterior.probabilities[1]
        result[k, "rope"] <- b_test$posterior.probabilities[2]
        result[k, "right"] <- b_test$posterior.probabilities[3]
        result[k, "repls"] <- i
        if (is.null(compare)) {compare <- "better"}
        if (compare == "better") { 
          threshold <- b_test$posterior.probabilities[3]
        } else if (compare == "equal") {
          threshold <- b_test$posterior.probabilities[2] + 
            b_test$posterior.probabilities[3]
        } 
        if (threshold > 0.95) {
          result[k, "significance_appears"] <- TRUE
        } else {
          result[k, "significance_appears"] <- FALSE
        }
        liste <-  rownames(result[result[["significance_appears"]] == TRUE, ])
      }
      if (!is.null(learner_b)) {
        if (threshold > 0.95) {
          break 
        }
      }
    }
    result_all[f, "result"] <- result
  }
  return(result)
}

#results <- seq_b_corr_t_test(df = test_benchmark_small, problemset = 'problem_a', 
#                             max_repls = 10, rho=0.1, 
#                             rope=c(-0.01, 0.01)) 
#results

## rbind results?? 



#------------------------------------------------------------------------------#

## alle Algorithmen sollten gleichzeitig verglichen werden 
## Außerdem: einen Algorithmus als Baseline festlegen und ersetzen wenn ein besserer gefunden wird. Das müsste leichter gehen 

## Jeden Algorithmus gegen jeden anderen im Datensatz testen: 


seq_b_corr_t_test <- function(problemset, baseline = NULL, learner_b = NULL, 
                              measure = NULL, compare = NULL, rho = 0.1, 
                              rope = c(-0.01, 0.01), max_repls = 20, ...) {
  result <- data.frame()
  result_all <- data.frame()
  
  for (i in 2:max_repls) {
    data <- get_replications(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, problemset, baseline = NULL, 
                                       learner_b = NULL, measure = NULL, 
                                       parameter_algorithm = NULL))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    algos <- unique(data[["algorithm"]])
    for (f in algos) {
      baseline <- f
      # define samples
      x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]
      algorithms <- unique(data[["algorithm"]])
      if (i == 2) {
        liste <- c()
      }
      algorithms <- setdiff(algorithms, liste)
      for (k in algorithms[algorithms != baseline]) {
        if (!is.null(learner_b)) {
          k <- learner_b
          y <- data[data[["problem"]] == problemset 
                    & data[["algorithm"]] == k, measure]
        } else {
          y <- data[data[["problem"]] == problemset 
                    & data[["algorithm"]] == k, measure]
        }
        # Bayesian correlated t Test
        b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
        #if (b_test$posterior.probabilities[3] > 0.95) {
        #  break
        #}
        result[k, "baseline"] <- baseline
        result[k, "method"] <- b_test$method
        result[k, "measure"] <- measure
        result[k, "left"] <- b_test$posterior.probabilities[1]
        result[k, "rope"] <- b_test$posterior.probabilities[2]
        result[k, "right"] <- b_test$posterior.probabilities[3]
        result[k, "repls"] <- i
        if (is.null(compare)) {compare <- "better"}
        if (compare == "better") { 
          threshold <- b_test$posterior.probabilities[3]
        } else if (compare == "equal") {
          threshold <- b_test$posterior.probabilities[2] + 
            b_test$posterior.probabilities[3]
        } 
        if (threshold > 0.95) {
          result[k, "significance_appears"] <- TRUE
        } else {
          result[k, "significance_appears"] <- FALSE
        }
        liste <-  rownames(result[result[["significance_appears"]] == TRUE, ])
      }
      result_all[f, k] <- result
    }
    # muss vielleicht woanders hingeschrieben werden...
    #if (!is.null(learner_b)) {
    #  if (threshold > 0.95) {
    #    break 
    #  }
    #}
  }
  return(result)
}

#results <- seq_b_corr_t_test(df = test_benchmark_small, problemset = 'problem_a', 
#                             max_repls = 10, rho=0.1, 
#                             rope=c(-0.01, 0.01)) 
#results



## find best algorithm --------------------------------------------------------- 
# Überlegungen: 
# Es wird keine Baseline festgelegt sondern nur der Datensatz (oder soll der User 
# eine Baseline festlegen können? ja falls das dann immernoch funktioniert)
# Die Namen aller Algorithmen speichern, dann wird der erste von denen ausgewählt 
# und als Baseline festgelegt. 
# Diesen teste ich dann gegen alle Algorithmen in diesem Datensatz und falls ich 
# einen anderen Algorithmus finde, der zu 95% besser (oder gleich gut) ist, dann
# wird die Zeile zu dem Ergebnis gespeichert (kommt später in einen Datensatz) 
# Außerdem wird der Name dieses Algorithmus in einer Variable gespeichert. 
# Ein break wird durchgeführt und der Algorithmus der besser war wird beim nächsten 
# durchlauf als Baseline verwendet und ebenfalls gegen alle anderen Algorithmen 
# (außer denen die zuvor schon als Baseline fungiert hatten und in einer Liste 
# gespeichert wurden) getestet und ab hier wiederholt sich das vorgehen dann nur 
# noch 
# --> wie können die Ergebnisse gespeichert werden? Frage: Will man die Ergebnisse
# nur von dem Algorithmus der signifikant besser war oder alle? Eigentlich alle. 
# Vielleicht kann man den Test später auch noch so anpassen dass nach methoden 
# getestet wird: entweder allvsall oder findbest. 
# Also werden in der inneren Schleife immer die Ergebnisse der Baseline gegen die 
# Algorithmen gespeichert (auch wenn nicht gegen alle getestet wurde sondern bei dem 
# ersten signifikanten Ergebnis abgebrochen wurde)

##+ FRAGE: Ist Algorithmus A, der signifikant besser ist als Algorithmus B, auch 
## signfikant besser als Algorithmus C (wenn B sig besser als C ist?)

# wenn man sich die Werte anschaut wirkt es als stimmt die aussage 
# was ist wenn kein Algorithmus als eindeutig besser identifiziert werden kann: 
# algo 2 ist besser als algo 1, aber nicht zwangsläufig besser als die anderen. 
# wie wird dann vorgegangen? wird dann automatisch gesagt dass der am besten ist? 
# das wäre falsch, weil dann die Reihenfolge eine Rolle spielt. Wenn z.b. algo 5 
# auch besser ist als algo 1 aber nicht besser als algo 2
## dann darf es nur zu einem Break kommen, wenn dieser einen Algorithmus (also 
# dann left) mit 95 % wahrscheinlichkeit besser ist als alle anderen. 
# wenn das nicht der Fall ist, wird der Algorithmus weiter durchlaufen gelassen
# und einfach die Ergebniss von allen Tests ausgegeben und dazu eine Aussage angezeigt, 
# das kein Algorithmus eindeutig besser ist als alle anderen 
# zum Testen: beim b_corr_t_test müsste auf Problem set a algo_5 der beste Test 
# sein, in problem_b darf aber keiner egfunden werden




seq_b_corr_t_test <- function(problemset, baseline, algorithm = NULL, 
                              measure = NULL, compare = NULL, rho = 0.1, 
                              rope = c(-0.01, 0.01), max_repls = 20, ...) {
  result = data.frame()
  for (i in 5:max_repls) {
    data <- get_replications(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, problemset, baseline, 
                                       algorithm = NULL, measure = NULL))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    # loop für baseline 
    algorithms <- unique(data[["algorithm"]])
    if (i == 5) {     # hier können alle ausgeschlossen werden die schon dran waren 
      liste <- c()
    }
    algo_baseline <- 
    algorithms <- setdiff(algorithms, liste)
    for (j in algorithms) {
      if (is.null(baseline)) {
        baseline <- algorithms[1]
      }
      # define samples
      x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]
      for (k in algorithms[algorithms != baseline]) {
        if (!is.null(algorithm)) {
          k <- algorithm
          y <- data[data[["problem"]] == problemset 
                    & data[["algorithm"]] == k, measure]
        } else {
          y <- data[data[["problem"]] == problemset 
                    & data[["algorithm"]] == k, measure]
        }
        # Bayesian correlated t Test
        b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
        # results 
        number <- paste(j, k, sep = "_")
        result[number, "algorithm"] <- k
        result[number, "left"] <- b_test$posterior.probabilities[1]
        result[number, "rope"] <- b_test$posterior.probabilities[2]
        result[number, "right"] <- b_test$posterior.probabilities[3]
        result[number, "repls"] <- i
        # falls einer signifikant ist, dann brich ab und mach mit diesem als 
        # nächster Baseline weiter 
        if (is.null(compare)) {compare <- "better"}
        if (compare == "better") { 
          threshold <- b_test$posterior.probabilities[3]
        } else if (compare == "equal") {
          threshold <- b_test$posterior.probabilities[2] + 
            b_test$posterior.probabilities[3]
        } 
        if (threshold > 0.95) {
          result[k, "significanct"] <- TRUE
        } else {
          result[k, "significanct"] <- FALSE
        }
        liste <-  rownames(result[result[["significanct"]] == TRUE, ])
      }
    }
  }  
}
#    #---------------------------------------------------------------------------
#    # define samples
#    x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]
#    algorithms <- unique(data[["algorithm"]])
#    if (i == 5) {
#      liste <- c()
#    }
#    algorithms <- setdiff(algorithms, liste)
#    for (k in algorithms[algorithms != baseline]) {
#      if (!is.null(algorithm)) {
#        k <- algorithm
#        y <- data[data[["problem"]] == problemset 
#                  & data[["algorithm"]] == k, measure]
#      } else {
#        y <- data[data[["problem"]] == problemset 
#                  & data[["algorithm"]] == k, measure]
#      }
#      # Bayesian correlated t Test
#      b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
#      #if (b_test$posterior.probabilities[3] > 0.95) {
#      #  break
#      #}
#      result[k, "algorithm"] <- k
#      result[k, "left"] <- b_test$posterior.probabilities[1]
#      result[k, "rope"] <- b_test$posterior.probabilities[2]
#      result[k, "right"] <- b_test$posterior.probabilities[3]
#      result[k, "repls"] <- i
#      if (is.null(compare)) {compare <- "better"}
#      if (compare == "better") { 
#        threshold <- b_test$posterior.probabilities[3]
#      } else if (compare == "equal") {
#        threshold <- b_test$posterior.probabilities[2] + 
#          b_test$posterior.probabilities[3]
#      } 
#      if (threshold > 0.95) {
#        result[k, "significanct"] <- TRUE
#      } else {
#        result[k, "significanct"] <- FALSE
#      }
#      liste <-  rownames(result[result[["significanct"]] == TRUE, ])
#    }
#    if (!is.null(algorithm)) {
#      if (threshold > 0.95) {
#        break 
#      }
#    }
#  }
#  output_test <- get_results(baseline, measure, method = b_test$method, 
#                             data = result)
#  return_test <- format_test(output_test)
#  return(return_test)
#}

#results <- seq_b_corr_t_test(df = test_benchmark_small, rho=0.1,
#                             problemset = 'problem_b', 
#                             baseline = 'algo_1', compare = 'equal', 
#                             max_repls = 10,  rope=c(-0.01, 0.01))
#results
