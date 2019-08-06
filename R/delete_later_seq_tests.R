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
      
      result[k, "baseline"] <- baseline  
      result[k, "method"] <- b_test$method  
      result[k, "measure"] <- measure  
      result[k, "left"] <- b_test$posterior.probabilities[1]  
      result[k, "rope"] <- b_test$posterior.probabilities[2]  
      result[k, "right"] <- b_test$posterior.probabilities[3]  
      result[k, "repls"] <- i
      if (b_test$posterior.probabilities[3] > 0.95) {
        result[k, "significance_appears"] <- TRUE
        next
      } else {
        result[k, "significance_appears"] <- FALSE
      }
    } 
    if (result[k, "significance_appears"] == TRUE) {
      break
    }
  }
  return(result)
} 

    
    
#results <- seq_b_corr_t_test(df = test_benchmark_small, problemset = "problem_a", baseline = "algo_1", max_repls = 10, rho=0.1, rope=c(-0.01, 0.01))
#results



# wenn ich die Bedingung innerhalb der k schleife Schreibe, dann hört der algorithmus 
# der k schleife auf, sobald der break point erreicht ist. ALso nach dem jeweiligen 
# Algorithmus, der signfikant ist
# was ich aber eigentlich will, ist dass für einen spezifischen Algorithmus das aufrufen 
# der Replikationen beendet wird, wenn der breakpoint erreicht wird
# also: wenn k zu einem früheren Zeitpuntk signifikant besser ist als die Baseline, 
# dann brich nach dieser Repliaktion bei diesem Algorithmus ab und mach mit dem nächsten 
# Algorithmus in der Reihe weiter.
# also wird bei dem aufrufen des Algorithmus mit next gearbeitet, weil mit dem nächsten 
# Algorithmus gearbeitet wird
# Dann wechselt man zu den Replikationen und sagt, wenn es ein next gab in der inneren 
# Schleife, dann brich nach der repliaktion ab, in der man Signifikanz erreicht hat 

# im ersten Datensatz der durchläuft liegen 2 Repliaktionen vor, hier wird für jeden 
# Algorithmus getestet ob der neue Algorithmus signifikant besser ist als die Baseline 
# dann wird der nächste Datensatz aufgerufen und wiederum auf Signfikanz getestet

# wenn ich jetzt eine neue Variable erstelle die TRUE ist, für diese Zeile, also diesen 
# Algorithmus, dann muss kann ich in der äußeren Schleife jetzt sagen, falls in dieser Zeile 
# Signifikanz vorliegt, also TRUE angegeben wurde, dann brich für diesen Algorithmus nach 
# dieser Replikation auf, 

#if (b_test$posterior.probabilities[3] > 0.95) {
#  stop <- TRUE
#}
#} 
#if (stop == TRUE) {
#  break
#}