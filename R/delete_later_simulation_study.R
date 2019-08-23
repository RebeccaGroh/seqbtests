## erstelle einen Datensatz um zu schauen wie gut das adaptive Design funktioniert 

## Simulation für die sequentiellen bayesianischen Tests 
# Experiment 1: get_replications generiert Daten z.B. aus Normalverteilung mit 
# gleichem mean für alle Algorithmen --> testen ob zu früh aufgehört wird 
# Experiment 2: get_replications generiert Daten z.B. aus Normalverteilung mit 
# unterschiedlichem mean --> testen ob Unterschiede gefunden werden 

# (keine theoretischen Angaben möglich, also wichtig zu zeigen, dass es in der 
# Praxis funktioniert )


## zunächst bayesianischen Test erstellen, bei dem auch alle Algorithmen gegen 
## eine Baseline getestet werden. 

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
    result[k, "baseline"] <- baseline
    result[k, "method"] <- b_corr$method
    result[k, "measure"] <- measure
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
  }
  return(result)
}
## dieser Test sollte den alten ersetzen (bzw. alle anderen bayesianischen Tests 
## sollten wenn möglich an die Form angepasst werden) --> gibt die Ergebnisse auch 
## beim Vergleich von nur 2 Algorithmen an 
results <- b_corr_t_test(df = test_benchmark_small, rho=0.1, rope=c(-0.01, 0.01),
                             problemset = 'problem_b', baseline = 'algo_1')
results


# jetzt können die Ergebnisse des normalen und sequentiellen Tests direkt verglichen werden 

results_seq <- seq_b_corr_t_test(df = test_benchmark_small, rho=0.1, max_repls = 10,
                                 problemset = 'problem_b', baseline = 'algo_1', rope=c(-0.01, 0.01))
results_seq


## es treten unterschiede in den Ergebnissen auf!! Teilweise wird zu früh abgebrochen 
## --> eine Mindestanzahl an Replikationen festlegen, die überrpüft werden muss
## um fehlerhaften Ergebnissen vorzubeugen ? 
