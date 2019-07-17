## eigentlich funktioniert es und es werden auch die richtigen Ergebnisse ausgegeben
# aber es werden auch viele Warnungen oder Hinweise angegeben 
## herausfinden wie die zustande kommen!

b_hierarchical_test <- function(df, learner_a, learner_b, 
                                measure = NULL, parameter_algorithm = NULL, rho = 0.1, 
                                std.upper=1000, d0.lower=NULL, d0.upper=NULL, 
                                alpha.lower=0.5, alpha.upper=5, 
                                beta.lower=0.05, beta.upper=0.15,
                                rope = c(-0.01, 0.01), nsim=2000, nchains=8, 
                                parallel=TRUE, stan.output.file=NULL,
                                seed=as.numeric(Sys.time()), ...) {
  requireNamespace("scmamp", quietly = TRUE)
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  if (!is.null(parameter_algorithm)){
    learner_a <- paste_algo_pars(algorithm = learner_a, parameter_algorithm)
    learner_b <- paste_algo_pars(algorithm = learner_b, parameter_algorithm)
    df[["algorithm"]] <- paste_algo_pars(algorithm = df[["algorithm"]], 
                                         parameter_algorithm = df[["parameter_algorithm"]])
  }
  # define samples 
  x.matrix <- data_transformation(df, algo = learner_a, measure)
  y.matrix <- data_transformation(df, algo = learner_b, measure)
  # check numbers in sample
  checkmate::assert_true(get_replications_count(x.matrix, y.matrix))
  # Bayesian correlated t Test 
  b_hierarchical <- scmamp::bHierarchicalTest(x.matrix, y.matrix, rho, 
                                              std.upper, d0.lower, d0.upper, 
                                              alpha.lower, alpha.upper, 
                                              beta.lower, beta.upper, 
                                              rope, nsim, nchains, parallel, 
                                              stan.output.file, seed)
  result <- list()
  result$measure <- measure
  result$method <- b_hierarchical$method
  result$posteriror_probabilities <- b_hierarchical$posterior.probabilities
  return(result)
}

#-------------------------------------------------------------------------------
df <- test_benchmark_small
learner_a <- "algo_1"
learner_b <- "algo_2"
measure <- c()


if (is.null(measure)) {
  measure <- get_measure_columns(df)[1]
} 

x.matrix <- data_transformation(df, algo = learner_a, measure)
y.matrix <- data_transformation(df, algo = learner_b, measure)
checkmate::assert_true(get_replications_count(x.matrix, y.matrix))


results <- bHierarchicalTest(sample.a, sample.b, rho=0.1, rope=c(-0.01, 0.01), nsim=2000, nchains=5)

b_hierarchical <- scmamp::bHierarchicalTest(x.matrix, y.matrix, rho=0.1, rope=c(-0.01, 0.01), nsim=2000, nchains=5)

b_hierarchical$posterior.probabilities

## sollte das Ergebnis sein: 
# Left   Rope  Right 
#0.9776 0.0084 0.0140 


test <- b_hierarchical_test(df = test_benchmark_small, learner_a = "algo_1", learner_b = "algo_2", rho=0.1, rope=c(-0.01, 0.01), nsim=2000, nchains=5)
test

#-------------------------------------------------------------------------------
results <- b_hierarchical_test(df = test_benchmark_small, learner_a = "algo_1", learner_b = "algo_2",
                               rho=0.1, rope=c(-0.01, 0.01), nsim=2000, nchains=5)
results$posteriror_probabilities
results <- bHierarchicalTest(x.matrix = x.matrix, y.matrix = y.matrix, rho=0.1, rope=c(-0.01, 0.01), nsim=2000, nchains=5)


## funktioniert noch nicht. 
# der Code läuft, aber die ausgegebenen Ergebnisse sind nicht sinnvoll











#-------------------------------------------------------------------------------
b_hierarchical_test <- function(df, learner_a, learner_b, 
                                measure = NULL, parameter_algorithm = NULL, rho = 0.1, 
                                std.upper=1000, d0.lower=NULL, d0.upper=NULL, 
                                alpha.lower=0.5, alpha.upper=5, 
                                beta.lower=0.05, beta.upper=0.15,
                                rope = c(-0.01, 0.01), nsim=2000, nchains=8, 
                                parallel=TRUE, stan.output.file=NULL,
                                seed=as.numeric(Sys.time()), ...) {
  requireNamespace("scmamp", quietly = TRUE)
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  if (!is.null(parameter_algorithm)){
    learner_a <- paste_algo_pars(algorithm = learner_a, parameter_algorithm)
    learner_b <- paste_algo_pars(algorithm = learner_b, parameter_algorithm)
    df[["algorithm"]] <- paste_algo_pars(algorithm = df[["algorithm"]], 
                                         parameter_algorithm = df[["parameter_algorithm"]])
  }
  # define samples 
  x.matrix <- matrix(data_wide[,  learner_a], byrow=TRUE, 
                     nrow=length(unique(data_wide$replications)))
  y.matrix <- matrix(data_wide[,  learner_b], byrow=TRUE, 
                     nrow=length(unique(data_wide$replications)))
  # check numbers in sample
  checkmate::assert_true(get_replications_count(x, y))
  # Bayesian correlated t Test 
  b_hierarchical <- scmamp::bHierarchicalTest(x.matrix, y.matrix=NULL, rho, 
                                              std.upper, d0.lower, d0.upper, 
                                              alpha.lower, alpha.upper, 
                                              beta.lower, beta.upper, 
                                              rope, nsim, nchains, parallel, 
                                              stan.output.file, seed,...)
  result <- list()
  result$measure <- measure
  result$method <- b_hierarchical$method
  result$posteriror_probabilities <- b_hierarchical$posterior.probabilities
  return(result)
}

