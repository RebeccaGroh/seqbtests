seq_b_corr_t_test <- function(problem, baseline, algorithm = NULL, 
                              measure = NULL, compare = NULL, rho = 0.1, rope = c(-0.01, 0.01), 
                              max_repls = 20, prob = 0.95, min_num = 5, ...) {
  result <- data.frame()
  for (i in min_num:max_repls) {
    data <- get_replications(i, ...)
    ## check if passed names, define columns in dataset
    checkmate::assert_true(check_structure(df = data))
    checkmate::assert_true(check_names(df = data, problem, baseline, 
                                       algorithm = NULL, measure = NULL))
    if (is.null(measure)) {
      measure <- get_measure_columns(data)[1]
    }
    # define samples
    x <- data[data[["problem"]] == problem 
              & data[["algorithm"]] == baseline, measure]
    algorithms <- unique(data[["algorithm"]])
    if (i == min_num) {
      liste <- c()
    }
    algorithms <- setdiff(algorithms, liste)
    if (!is.null(algorithm)) {
      algorithms <- algorithm
    }
    for (k in algorithms[algorithms != baseline]) {
      y <- data[data[["problem"]] == problem 
                & data[["algorithm"]] == k, measure]
      # Bayesian correlated t Test
      b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
      test_result <- get_data_frame_seq(k = k, 
                                        posterior = b_test$posterior.probabilities, repls = i)
      result <- rbind(result, test_result)
      if (is.null(compare)) {compare <- "better"}
      if (compare == "better") { 
        threshold <- b_test$posterior.probabilities[1]
        threshold_vv <- b_test$posterior.probabilities[3]
      } else if (compare == "equal") {
        threshold <- b_test$posterior.probabilities[2] + 
          b_test$posterior.probabilities[1]
        threshold_vv <- b_test$posterior.probabilities[2] + 
          b_test$posterior.probabilities[3]
      } 
      if (threshold > prob | threshold_vv > prob) {
        liste <- rbind(liste, k)
      }
    }
    if (!is.null(algorithm)) {
      if (threshold > prob) {
        break 
      }
    }
  }
  result <- get_rows(data_frame = result)
  output <- get_results(baseline, measure, method = b_test$method,
                        data = result)
  return(output)
}

results <- seq_b_corr_t_test(df = test_benchmark_small, rho=0.1,
                             problem = "problem_b", baseline = "algo_1",
                             compare = "better", max_repls = 10, min_num = 5)
results

test <- results$data_frame
test

get_probabilities <- function(result, compare = "better") {
  for (i in 1:nrow(result)) {
    if (compare == "better") {
      threshold <- result[i, "left"]
      threshold_vv <- result[i, "right"]
      decision_base <- ">>"
      decision_algo <- "<<"
    }
    if (compare == "equal") {
      threshold <- result[i, "left"] + result[i, "rope"]
      threshold_vv <- result[i, "right"] + result[i, "rope"]
      decision_base <- ">="
      decision_algo <- "<="
    }
    if (threshold > 0.95) {
      result$tests[i] <- paste("P(Baseline", decision_base,  "Algorithm) >", prob, sep = " ")
    } else if (threshold_vv > 0.95) {
      result$tests[i] <- paste("P(Baseline", decision_algo, "Algorithm) >", prob, sep = " ")
    } else if (result[i, "rope"] > 0.95) {
      result$tests[i] <- cat("P(Baseline = Algorithm) >", prob)
    } else {
      result$tests[i] <- "no decision"
    }
  }
  return(result)
}
test <- get_probabilities(test, compare = "equal")  
test  



#-------------------------------------------------------------------------------
## get_threshold