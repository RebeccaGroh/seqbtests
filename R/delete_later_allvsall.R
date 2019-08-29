#df = test_benchmark_small
#rho=0.1
#problemset = 'problem_b'
#baseline = 'algo_1'
#compare = 'equal'
#max_repls = 10
#rope=c(-0.01, 0.01)

#result = data.frame()
#i = 5
#data <- get_replications(i, df)
#measure = c()
#if (is.null(measure)) {
#  measure <- get_measure_columns(data)[1]
#}
#algorithms <- unique(data[["algorithm"]])
#if (i == 5) {     # hier können alle ausgeschlossen werden die schon dran waren 
#  liste <- c()
#}
#baseline <- liste 
#algorithms <- setdiff(algorithms, liste)
#j = 1
#baseline = NULL
#if (is.null(baseline)) {
#  baseline <- algorithms[1]
#} 
#x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]
#k = "algo_2"
## prüfen was in algorithms noch angezeigt wird und für k dann den wert verwenden 
#algorithm = c()
#if (!is.null(algorithm)) {
#  k <- algorithm
#  y <- data[data[["problem"]] == problemset 
#            & data[["algorithm"]] == k, measure]
#} else {
#  y <- data[data[["problem"]] == problemset 
#            & data[["algorithm"]] == k, measure]
#}
#b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
#number <- paste(baseline, k, sep = "_")
#result[number, "algorithm"] <- k
#result[number, "left"] <- b_test$posterior.probabilities[1]
#result[number, "rope"] <- b_test$posterior.probabilities[2]
#result[number, "right"] <- b_test$posterior.probabilities[3]
#result[number, "repls"] <- i
#result
#if (is.null(compare)) {compare <- "better"}
#if (compare == "better") { 
#  threshold <- b_test$posterior.probabilities[3]
#} else if (compare == "equal") {
#  threshold <- b_test$posterior.probabilities[2] + 
#    b_test$posterior.probabilities[3]
#} 
#if (threshold > 0.95) {
#  result[number, "significanct"] <- TRUE
#} else {
#  result[k, "significanct"] <- FALSE
#}
#liste <-  rownames(result[result[["significanct"]] == TRUE, ])
#if (result[number, "significanct"] == TRUE) {
#  break
#}
#-------------------------------------------------------------------------------

#seq_b_corr_t_test <- function(problemset, measure = NULL, compare = NULL, rho = 0.1, 
#                              rope = c(-0.01, 0.01), max_repls = 20, ...) {
#  result = data.frame()
#  for (i in 5:max_repls) {
#    data <- get_replications(i, ...)
#    ## check if passed names, define columns in dataset
#    checkmate::assert_true(check_structure(df = data))
#    checkmate::assert_true(check_names(df = data, problemset, baseline, 
#                                       algorithm = NULL, measure = NULL))
#    if (is.null(measure)) {
#      measure <- get_measure_columns(data)[1]
#    }
#    # loop für baseline 
#    algorithms <- unique(data[["algorithm"]])
#    if (i == 5) {     # hier können alle ausgeschlossen werden die schon dran waren 
#      liste <- c()
#    }
#    for (j in algorithms) {
#      
#      if (is.null(baseline)) {
#        # für jedes j in algorithms, verwende den algorithmus als Baseline, ,
#        #
#        j <- algorithms[1]
#      } 
#      
#    }
#    baseline <- liste 
#    #---------------------------------------------------------------------------
#    algorithms <- setdiff(algorithms, liste)
#    for (j in algorithms) {
#      if (is.null(baseline)) {
#        baseline <- algorithms[1]
#      }
#      # define samples
#      x <- data[data[["problem"]] == problemset & data[["algorithm"]] == baseline, measure]
#      for (k in algorithms[algorithms != baseline]) {
#        if (!is.null(algorithm)) {
#          k <- algorithm
#          y <- data[data[["problem"]] == problemset 
#                    & data[["algorithm"]] == k, measure]
#        } else {
#          y <- data[data[["problem"]] == problemset 
#                    & data[["algorithm"]] == k, measure]
#        }
#        # Bayesian correlated t Test
#        b_test <- scmamp::bCorrelatedTtest(x, y, rho, rope)
#        # results 
#        number <- paste(j, k, sep = "_")
#        result[number, "algorithm"] <- k
#        result[number, "left"] <- b_test$posterior.probabilities[1]
#        result[number, "rope"] <- b_test$posterior.probabilities[2]
#        result[number, "right"] <- b_test$posterior.probabilities[3]
#        result[number, "repls"] <- i
#        # falls einer signifikant ist, dann brich ab und mach mit diesem als 
#        # nächster Baseline weiter 
#        if (is.null(compare)) {compare <- "better"}
#        if (compare == "better") { 
#          threshold <- b_test$posterior.probabilities[3]
#        } else if (compare == "equal") {
#          threshold <- b_test$posterior.probabilities[2] + 
#            b_test$posterior.probabilities[3]
#        } 
#        if (threshold > 0.95) {
#          result[k, "significanct"] <- TRUE
#        } else {
#          result[k, "significanct"] <- FALSE
#        }
#        liste <-  rownames(result[result[["significanct"]] == TRUE, ])
#      }
#    }
#  }  
#}
