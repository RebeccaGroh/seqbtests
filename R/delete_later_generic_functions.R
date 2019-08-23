


#------------------------------------------------------------------------------#

df = test_benchmark_small
rho=0.1
rope=c(-0.01, 0.01)
problemset = 'problem_e' 
baseline = 'algo_1'
learner_b = "algo_2"
measure = c()

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
  
y <- df[df[["problem"]] == problemset 
        & df[["algorithm"]] == learner_b, measure]

# Bayesian correlated t Test
b_corr <- scmamp::bCorrelatedTtest(x, y, rho, rope)
# results
#result[k, "baseline"] <- baseline
#result[k, "method"] <- b_corr$method
#result[k, "measure"] <- measure
#
result[learner_b, "algorithm"] <-learner_b
result[learner_b, "left"] <- b_corr$posterior.probabilities[1]
result[learner_b, "rope"] <- b_corr$posterior.probabilities[2]
result[learner_b, "right"] <- b_corr$posterior.probabilities[3]
    
    #
#result
## alles in einer Liste sammeln: 
get_test_results <- function(baseline, method, measure, data = NULL, extra = NULL) {
 output <- list(baseline = baseline, 
               method = method, 
                 measure = measure, 
                 data_frame = data, 
                 extra = extra)
  class(output) <- "Btest"
  output
}

output_test <- get_test_results(baseline, measure, method = b_corr$method, data = result)
output_test
#-------------------------------------------------------------------------------
format_test <- function(baseline, method, measure, data = NULL, extra = NULL) {
  UseMethod("format_test")
}
   
  
format_test.Btest <- function(test) {
  cat("\t", "\t", "\t", "Results of the", output_test$method, "\n", "\n",
      "Measure column = ", output_test$measure, "\t", "\t", "\t", "\t",
      "Baseline algorithm = ", output_test$baseline, "\n", "\n") 
  row.names(output_test$data_frame) <- NULL
  print(output_test$data_frame)
}
testen <- format_test(output_test)
testen



format_test.Btest <- function(test) {
  #text1 <- sprintf("Results of the %s", test$method)
  #text2 <- sprintf("Measure column = %s", test$measure)
  #text3 <- sprintf("Baseline algorithm = %s", test$baseline)
  #text <- rbind(text1, text2, text3)
  #row.names(text) <- NULL
  #text <- sprintf("Results of the %s \n Measure column = %s \n
  #                Baseline algorithm = %s", test$method, test$measure, test$baseline)
  text <- paste("Results of the",  test$method, "\n", 
                "Measure column =", test$measure, "\n", 
                "Baseline algorithm =", test$baseline, "\n")
  row.names(test$data_frame) <- NULL
  output_data <- test$data_frame
  return(list(text, output_data))
}
testen <- format_test(output_test)
testen
## gibt es eine alternative mit cat?? 

## \n starts new line    
## to get center 
## https://stackoverflow.com/questions/26922462/r-center-output-text-in-r-console-r-studio-and-r-app 
## ?cat --> fill und label anschauen ob man damit schöner formatieren kann 
#-------------------------------------------------------------------------------   
#print.Btest <- function(x) {
#  print("Result of Bayesian Test")
#  #print(x)
#}
    
    
 