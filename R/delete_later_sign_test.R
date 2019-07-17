#' @title McNemar test for counts of data
#'
#' @export
#' @description This function performs the McNemar test
#' @param matrix Matrix of data
#' @examples
#' mcNemar.test(matrix(c(10,4,2,7), ncol=2))
#' @return A list with pvalues for alternative hypothesis, statistics, method and data name
mcNemar.test <- function(matrix){
  checkCountDataConditions(matrix)
  
  # Compute statistics
  S <- matrix[1,2] + matrix[2,1]
  Z <- (matrix[1,2] - matrix[2,1] + 0.5) / sqrt(matrix[1,2] + matrix[2,1])
  T <- (matrix[1,2] - matrix[2,1]) * (matrix[1,2] - matrix[2,1]) / (matrix[1,2] + matrix[2,1])
  
  exact.pvalue <- stats::pbinom(matrix[1,2], S,  0.5)
  asymptotic.normal.pvalue <- stats::pnorm(Z)
  asymptotic.chi.pvalue <- 1 - stats::pchisq(T, 1)
  
  pvalues <- c("Exact p-value" = exact.pvalue,
               "Asymtotic Normal p-value" = asymptotic.normal.pvalue,
               "Asymtotic Chi p-value" = asymptotic.chi.pvalue)
  
  htest <- list(data.name = deparse(substitute(matrix)),
                statistic = c("S" = S, "Z" = Z, "T"=T), p.value = pvalues,
                method = "McNemar")
  return(htest)
}



#' @title Sign test for one sample
#'
#' @export
#' @description This function performs the Sign test
#' @param matrix Sequence of data
#' @return A list with pvalues for alternative hypothesis, statistics, method and data name
binomialSign.test <- function(matrix){
  if(length(dim(matrix)) == 1 || is.vector(matrix)){
    java.test.object <- rJava::.jnew("javanpst.tests.oneSample.signTest.SignTest",
                                     numericSequence(matrix))
    method <- "Binomial Sign test for One Sample"
  }
  else{
    java.test.object <- rJava::.jnew("javanpst.tests.oneSample.signTest.SignTest",
                                     dataTable(matrix))
    method <- "Binomial Sign test"
  }
  
  rJava::.jcall(java.test.object, "V", "doTest")
  statistic <- c("K" = rJava::.jcall(java.test.object, "D", "getK"),
                 "K2" = rJava::.jcall(java.test.object, "D", "getK2"))
  pvalue <- c("Exact P-Value (Left tail, Y > X)" = rJava::.jcall(java.test.object, "D", "getExactLeftPValue"),
              "Exact P-Value (Right tail, Y < X)" = rJava::.jcall(java.test.object, "D", "getExactRightPValue"),
              "Exact P-Value (Double tail, Y != X)" = rJava::.jcall(java.test.object, "D", "getExactDoublePValue"),
              "Asymptotic P-Value (Left tail, Y > X)" = rJava::.jcall(java.test.object, "D", "getLeftPValue"),
              "Asymptotic P-Value (Right tail, Y < X)" = rJava::.jcall(java.test.object, "D", "getRightPValue"),
              "Asymptotic P-Value (Double tail, Y != X)" = rJava::.jcall(java.test.object, "D", "getDoublePValue"))
  htest <- list(data.name = deparse(substitute(matrix)),
                statistic = statistic, p.value = pvalue,
                method = method)
  return(htest)
}


## wie sieht die Matrix aus die aufgebaut wird um den test anzuwenden?=? 

EBO <- unlist(select(filter(cec17.final, Algorithm == "EBO", Dimension == 10), Result), use.names = F)
jSO <- unlist(select(filter(cec17.final, Algorithm == "jSO", Dimension == 10), Result), use.names = F)



sign.results <- rNPBST::binomialSign.test(cbind(EBO, jSO))
sign.results


ApplyNPPairwise <- function(data, test){
  split(data, data$Dimension) %>%
    lapply(function(x){
      y <- test(matrix(c(unlist(select(filter(x, Algorithm == "EBO"), Result)),
                         unlist(select(filter(x, Algorithm == "jSO"), Result))),
                       ncol = 2))$p.value[3]
    }) %>%
    unlist
}


results.sign <- ApplyNPPairwise(cec17.final, rNPBST::binomialSign.test)
results.sign


?binom.test




#-------------------------------------------------------------------------------

nemenyiTest <- function (data, alpha=0.05) {
  k <- dim(data)[2]
  N <- dim(data)[1]
  cd <- getNemenyiCD (alpha=alpha, num.alg=k, num.problems=N)
  
  mean.rank <- colMeans(rankMatrix(data))
  pairs <- generatePairs(k, control=NULL)
  
  differences <- apply(pairs, MARGIN=1, 
                       FUN=function(x) {
                         mean.rank[x[1]] - mean.rank[x[2]]
                       })
  difference.matrix <- matrix(rep(0, k^2), ncol=k)
  difference.matrix[pairs] <- differences
  difference.matrix[pairs[,c(2,1)]] <- differences
  colnames(difference.matrix) <- rownames(difference.matrix)
  colnames(difference.matrix) <- colnames(data)
  
  names(cd)<-"Critical difference"
  parameter <- c(k, (N - 1) * k)
  names(parameter) <- c("k","df")
  method <- "Nemenyi test"
  data.name <- deparse(substitute(data))
  htest.results <- list(statistic=cd, parameter=parameter, method=method, 
                        data.name=data.name, diff.matrix=difference.matrix)
  class(htest.results)<-"htest"
  return(htest.results)
}

nemenyi_test <- function(df, measure = NULL, alpha = 0.05) {
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  algo_names <- unique(df$algorithm)
  data_wide <- spread(df, algorithm, measure)
  sum_data <- aggregate(data_wide[, algo_names],
                        by = list(data_wide[["problem"]]), FUN = mean)
  # define dataset
  data <- data.frame(sum_data[,-1], row.names=sum_data[,1])
  # Nemenyi post hoc test 
  n_test <- scmamp::nemenyiTest(data, alpha)
  result <- list()
  result$measure <- measure
  result$method <- n_test$method
  result$parameter <- n_test$parameter
  result$diff.matrix <- n_test$diff.matrix
  result$statistic <- n_test$statistic
  result$p_value <- n_test$p.value
  result$significance <- abs(n_test$diff.matrix) > n_test$statistic
  return(result)
}


nemenyi_test(df= test_benchmark)




abs(test$diff.matrix) > test$statistic


#-------------------------------------------------------------------------------
friedman_test <- function(df, measure = NULL) {
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  algo_names <- unique(df$algorithm)
  data_wide <- spread(df, algorithm, measure)
  sum_data <- aggregate(data_wide[, algo_names],
                        by = list(data_wide[["problem"]]), FUN = mean)
  # define dataset
  data <- data.frame(sum_data[,-1], row.names=sum_data[,1])
  # Friedman Test 
  f_test <- scmamp::friedmanTest(data)
  ## return results 
  result <- list()
  result$measure <- measure
  result$method <- f_test$method
  result$statistic <- f_test$statistic
  result$p_value <- f_test$p.value
  result$parameter <- f_test$parameter
  
  return(result)
}

test <- friedman_test(df = test_benchmark)
test




#-------------------------------------------------------------------------------


friedmanTest(data.gh.2008)
imanDavenportTest(data.gh.2008)
friedmanAlignedRanksTest(data.gh.2008)
quadeTest(data.gh.2008)





test <- nemenyiTest (data.gh.2008, alpha=0.05)
test
test$diff.matrix
abs(test$diff.matrix) > test$statistic
