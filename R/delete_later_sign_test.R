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
