

#' Benchmark Data 
#' 
#' Benchmark Data for multiple algorithms on a number of different data sets. 
#' (`test_benchmark`)
#'
#' @name test_benchmark
#' @references [seqbtests::test_benchmark]
#' @format A data frame with 2500 rows and 5 variables:
#' \describe{
#'   \item{problem}{Problem set, where the data is tested on}
#'   \item{algorithm}{ML algorithms}
#'   \item{parameter_algorithm}{Parameter that further defines the ML algorithm}
#'   \item{replication}{Number of replication, generated through sub-sampling}
#'   \item{measure_col}{Performance of algorithm measured with accuracy}
#' }
#' @docType data
#' @source Time series classification and regression with mlR. 
NULL


#' Benchmark Data 
#' 
#' Benchmark Data for multiple algorithms on a number of different data sets. 
#' (`test_benchmark_small`)
#' 
#' @name test_benchmark_small
#' @references [seqbtests::test_benchmark_small]
#' @format A data frame with 2500 rows and 5 variables:
#' \describe{
#'   \item{problem}{Problem set, where the data is tested on}
#'   \item{algorithm}{ML algorithms}
#'   \item{replication}{Number of replication, generated through sub-sampling}
#'   \item{measure_col}{Performance of algorithm measured with accuracy}
#' }
#' @docType data
#' @source Time series classification and regression with mlR. 
NULL