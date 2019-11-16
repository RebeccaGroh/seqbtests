#' Benchmark Data 
#' 
#' Benchmark Data for multiple algorithms on a number of different data sets. 
#'
#' @format A data frame with 2500 rows and 5 variables:
#' \describe{
#'   \item{problem}{Problem set, where the data is tested on}
#'   \item{algorithm}{ML algorithms}
#'   \item{parameter_algorithm}{Parameter that further defines the ML algorithm}
#'   \item{replication}{Number of replication, generated through sub-sampling}
#'   \item{measure_col}{Performance of algorithm measured with accuracy}
#' }
#' @source Time series classification and regression with mlR. 