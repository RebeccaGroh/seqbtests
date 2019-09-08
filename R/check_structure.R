#' @title Check column names 
#' @description Check if column names that were passed by User are contained 
#'     in the data frame. 
#' @param df Input data frame.
#' @param problem Problem set on which the test should be performed. 
#' @param baseline First algorithm.
#' @param algorithm Second algorithm. If not defined, every algorithm will be 
#'     tested against baseline. 
#' @param measure Measure column. 
#' @export
check_names <- function(df, problem = NULL, baseline = NULL, 
                        algorithm = NULL, measure = NULL) {
    if (!is.null(problem)) {
        checkmate::assert_true(problem %in% df[["problem"]])
    }
    if (!is.null(baseline)) {
        checkmate::assert_true(baseline %in% df[["algorithm"]])
    }
    if (!is.null(algorithm)) {
        checkmate::assert_true(algorithm %in% df[["algorithm"]])
    }
    if (!is.null(measure)) {
        checkmate::assert_true(measure %in% colnames(df))
    }
    return(TRUE)
}



#' @title Check Columns of data frame 
#' @description Check if data frame only contains legit columns and whether any
#'     compulsory columns exist. 
#' @param df Input data frame.
#' @return TRUE if test was successful.
#' @export
check_column_names <- function(df) {
    checkmate::assert_true(get_main_columns_count(df) + 
                             get_measure_columns_count(df) + 
                             get_parameter_columns_count(df) == ncol(df))
    checkmate::assert_true("problem" %in% colnames(df))
    checkmate::assert_true("algorithm" %in% colnames(df))
    checkmate::assert_true("replications" %in% colnames(df))
    return(TRUE)
}


#' @title Check Structure 
#' @description Check if the structure of the data frame satisfies the 
#'     requirements. 
#' @param df Input data frame,
#' @return TRUE if test was successful.
#' @export
check_structure <- function(df) {
    # check column and row count: at least 4 cols (problem, algorithm, 
    # replications, 1x measure_) and minimum of 1 row
    checkmate::assert_data_frame(df, min.rows = 1, min.cols = 4)
    # check number of measures is at least 1
    checkmate::assert_true(get_measure_columns_count(df) >= 1)
    # check if data frame contains not allowed columns
    checkmate::assert_true(check_column_names(df))
    # Check basic structure
    for (x in get_main_columns(df)) {
        checkmate::assert_false(anyNA(df[[x]]))
    }
    for (x in get_parameter_columns(df)) {
        checkmate::assert_false(anyNA(df[[x]]))
    }
    for (x in get_measure_columns(df)) {
        checkmate::assert_false(anyNA(df[[x]]))
        checkmate::assert_true(is.numeric(df[[x]]))
    }
    return(TRUE)
}


#' @title Paste algorithm and parameter
#' @description If there is a parameter additional to the algorithm, both can 
#'     be combined, when using this function. 
#' @param df Input data frame,
#' @param parameter_algorithm Algorithm parameter in data frame. 
#' @return New data frame, where the algorithm and parameter_algorithm column, 
#'     as well as the value names are combined. 
#' @export 
paste_algo_pars <- function(df, parameter_algo = NULL) {
  df[["algorithm"]] <- 
    paste(df[["algorithm"]], df[["parameter_algorithm"]], sep = "_")
  if (!is.null(parameter_algo)) {
    df <- subset(df, df[["parameter_algorithm"]] == parameter_algo , 
                 select = -c(parameter_algorithm))
  } else {
    df <- subset(df, select = -c(parameter_algorithm))  
  }
}



#' @title Table test results (Bayesian tests)
#' @description Create a list containing the Bayesian test results based on a 
#'     generic function.
print.b_test <- function(x, ...) {
  cat( "\n", "Results of the", x$method, "\n", 
      "Measure column =", x$measure, "\n", 
      "Baseline algorithm =", x$baseline, "\n", "\n" )
  row.names(x$data_frame) <- NULL
  output_data <- x$data_frame
  print(output_data)
}

#' @title Table test results (Frequentist tests)
#' @description Create a list containing the Frequentist test results based on a 
#'     generic function.
print.h_test <- function(x, ...) {
  cat( "\n", "Results of the", x$method, "\n", 
       "Measure column =", x$measure, "\n", 
       "Baseline algorithm =", x$baseline, "\n", "\n" )
  row.names(x$data_frame) <- NULL
  output_data <- x$data_frame
  print(output_data)
}

#' @title Table test results (Frequentist tests)
#' @description Create a list containing the Frequentist test results based on a 
#'     generic function. Not containing the Baseline. 
print.h_test_small <- function(x, ...) {
  cat( "\n", "Results of the", x$method, "\n", 
       "Measure column =", x$measure, "\n", "\n" )
  row.names(x$data_frame) <- NULL
  output_data <- x$data_frame
  print(output_data)
}

#' @title Table test results (Nemenyi tests)
#' @description Create a list containing the Nemenyi test results based on a 
#'     generic function. 
print.nemenyi <- function(x, ...){
  cat( "\n", "Results of the", x$method, "\n", 
       "Measure column =", x$measure, "\n", "\n" )
  row.names(x$data_frame) <- NULL
  output_data <- x$data_frame
  output_matrix <- x$matrix
  print(list(output_data, output_matrix))
}

