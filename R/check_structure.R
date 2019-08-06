#' @title Check column names 
#' @description 
#'   Check if column names that were passed by User
#'   are contained in the data frame. 
check_names <- function(df, problemset = NULL, baseline = NULL, learner_a = NULL, learner_b = NULL, measure = NULL, parameter_algorithm = NULL) {
    if (!is.null(problemset)) {
        checkmate::assert_true(problemset %in% df[["problem"]])
    }
    if (!is.null(baseline)) {
        checkmate::assert_true(baseline %in% df[["algorithm"]])
    }
    if (!is.null(learner_a)) {
        checkmate::assert_true(learner_a %in% df[["algorithm"]])
    }
    if (!is.null(learner_b)) {
        checkmate::assert_true(learner_b %in% df[["algorithm"]])
    }
    if (!is.null(measure)) {
        checkmate::assert_true(measure %in% colnames(df))
    }
    if (!is.null(parameter_algorithm)) {
        checkmate::assert_true(parameter_algorithm %in% df[["parameter_algorithm"]])
    }
    return(TRUE)
}



#' @title Check Columns of Data Frame 
#' @description 
#'   Check if data frame only contains legit columns
#'   and whether the compulsory columns exist. 
#' @param df input data frame
#' @return TRUE if test was successful 
#' @export
check_column_names <- function(df) {
    checkmate::assert_true(get_main_columns_count(df) + get_measure_columns_count(df) + get_parameter_columns_count(df) == 
        ncol(df))
    checkmate::assert_true("problem" %in% colnames(df))
    checkmate::assert_true("algorithm" %in% colnames(df))
    checkmate::assert_true("replications" %in% colnames(df))
    return(TRUE)
}


#' @title Check Structure 
#' @description 
#'   Check if the structure of the data frame satisfies the requirements. 
#' @param df input data frame 
#' @return TRUE if test was successful 
#' @export
check_structure <- function(df) {
    # check column and row count: at least 4 cols (problem, algorithm, replications, 1x measure_) and minimum of 1 row
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
    # All checks passed. Return true
    return(TRUE)
}

