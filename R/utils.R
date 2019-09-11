#' @title Main columns 
#' @description Get the list of names of the main columns within the data frame, 
#'   namely 'problem', 'algorithm' and 'replications'. 
#' @param df Input data frame.
#' @return A vector containing all main column names.
get_main_columns <- function(df) {
    main_columns <- c("problem", "algorithm", "replications")
    return(intersect(names(df), main_columns))
}


#' @title Parameter columns 
#' @description Get the list of names for all parameter columns within the 
#'     data frame. 
#' @param df Input data frame.
#' @return A vector containing all parameter columns.
get_parameter_columns = function(df) {
    checkmate::assert_data_frame(df)
    return(subset(names(df), startsWith(names(df), "parameter_")))
}


#' @title Measure columns 
#' @description Get the list of names for all columns containing performance 
#'     measures of the algorithms within the data frame.
#' @param df Input data frame.
#' @return A vector containing all measure columns. 
get_measure_columns <- function(df) {
    checkmate::assert_data_frame(df)
    return(subset(names(df), startsWith(names(df), "measure_")))
}


#' @title Count main columns 
#' @description Count the number of main columns in the data frame. 
#' @param df Input data frame.
#' @return A numeric vector displaying the number of main columns. 
get_main_columns_count <- function(df) {
    return(length(get_main_columns(df)))
}


#' @title Count parameter columns 
#' @description Get the number parameter columns in the data frame. 
#' @param df Input data frame.
#' @return A numeric vector displaying the number of parameter columns. 
get_parameter_columns_count <- function(df) {
    return(length(get_parameter_columns(df)))
}


#' @title Count measure columns 
#' @description Get the number parameter columns in the data frame. 
#' @param df Input data frame.
#' @return A numeric vector displaying the number of columns containing 
#'     performance measures.  
get_measure_columns_count <- function(df) {
    return(length(get_measure_columns(df)))
}

#' @title Build replications
#' @description When not defined by user, it calls the replications in a 
#' complete data frame provided by the user. Can otherwise be used to build 
#' replications during testing procedure.
#' @param i Number of replications in data frame.  
#' @param df Input data frame.
#' @return Data frame containing defined the number of replications.   
get_replications <- function(i, df) {
    df[df[["replications"]] <= i, ]
}

#' @title Check number of replications 
#' @description  Check if number of observations in the first and second 
#'     sample are equal.
#' @param x First sample.
#' @param y Second sample.
#' @return TRUE if both samples are of same length. 
get_replications_count <- function(x, y) {
    checkmate::assert_true(length(x) == length(y))
}

#' @title Data transformation
#' @description Extract a data matrix out of the original dataset. The matrix 
#'     contains all observations for each replication in each problem set. 
#'     The replications are stored in the rows, while the columns are according  
#'     to the problem sets. 
#' @param df Input data frame.
#' @param algo Name of the algorithm that shall be compared. 
#' @param measure Measure column. 
#' @return Matrix. 
data_transformation <- function(df, algo, measure) {
    keep_algo <- subset(df, df[["algorithm"]] == algo)
    data_wide <- tidyr::spread(keep_algo, replications, measure)
    # columns need to be dropped
    drop_cols <- setdiff(colnames(data_wide), unique(df[["replications"]]))
    # columns to keep
    names.use <- names(data_wide)[!(names(data_wide) %in% drop_cols)]
    # subset
    subset_df <- data_wide[, names.use]
    return(subset_df)
}


#' @title Get test results (Bayesian tests)
#' @description This function collects the results of the Bayesian tests. 
#' @param baseline Baseline algorithms that every algorithm can be compared to.  
#' @param method Bayesian test. 
#' @param measure Measure column. 
#' @param data Data frame containing the posterior probabilities. 
#' @param extra Any extra arguments needed to build the plots. 
#' @return List. 
get_results <- function(baseline, method, measure, data = NULL, extra = NULL) {
    output <- list(baseline = baseline, 
                   method = method, 
                   measure = measure, 
                   data_frame = data, 
                   extra = extra)
    class(output) <- "b_test"
    return(output)
}


#' @title Get test results (Frequentist tests)
#' @description This function collects the results of the Frequentist tests. 
#' @param baseline Baseline algorithms that every algorithm can be compared to.  
#' @param method Frequentist test. 
#' @param measure Measure column. 
#' @param data Data frame containing the posterior probabilities. 
#' @param matrix A matrix with all the pair wise differences of 
#'     average rankings.
#' @return List. 
get_results_htest <- function(baseline = NULL, method, measure, data = NULL, 
                              matrix = NULL) {
    output <- list(baseline = baseline, 
                   method = method, 
                   measure = measure, 
                   data_frame = data, 
                   matrix = matrix)
    return(output)
}

#' @title Get test results (for data frame)
#' @description This function collects the part of the results shown in the data 
#'     frame. 
#' @param algorithm   
#' @param left  
#' @param rope 
#' @param right 
#' @param repls Number of replications used until a decision is made. 
#' @return List. 
get_data_frame <- function(algortihm, left, rope, right, repls = NULL) {
    output <- list(algorithm = algorithm, left = left, 
                   rope = rope, right = right, repls = repls)
    return(output)
}
