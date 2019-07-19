#' @title Main Columns 
#' @description 
#'   Get the list of names of the main columns within the data frame, 
#'   namely "problem", "algorithm" and "replications". 
#' @param df input data frame
#' @return A vector containing all main column names.
#' @export
get_main_columns <- function(df) {
  main_columns <- c("problem", "algorithm", "replications")
  return(intersect(names(df), main_columns))
}


#' @title Parameter Columns 
#' @description 
#' Get the list of names for all parameter columns within the data frame. 
#' @param df input data frame
#' @return A vector containing all parameter columns.
#' @export
get_parameter_columns = function(df) {
  checkmate::assert_data_frame(df)
  return(subset(names(df), startsWith(names(df), "parameter_")))
}


#' @title Measure Columns 
#' @description 
#' Get the list of names for all measure columns within the data frame.
#' @param df input data frame 
#' @return A vector containing all measure columns. 
#' @export
get_measure_columns <- function(df){
  checkmate::assert_data_frame(df)
  return(subset(names(df), startsWith(names(df), "measure_")))
}

## Count Columns 

#' @title Count Main Columns 
#' @description 
#' Count the number of main columns in the data frame. 
#' @param df input data frame 
#' @return A numeric vector displaying the number of main columns. 
#' @export
get_main_columns_count <- function(df){
  return(length(get_main_columns(df)))
} 


#' @title Count Parameter Columns 
#' @description 
#' Get the number parameter columns in the data frame. 
#' @param df input data frame 
#' @return A numeric vector displaying the number of parameter columns. 
#' @export
get_parameter_columns_count <- function(df){
  return(length(get_parameter_columns(df)))
}


#' @title Count Measure Columns 
#' @description 
#' Get the number parameter columns in the data frame. 
#' @param df input data frame 
#' @return A numeric vector displaying the number of columns containing measures.  
#' @export
get_measure_columns_count <- function(df){
  return(length(get_measure_columns(df)))
}

#' @title Build Replications
#' @description 
#' When not defined by user, it calls the replications in a complete dataset.  
#' Can be used to build replications during the testing.
#' @param i number of replications used 
#' @param df input data frame 
#' @return Dataframe containing defined number of replications.   
#' @export
get_replications <- function(i, df) {
  df[df[["replications"]] <= i, ]
}

#' @title Check number of replications 
#' @describtion
#' Check if number of observations in the first an second sample are equal.
#' @param x First sample.
#' @param y Second sample.
#' @return TRUE if both samples are of same length. 
#' @export
get_replications_count <- function(x, y) {
  checkmate::assert_true(length(x) == length(y))
}


#' @title Paste algorithm and parameter
#' @description 
#' If there is a parameter additional to the algorithm, both can be combined, 
#' when using this function. 
#' @param algorithm Algorithm in data frame.
#' @param parameter_algorithm Algorithm parameter in data frame. 
#' @return New algorithm name, combining algorithm and its parameter. 
#' @export 
paste_algo_pars <- function(algorithm, parameter_algorithm) {
  algorithm <- paste(algorithm, parameter_algorithm, sep = "_")
}


#' @title Data transformation
#' @description Extract a data matrix out of the original dataset. The matrix 
#' contains all observations for each replication in each problemset. 
#' The replications are stored in the rows, while the columns are according to 
#' the problemsets. 
#' @param df Input data frame. 
#' @param algo Name of the algorithm that shall be compared. 
#' @param measure Measure column. 
#' @return Matrix. 
data_transformation <- function(df, algo, measure){
  keep_algo <- subset(df, df[["algorithm"]] == algo)
  data_wide <- spread(keep_algo, replications, measure)
  # columns need to be dropped 
  drop_cols <- setdiff(colnames(data_wide), unique(df[["replications"]]))
  # columns to keep 
  names.use <- names(data_wide)[!(names(data_wide) %in% drop_cols)]
  # subset 
  subset_df <- data_wide[, names.use]
  return(subset_df)
}
