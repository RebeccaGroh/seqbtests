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

paste_algo_pars <- function(algorithm, parameter_algorithm) {
  algorithm <- paste(algorithm, parameter_algorithm, sep = "_")
}
  
