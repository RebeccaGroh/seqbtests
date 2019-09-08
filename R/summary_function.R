#' @title Summary
#' @description Short summary of the data frame, 
#'     including the columns names and number of rows.
#' @param df Input data frame.
#' @return A vector containing the columns names and number of rows.
#' @export
data_summary <- function(df) {
    rows <- nrow(df)
    columns <- colnames(df)
    return(list(Rows = rows, Columns = columns))
}


#' @title NA Check
#' @description 
#'     Check if the measure column is complete. 
#'     For the problem sets and all algorithms present in the data frame, this 
#'     function specifies the ratio of existing NAs. If there are any NAs the 
#'     User can decide to drop all observations for that specific value, since 
#'     the data frame needs to be complete for testing. 
#' @param df Input data frame.
#' @param measure Measure column.
#' @param check_var Column in data frame used to check for NAs. Either 
#'     "problem" (default) or "algorithm". 
#' @return List of Cases, NAs and the NA ratio according to check_var. 
#' @export 
na_check <- function(df, measure = NULL, check_var = NULL){
  result <- data.frame()
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  if (is.null(check_var)) {
    check_var <- "problem"
  }
  if (any(is.na(df))) {
    values <- unique(df[, check_var])
    for (i in as.character(values)) {
      value_data <- subset(df, df[, check_var] == i)
      result[i, "na_number"] <- sum(is.na(value_data[, measure]))
      result[i, "observations"] <- length(which(df[, check_var] == i))
      result[i, "na_ratio"] <- 
        (result[i, "na_number"]/result[i, "observations"])
    }
  } else {
    result <- "data complete"
  }
  return(result)
}


#' @title Drop NAs by groups 
#' @description 
#'     Drop group of rows that contain any NA depending on values of check_var. 
#' @param df Input data frame.
#' @param measure Measure column.
#' @param check_var Column in data frame used to check for NAs. Either 
#'     "problem" (default) or "algorithm".
#' @return New data frame without NAs. 
#' @export 
na_drop <- function(df, check_var = NULL, measure = NULL) {
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  if (is.null(check_var)) {
    check_var <- "problem"
  }
  df[!(df[, check_var] %in% df[, check_var][is.na(df[, measure])]), ]
}

