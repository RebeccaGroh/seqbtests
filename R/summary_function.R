
#' @title Summary
#' @description 
#' Short summary of the data frame, 
#' including the columns names and number of rows
#' @param df input data frame
#' @return A vector containig the columns names and number of rows
#' @export
data_summary <- function(df){
  rows <- nrow(df)
  columns <- colnames(df)
  return(list(Rows = rows, Columns = columns))
}

#' @title Drop NAs by groups 
#' @description 
#' Drop rows that contain any NA depending on groups
#' @param 
#' df input data frame 
l#' measure Variable containing measure values 
#' @return new Data Frame without NAs 
#' @export 
na_drop <- function(df, group ,measure) {
  x <- deparse(substitute(group))
  y <- deparse(substitute(measure))
  df[!(df[[x]] %in% df[[x]][is.na(df[[y]])]), ]
}

#' @title NA Check
#' @description 
#' Checks if there are any NA's in the dataframe 
#' @param df input data frame 
#' @return ??
#' @export 
#' 
na_check <- function(df, measure, var_test) {
  if (any(is.na(df))) {
    # incomplete columns 
    for (x in get_main_columns(df)) {
      checkmate::assert_false(anyNA(df[[x]]))
    }    
    checkmate::assert_true(require(dplyr))
    var_test <- enquo(var_test)
    print(var_test)
    measure <- enquo(measure)
    print(measure)
    # Share of NAs in Measure Columns 
    count <- df %>% group_by(!! var_test) %>% summarise(na_count = sum(is.na(!! measure)), cases_count = n())
    na_dataframe <- data.frame(count)
    # na_dataframe$cases_count <- col_count$cases_count
    na_dataframe$na_ratio <- paste0(round((na_dataframe$na_count/na_dataframe$cases_count)*100,digits = 2),"%",sep = "")
    result <- na_dataframe
  }else {
    result <- "data complete"
  }
  return(result)
}