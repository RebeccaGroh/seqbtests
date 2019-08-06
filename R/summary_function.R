
#' @title Summary
#' @description 
#'   Short summary of the data frame, 
#'   including the columns names and number of rows
#' @param df input data frame
#' @return A vector containing the columns names and number of rows
#' @export
data_summary <- function(df) {
    rows <- nrow(df)
    columns <- colnames(df)
    return(list(Rows = rows, Columns = columns))
}


#' @title NA Check
#' @description 
#' Check if the measure column is complete. For values of one of the other 
#' columns this function shows the Ratio of NAs existing in the measure column. 
#' If there are any NAs the User can decide to drop all observations for that 
#' specific value, since the data frame needs to be complete for testing. 
#' @param df input data frame 
#' @param measure Measure column 
#' @param check_var Column in data frame used to check for NAs
#' @return List of Cases, NAs and the NA ratio according to the check_var values. 
#' @export 
na_check <- function(df, measure, check_var) {
    if (any(is.na(df))) {
        # incomplete columns
        for (x in get_main_columns(df)) {
            checkmate::assert_false(anyNA(df[[x]]))
        }
        requireNamespace("dplyr")
        check_var <- enquo(check_var)
        print(check_var)
        measure <- enquo(measure)
        print(measure)
        # Share of NAs in Measure Columns
        count <- df %>% group_by(!!check_var) %>% summarise(na_count = sum(is.na(!!measure)), cases_count = n())
        na_dataframe <- data.frame(count)
        # na_dataframe$cases_count <- col_count$cases_count
        ratio <- (na_dataframe$na_count/na_dataframe$cases_count)
        na_dataframe$na_ratio <- paste0(round(ratio * 100, digits = 2), "%", sep = "")
        result <- na_dataframe
    } else {
        result <- "data complete"
    }
    return(result)
}

#' @title Drop NAs by groups 
#' @description 
#'   Drop rows that contain any NA depending on values of check_var
#' @param df input data frame 
#' @param measure Measure column 
#' @param check_var Column in data frame used to check for NAs
#' @return New data frame without NAs. 
#' @export 
na_drop <- function(df, check_var, measure) {
    x <- deparse(substitute(check_var))
    y <- deparse(substitute(measure))
    df[!(df[[x]] %in% df[[x]][is.na(df[[y]])]), ]
}

