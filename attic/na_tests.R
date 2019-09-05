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
na_check(df = data_with_na)

na_drop <- function(df, check_var = NULL, measure = NULL) {
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  if (is.null(check_var)) {
    check_var <- "problem"
  }
  df[!(df[, check_var] %in% df[, check_var][is.na(df[, measure])]), ]
}
 
complete_data <- na_drop(df= data_with_na)
