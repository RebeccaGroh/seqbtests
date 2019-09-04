df <- test_benchmark_small
measure <- "measure_col"
variables <- c("algorithm", "problem")
i <- 1
for (i in variables) {
  checking <- levels(df[["algorithm"]])
}


# check amount of NAs in problem sets and algorithms
algorithms <- levels(df[["algorithm"]])
problems <- levels(df[["problem"]])
variables <- list(algorithms, problems)
for (i in variables) {
  for (k in i) {
    ###################################################################
  }
}



# Share of NAs in Measure Columns
count <- df %>% group_by(!!check_var) %>%
  summarise(na_count = sum(is.na(!!measure)), cases_count = n())
na_dataframe <- data.frame(count)
# na_dataframe$cases_count <- col_count$cases_count
ratio <- (na_dataframe$na_count/na_dataframe$cases_count)
na_dataframe$na_ratio <- paste0(round(ratio * 100, digits = 2),
                                "%", sep = "")
result <- na_dataframe
sum(is.na 1.4k(z$Ozone))

################################################################################
df <- data_with_na
measure <- "measure_col"
result <- data.frame()
algorithms <- unique(df[["algorithm"]])
problems <- levels(df[["problem"]])
variables <- c(algorithms, problems)
for (k in variables) {
  for (i in algorithms) {
    result[i, "na_number"] <- sum(is.na(df[["measure"]]))
    result[i, "observations"] <- length(which(df[["algorithm"]] == i))
    result[i, "na_ratio"] <- (result[i, "na_number"]/result[i, "observations"])
  }
}



################################################################################
na_check <- function(df, measure = NULL) {
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  if (any(is.na(df))) {
    # incomplete columns
    for (x in get_main_columns(df)) {
      checkmate::assert_false(anyNA(df[[x]]))
    }
    result <- data.frame()
    algorithms <- unique(df[["algorithm"]])
    problems <- levels(df[["problem"]])
    variables <- c(algorithms, problems)
    for (k in variables) {
      if(k == algorithms) {
        length_rows <- length(which(df[["algorithm"]] == k))
      } else if(k == problems) {
        length_rows <- length(which(df[["problem"]] == k))
      }
      for (i in k) {
        result[i, "na_number"] <- sum(is.na(df[["measure"]]))
        result[i, "observations"] <- length_rows
        result[i, "na_ratio"] <- (result[i, "na_number"]/result[i, "observations"])
      }
    }
  } else {
    result <- "data complete"
  }
  return(result)
}
na_check(df = data_with_na)


## muss alles angepasst werden, damit die Funktion läuft 
