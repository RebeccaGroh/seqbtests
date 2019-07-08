context()

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


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

get_main_columns(df)


test_that("get_main_columns", {
  expect
})