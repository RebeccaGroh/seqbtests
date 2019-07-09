context("helpers")

# check if get_main_columns() works correctly for test_benchmark 
test_that("get_main_columns for test_benchmark", {
  columns = get_main_columns(test_benchmark)
  expect_output(str(columns), "chr [1:3]", fixed = TRUE)
})

# check if get_parameter_columns() works correctly for test_benchmark 
test_that("get_parameter_columns() for test_benchmark", {
  columns = get_parameter_columns(test_benchmark)
  expect_match(columns, "parameter_algorithm")
})


#  check if get_measure_columns works correctly for test_benchmark
test_that("get_measure_columns for test_benchmark", {
  columns = get_measure_columns(test_benchmark)
  expect_match(columns, "measure_mmce")
})


# check if get_main_columns_count works correctly for test_benchmark 
test_that("get_main_columns_count for test_benchmark", {
  count = get_main_columns_count(test_benchmark)
  expect_equal(count, 3)
})


# check if get_parameter_columns_count works correctly for test_benchmark
test_that("get_parameter_columns_count for test_benchmark", {
  count = get_parameter_columns_count(test_benchmark)
  expect_equal(count, 1)
})


# check if get_measure_columns_count works correctly for test_benchmark
test_that("get_measure_columns_count for test_benchmark", {
  count = get_measure_columns_count(test_benchmark)
  expect_equal(count, 1)
})

# 
test_that("get_replications", {
  data <- get_replications(df = )
})
##  muss man hier auch mit einem realen Datensatz arbeiten?? 

get_replications <- function(i, df) {
  df[df[["replications"]] <= i, ]
}
data <- get_replications(df= test_benchmark, i = 10)
