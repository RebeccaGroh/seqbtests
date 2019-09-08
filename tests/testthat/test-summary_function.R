context("summary_function")


# check if data_summary() returns list 
test_that("data_summary returns list", {
  expect_type(data_summary(test_benchmark), "list")
})

#check if na_check() returns list for data frame containing NAs 
test_that("na_check returns list", {
  data_tmp <- test_benchmark_small
  data_tmp$measure_col[1] <- NA 
  expect_type(na_check(data_tmp, check_var = "algorithm"), "list")
})

# check if na_check() returns character for complete data frame 
test_that("na_check returns character", {
  expect_type(na_check(test_benchmark, check_var = "algorithm"), "character")
})

# check if na_drop() returns list
test_that("na_drop returns list", {
  data_tmp <- test_benchmark_small
  data_tmp$measure_col[1] <- NA 
  expect_type(na_drop(df = data_tmp, check_var = "algorithm"), "list")
})

