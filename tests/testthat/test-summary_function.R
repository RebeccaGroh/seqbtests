context("summary_function")


# check if data_summary() returns list 
test_that("data_summary returns list", {
  expect_type(data_summary(test_benchmark), "list")
})

  
## check if na_check() returns list for data frame containing NAs 
#test_that("na_check returns list", {
#  expect_type(na_check(benchmark_test), "list")
#})
# no incomplete datat frame in Github
  

# check if na_check() returns character for complete data frame 
test_that("na_check returns character", {
  expect_type(na_check(df = test_benchmark, 
                       measure = measure_col, 
                       check_var = algorithm), "character")
})


# 

# check if na_drop() returns list 
test_that("na_drop reurns list", {
  expect_type(na_drop(df = test_benchmark, 
                      measure = measure_col, 
                      check_var = algorithm), "list")
})

