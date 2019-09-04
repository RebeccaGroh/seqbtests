

data_with_na <- test_benchmark_small
data_with_na$measure_col[1] <- NA 


na_check(df = data_with_na, measure = measure_col, check_var = "problem")
