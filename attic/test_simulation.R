library(data.table)

# mu werte jeweils als Problemsets verwenden 
generate_data <- function(num, mu, delta_mean, sigma){
  data <- data.frame(algorithm = rep(c("algo_a", "algo_b"), length = num*2), 
                     measure_accuracy = rnorm(n = num*2, mean = c(mu, mu + delta_mean), 
                                              sd = sigma))
  data$mu <- mu 
  for (i in unique(data$algorithm)) {
    data$replications[data$algorithm == i] <- seq_len(sum(data$algorithm == i))
  } 
  return(data)
}
# test_df$problem <- paste("problem_", 1, sep = "")


test_df_1 <- generate_data(num = 30, mu = 0.4, delta_mean = 0.2, sigma = 0.2)
test_df_2 <- generate_data(num = 30, mu = 0.5, delta_mean = 0.2, sigma = 0.2)
test_df_3 <- generate_data(num = 30, mu = 0.6, delta_mean = 0.2, sigma = 0.2)
data <- do.call("rbind", list(test_df_1, test_df_2, test_df_3))


setDT(data)[, id := .GRP, by = mu]
data$problem <- paste("problem_", data$id, sep = "")




#-- relevanter Code ------------------------------------------------------------
test_result <- data.frame()
for (start_iter in c(5, 10, 30)) {
  for (j in 1:5) {
    delta_mean_all <- c(0, 0.01, 0.05, 0.1, 0.2)
    delta_mean <- delta_mean_all[j]
    for (k in 1:5) {
      sigma_all <- c(0.01, 0.02, 0.05, 0.1, 0.2)
      sigma <- sigma_all[k]
      set.seed(123456)
      df_1 <- generate_data(num = 30, mu = 0.4, delta_mean, sigma)
      df_2 <- generate_data(num = 30, mu = 0.5, delta_mean, sigma)
      df_3 <- generate_data(num = 30, mu = 0.6, delta_mean, sigma)
      data <- do.call("rbind", list(df_1, df_2, df_3))
      setDT(data)[, id := .GRP, by = mu]
      data$problem <- paste("problem_", data$id, sep = "")
      df <- subset(data, select = -c(mu, id))
      out_seq <- seq_b_signed_rank_test(df = df, baseline = "algo_a", 
                                        min_repls = start_iter, max_repls = 30)
      row_id <- paste(start_iter, mu, delta_mean, sigma, sep = "_")
      test_result[row_id, "algorithm"] <- out_seq$data_frame[1]
      test_result[row_id, "left"] <- out_seq$data_frame[2]
      test_result[row_id, "rope"] <- out_seq$data_frame[3]
      test_result[row_id, "right"] <- out_seq$data_frame[4]
      test_result[row_id, "repls"] <- out_seq$data_frame[5]
      test_result[row_id, "probabilities"] <- out_seq$data_frame[6]
      test_result[row_id, "delta"] <- delta_mean
      test_result[row_id, "sigma"] <- sigma
      test_result[row_id, "start_iter"] <- start_iter
    }
  }
}


#-- relevanter Code ------------------------------------------------------------
# (läuft gerade)



test_df_1 <- generate_data(num = 30, mu = 0.4, delta_mean = 0.2, sigma = 0.2)
test_df_2 <- generate_data(num = 30, mu = 0.5, delta_mean = 0.2, sigma = 0.2)
test_df_3 <- generate_data(num = 30, mu = 0.6, delta_mean = 0.2, sigma = 0.2)
data <- do.call("rbind", list(test_df_1, test_df_2, test_df_3))


setDT(data)[, id := .GRP, by = mu]
data$problem <- paste("problem_", data$id, sep = "")




test_result <- data.frame()
for (start_iter in 2:30) {
    for (j in 1:5) {
      delta_mean_all <- c(0, 0.01, 0.05, 0.1, 0.2)
      delta_mean <- delta_mean_all[j]
      for (k in 1:5) {
        sigma_all <- c(0.01, 0.02, 0.05, 0.1, 0.2)
        sigma <- sigma_all[k]
        set.seed(123456)
        df_1 <- generate_data(num = 30, mu = 0.4, delta_mean, sigma)
        df_2 <- generate_data(num = 30, mu = 0.5, delta_mean, sigma)
        df_3 <- generate_data(num = 30, mu = 0.6, delta_mean, sigma)
        data <- do.call("rbind", list(df_1, df_2, df_3))
        setDT(data)[, id := .GRP, by = mu]
        data$problem <- paste("problem_", data$id, sep = "")
        df <- subset(data, select = -c(mu, id))
        out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a",
          min_repls = start_iter, max_repls = 30, problem = "problem_1")
        row_id <- paste(start_iter, mu, delta_mean, sigma, sep = "_")
        test_result[row_id, "algorithm"] <- out_seq$data_frame[1]
        test_result[row_id, "left"] <- out_seq$data_frame[2]
        test_result[row_id, "rope"] <- out_seq$data_frame[3]
        test_result[row_id, "right"] <- out_seq$data_frame[4]
        test_result[row_id, "repls"] <- out_seq$data_frame[5]
        test_result[row_id, "probabilities"] <- out_seq$data_frame[6]
        test_result[row_id, "mu"] <- mu
        test_result[row_id, "delta"] <- delta_mean
        test_result[row_id, "sigma"] <- sigma
        test_result[row_id, "start_iter"] <- start_iter
      }
    }
}


out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a",
                             problem = "problem_1", min_repls = start_iter, max_repls = 30)
row_id <- paste(start_iter, mu, delta_mean, sigma, sep = "_")
test_result[row_id, "algorithm"] <- out_seq$data_frame[1]
test_result[row_id, "left"] <- out_seq$data_frame[2]
test_result[row_id, "rope"] <- out_seq$data_frame[3]
test_result[row_id, "right"] <- out_seq$data_frame[4]
test_result[row_id, "repls"] <- out_seq$data_frame[5]
test_result[row_id, "probabilities"] <- out_seq$data_frame[6]
test_result[row_id, "mu"] <- mu
test_result[row_id, "delta"] <- delta_mean
test_result[row_id, "sigma"] <- sigma
test_result[row_id, "start_iter"] <- start_iter
