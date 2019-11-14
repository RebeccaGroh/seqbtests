
## Test Sequential Bayesian Tests on Simualted Data ----------------------------

# Tests: Bayesian Correlated t-test, Bayesian Signed Ranks Test, Bayesian 
# Hierarchichal Correlated t-test 
# Ground Truth: When the difference between the first and second algorithm 
# (delta) differs by more than the rope 


# Set up data ------------------------------------------------------------------
# generate data depending on num (Number of Iterations), mu (mean accuracy of
# first algorithm), delta_mean (mean difference between first and second 
# algorithm), sigma (Variance of each algorithms performance)
generate_data <- function(num, mu, delta_mean, sigma){
  data <- data.frame(algorithm = rep(c("algo_a", "algo_b"), length = num*2), 
    measure_accuracy = rnorm(n = num*2, mean = c(mu, mu + delta_mean), 
      sd = sigma))
  data$problem <- "problem_1"
  for (i in unique(data$algorithm)) {
    data$replications[data$algorithm == i] <- seq_len(sum(data$algorithm == i))
  } 
  return(data)
}


#------------------------------------------------------------------------------#
# Bayesian correlated t-test ---------------------------------------------------
#------------------------------------------------------------------------------#

test_result <- data.frame()
for (start_iter in 2:30) {
  for (i in 1:3) {
    mu_all <- c(0.4, 0.5, 0.6)
    mu <- mu_all[i]
    for (j in 1:5) {
      delta_mean_all <- c(0, 0.01, 0.05, 0.1, 0.2)
      delta_mean <- delta_mean_all[j]
      for (k in 1:5) {
        sigma_all <- c(0.01, 0.02, 0.05, 0.1, 0.2)
        sigma <- sigma_all[k]
        set.seed(123456)
        df <- generate_data(num = 30, mu, delta_mean, sigma)
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
      }
    }
  }
}

b_corr <- test_result
# setwd("H:/MA/simulation_data")
# write.csv(simulation_b_corr_results, file = "simulation_b_corr_results.csv", row.names = FALSE)
b_corr <- simulation_b_corr_results
# Compare to ground truth ------------------------------------------------------
for (i in 1:nrow(b_corr)) {
  if (b_corr$delta[i] < 0.01) {
    b_corr$probabilities_30[i] <- "P(Baseline = Algorithm) > 0.95"
  } else {
    b_corr$probabilities_30[i] <- "P(Baseline << Algorithm) > 0.95"
  }
  if (identical(as.character(b_corr$probabilities[i]), 
    b_corr$probabilities_30[i])) {
    b_corr$decision[i] <- 0 
  } else {
    b_corr$decision[i] <- 1
  }
}
# plot error rate per iteration ------------------------------------------------
par(mfrow=c(1,2))
for (i in 1:nrow(b_corr)) {
  b_corr$time[i] <- 30 - b_corr$repls[i]
}

errors <- list()
time_saved <- list()

for (i in b_corr$start_iter) {
  number_errors <- subset(b_corr, start_iter == i & delta == 0.20, select = c(decision))
  errors[i] <- colMeans(number_errors)
  subset_iter <- subset(b_corr, start_iter == i & delta == 0.20, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/30
}

start_iter <- 1:30
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[-c(1),] 
plot(plot_error, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "error rate", 
     main = "Benchmark Data")

# plot time saved per iteration ------------------------------------------------
plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[-c(1),] 
plot(plot_time, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "time saved",
     main = "Bayesian Correlated t-test")



#------------------------------------------------------------------------------#
# Bayesian Signed Ranks test ---------------------------------------------------
#------------------------------------------------------------------------------#


test_result <- data.frame()
for(start_iter in c(5, 10, 30)){
  for (i in 1:3) {
    mu_all <- c(0.4, 0.5, 0.6)
    mu <- mu_all[i]
    for (j in 1:6) {
      delta_mean_all <- c(0, 0.01, 0.05, 0.1, 0.2)
      delta_mean <- delta_mean_all[j]
      for (k in 1:5) {
        sigma_all <- c(0.01, 0.02, 0.05, 0.1, 0.2)
        sigma <- sigma_all[k]
        set.seed(123456)
        df <- generate_data(num = 30, mu, delta_mean, sigma)
        out_seq <- seq_b_signed_rank_test(df = df, baseline = "algo_a", 
                                          min_repls = start_iter, max_repls = 30)
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
}


b_signed <- simulation_b_signed_results
# Compare to ground truth ------------------------------------------------------
for (i in 1:nrow(b_signed)) {
  if (b_signed$delta[i] <= 0.01) {
    b_signed$probabilities_30[i] <- "P(Baseline = Algorithm) > 0.95"
  } else {
    b_signed$probabilities_30[i] <- "P(Baseline << Algorithm) > 0.95"
  }
  if (identical(as.character(b_signed$probabilities[i]), 
                b_signed$probabilities_30[i])) {
    b_signed$decision[i] <- 0 
  } else {
    b_signed$decision[i] <- 1
  }
  if (b_signed$probabilities[i] == "no decision") {
    b_signed$decision[i] <- 0 
  }
}

# plot error rate per iteration ------------------------------------------------
par(mfrow=c(1,2))
for (i in 1:nrow(b_signed)) {
  b_signed$time[i] <- 30 - b_signed$repls[i]
}

errors <- list()
time_saved <- list()

for (i in b_signed$start_iter) {
  number_errors <- subset(b_signed, start_iter == i & delta == 0.20, select = c(decision))
  errors[i] <- colMeans(number_errors)
  subset_iter <- subset(b_signed, start_iter == i & delta == 0.20, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/30
}

start_iter <- 1:30
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[c(5, 10, 30),] 
plot(plot_error, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "error rate", 
     main = "Benchmark Data")

# plot time saved per iteration ------------------------------------------------

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[c(5, 10, 30),] 
plot(plot_time, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "time saved",
     main = "Bayesian Correlated t-test")


#------------------------------------------------------------------------------#
#----------------------- truncated normal distribution ------------------------#
#------------------------------------------------------------------------------#
# install.packages("truncnorm")
library(truncnorm)



# Set up data ------------------------------------------------------------------
# generate data depending on num (Number of Iterations), mu (mean accuracy of
# first algorithm), delta_mean (mean difference between first and second 
# algorithm), sigma (Variance of each algorithms performance)
generate_trunc_data <- function(num, mu, delta_mean, sigma){
  data <- data.frame(algorithm = rep(c("algo_a", "algo_b"), length = num*2), 
                     measure_accuracy = rtruncnorm(n = num*2, mean = c(mu, mu + delta_mean), 
                                                   a = 0, b = 1, sd = sigma))
  data$problem <- "problem_1"
  for (i in unique(data$algorithm)) {
    data$replications[data$algorithm == i] <- seq_len(sum(data$algorithm == i))
  } 
  return(data)
}


#------------------------------------------------------------------------------#
# Bayesian correlated t-test ---------------------------------------------------
#------------------------------------------------------------------------------#

test_result <- data.frame()
for (start_iter in 2:30) {
  for (i in 1:3) {
    mu_all <- c(0.4, 0.5, 0.6)
    mu <- mu_all[i]
    for (j in 1:6) {
      delta_mean_all <- c(0, 0.01, 0.05, 0.1, 0.2)
      delta_mean <- delta_mean_all[j]
      for (k in 1:5) {
        sigma_all <- c(0.01, 0.02, 0.05, 0.1, 0.2)
        sigma <- sigma_all[k]
        set.seed(123456)
        df <- generate_trunc_data(num = 30, mu, delta_mean, sigma)
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
      }
    }
  }
}
# simulation_trunc_b_corr_results <-  test_result
# setwd("H:/MA/simulation_data")
# write.csv(simulation_trunc_b_corr_results, file = "simulation_trunc_b_corr_results.csv", row.names = FALSE)
b_corr_trunc <- simulation_trunc_b_corr_results

# Compare to ground truth ------------------------------------------------------
for (i in 1:nrow(b_corr_trunc)) {
  if (b_corr_trunc$delta[i] < 0.01) {
    b_corr_trunc$probabilities_30[i] <- "P(Baseline = Algorithm) > 0.95"
  } else {
    b_corr_trunc$probabilities_30[i] <- "P(Baseline << Algorithm) > 0.95"
  }
  if (identical(as.character(b_corr_trunc$probabilities[i]), 
                b_corr_trunc$probabilities_30[i])) {
    b_corr_trunc$decision[i] <- 0 
  } else {
    b_corr_trunc$decision[i] <- 1
  }
}


# plot error rate per iteration ------------------------------------------------
par(mfrow=c(1,2))

errors <- list()
for (i in b_corr_trunc$start_iter) {
  number_errors <- subset(b_corr_trunc, start_iter == i & delta == 0.00 & sigma == 0.2, select = c(decision))
  errors[i] <- colMeans(number_errors)
}

start_iter <- 1:30
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[-c(1),] 
plot(plot_error, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "error rate", 
     main = "Benchmark Data")

# plot time saved per iteration ------------------------------------------------
for (i in 1:nrow(b_corr_trunc)) {
  b_corr_trunc$time[i] <- 30 - b_corr_trunc$repls[i]
}

time_saved <- list()
for (i in b_corr_trunc$start_iter) {
  subset_iter <- subset(b_corr_trunc, start_iter == i & delta == 0.20, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/30
}

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[-c(1),] 
plot(plot_time, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "time saved",
     main = "Bayesian Correlated t-test")




#------------------------------------------------------------------------------#
# Bayesian correlated t-test 250 Replications ----------------------------------
#------------------------------------------------------------------------------#

test_result_250 <- data.frame()
for (i in 1:3) {
  mu_all <- c(0.4, 0.5, 0.6)
  mu <- mu_all[i]
  for (j in 1:5) {
    delta_mean_all <- c(0, 0.01, 0.05, 0.1, 0.2)
    delta_mean <- delta_mean_all[j]
    for (k in 1:5) {
      sigma_all <- c(0.01, 0.02, 0.05, 0.1, 0.2)
      sigma <- sigma_all[k]
      set.seed(123456)
      df <- generate_data(num = 250, mu, delta_mean, sigma)
      out_seq <- b_corr_t_test(df = df, baseline = "algo_a",
                               problem = "problem_1")
      row_id <- paste(mu, delta_mean, sigma, sep = "_")
      test_result_250[row_id, "algorithm"] <- out_seq$data_frame[1]
      test_result_250[row_id, "left"] <- out_seq$data_frame[2]
      test_result_250[row_id, "rope"] <- out_seq$data_frame[3]
      test_result_250[row_id, "right"] <- out_seq$data_frame[4]
      test_result_250[row_id, "probabilities"] <- out_seq$data_frame[5]
      test_result_250[row_id, "mu"] <- mu
      test_result_250[row_id, "delta"] <- delta_mean
      test_result_250[row_id, "sigma"] <- sigma
      test_result_250[row_id, "start_iter"] <- 250
      test_result_250[row_id, "repls"] <- 250
      }
   }
}

# merge data 
b_corr_250 <- subset(test_result_250, select = c(algorithm, mu, delta, sigma, probabilities))

# rename column to merge 
colnames(b_corr_250)[colnames(b_corr_250) == "probabilities"] <- 
  "probabilities_250"

# merge 
b_corr_comp <- merge(b_corr, b_corr_250)

# check for errors 

# compare (1 = wrong, 0 = right <- no differences found)
for (i in 1:nrow(b_corr_comp)) {
  if (identical(b_corr_comp$probabilities[i], b_corr_comp$probabilities_250[i])){
    b_corr_comp$decision[i] <- 0 
  } else {
    b_corr_comp$decision[i] <- 1
  }
}

# plot error rate per iteration ------------------------------------------------
# (average errors over problemsets )
par(mfrow=c(1,2))



errors <- list()
for (i in b_corr_comp$start_iter) {
  number_errors <- subset(b_corr_comp, start_iter == i, select = c(decision))
  errors[i] <- colMeans(number_errors)
}

start_iter <- 1:30
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[-c(1),] 
par(mgp = c(2, 1, 0))
plot(plot_error, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of replications", ylab = "error rate")



#------------------------------------------------------------------------------#
# Bayesian Signed Ranks test 250 Replications ----------------------------------
#------------------------------------------------------------------------------#
# 30 repls: 
b_signed <- simulation_b_signed_results

test_result_250 <- data.frame()
for (i in 1:3) {
  mu_all <- c(0.4, 0.5, 0.6)
  mu <- mu_all[i]
  for (j in 1:5) {
    delta_mean_all <- c(0, 0.01, 0.05, 0.1, 0.2)
    delta_mean <- delta_mean_all[j]
    for (k in 1:5) {
      sigma_all <- c(0.01, 0.02, 0.05, 0.1, 0.2)
      sigma <- sigma_all[k]
      set.seed(123456)
      df <- generate_data(num = 250, mu, delta_mean, sigma)
      out_seq <- b_signed_rank_test(df = df, baseline = "algo_a")
      row_id <- paste(mu, delta_mean, sigma, sep = "_")
      test_result_250[row_id, "algorithm"] <- out_seq$data_frame[1]
      test_result_250[row_id, "left"] <- out_seq$data_frame[2]
      test_result_250[row_id, "rope"] <- out_seq$data_frame[3]
      test_result_250[row_id, "right"] <- out_seq$data_frame[4]
      test_result_250[row_id, "probabilities"] <- out_seq$data_frame[5]
      test_result_250[row_id, "mu"] <- mu
      test_result_250[row_id, "delta"] <- delta_mean
      test_result_250[row_id, "sigma"] <- sigma
    }
  }
}


# merge data 
b_signed_250 <- subset(test_result_250, select = c(algorithm, mu, delta, sigma, probabilities))

# rename column to merge 
colnames(b_signed_250)[colnames(b_signed_250) == "probabilities"] <- 
  "probabilities_250"



# Bayesian signed rank test 
# merge 
b_signed_comp <- merge(b_signed, b_signed_250)

# check for errors 
for (i in 1:nrow(b_signed_comp)) {
  if (identical(as.character(b_signed_comp$probabilities[i]), b_signed_comp$probabilities_250[i])){
    b_signed_comp$decision[i] <- 0 
  } else {
    b_signed_comp$decision[i] <- 1
  }
}

# plot error rate per iteration ------------------------------------------------
# (average errors over problemsets )
par(mfrow=c(1,2))

errors <- list()
for (i in b_signed_comp$start_iter) {
  number_errors <- subset(b_signed_comp, start_iter == i, select = c(decision))
  errors[i] <- colMeans(number_errors)
}

start_iter <- 1:30
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[-c(1),] 
par(mgp = c(2, 1, 0))
plot(plot_error, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of replications", ylab = "error rate")


