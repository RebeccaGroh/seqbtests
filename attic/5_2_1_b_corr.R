
# Chapter 5 - Experiments ------------------------------------------------------

# Subsection 5.2 - Application on simulated data -------------------------------

# Subsubsection 5.2.1 - Bayesian correlated t test -----------------------------

# Ground Truth 1: decision is made based on the values of delta and Rope 
# Ground Truth 2: decision is made based on posterior after 250 Replications

# Pakcages 
library(data.table)
library(stringr)
library(gridExtra)
library(ggplot2)

# Set-up ----------------------------------------------------------------------- 

# Tests: Bayesian Correlated t-test, Bayesian Signed Ranks Test
# Ground Truth: When the mean difference between the first and second algorithm 
# (delta) differs by more than the rope, 250 replications

# generate data depending on num (Number of Iterations), mu (mean accuracy of
# first algorithm), delta_mean (mean difference between first and second 
# algorithm), sigma (Variance of each algorithms performance)
generate_data <- function(num, mu, delta_mean, sigma){
  data <- data.frame(algorithm = rep(c("algo_a", "algo_b"), length = num*2), 
    measure_accuracy = rnorm(n = num*2, mean = c(mu, mu + delta_mean), 
    sd = sigma))
  data$problem <- "problem_1"
  for (i in unique(data$algorithm)) {
    data$replication[data$algorithm == i] <- seq_len(sum(data$algorithm == i))
  } 
  return(data)
}

# test on simulated data -------------------------------------------------------
test_result_b1 <- data.frame()
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
        test_result_b1[row_id, "algorithm"] <- out_seq$data_frame[1]
        test_result_b1[row_id, "left"] <- out_seq$data_frame[2]
        test_result_b1[row_id, "rope"] <- out_seq$data_frame[3]
        test_result_b1[row_id, "right"] <- out_seq$data_frame[4]
        test_result_b1[row_id, "repls"] <- out_seq$data_frame[5]
        test_result_b1[row_id, "probabilities"] <- out_seq$data_frame[6]
        test_result_b1[row_id, "mu"] <- mu
        test_result_b1[row_id, "delta"] <- delta_mean
        test_result_b1[row_id, "sigma"] <- sigma
        test_result_b1[row_id, "start_iter"] <- start_iter
      }
    }
  }
}

# SAVE DATA 
simulation_b_corr_results <- test_result_b1
# setwd("H:/MA/simulation_data")
# write.csv(simulation_b_corr_results, file = "simulation_b_corr_results.csv", row.names = FALSE)
b_corr <- simulation_b_corr_results

# Compare to ground truth 1 ----------------------------------------------------
for (i in 1:nrow(b_corr)) {
  if (b_corr$delta[i] <= 0.01) {
    b_corr$probabilities_30[i] <- "P(Baseline = Algorithm) > 0.95"
  } else {
    b_corr$probabilities_30[i] <- "P(Baseline << Algorithm) > 0.95"
  }
  if (b_corr$delta[i] <= b_corr$sigma[i]) {
    b_corr$probabilities_30[i] <- "no decision"
  }
  if (identical(as.character(b_corr$probabilities[i]), 
                b_corr$probabilities_30[i])) {
    b_corr$decision[i] <- 0 
  } else {
    b_corr$decision[i] <- 1
  }
}

# PLOT -------------------------------------------------------------------------

for (i in 1:nrow(b_corr)) {
  b_corr$time[i] <- 30 - b_corr$repls[i]
}

errors <- list()
time_saved <- list()

for (i in b_corr$start_iter) {
  number_errors <- subset(b_corr, start_iter == i, select = c(decision))
  errors[i] <- colMeans(number_errors)
  subset_iter <- subset(b_corr, start_iter == i, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/30
}

start_iter <- 1:30
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[-c(1),] 

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[-c(1),] 

error_plot <- ggplot(data=plot_error, aes(x=as.numeric(start_iter), 
                                          y=as.numeric(errors), group=1)) +
  geom_line()+
  geom_point() + 
  xlab("minimum number of replications") + ylab("error rate") + 
  ylim(0, 1)

time_plot <- ggplot(data=plot_time, aes(x=as.numeric(start_iter), 
                                        y=as.numeric(time_saved), group=1)) +
  geom_line()+
  geom_point() + 
  xlab("minimum number of replications") + ylab("time saving (%)") + 
  ylim(0, 1)


simulation_b1_gr1 <- grid.arrange(error_plot, time_plot, nrow = 1)

# save plot 
ggsave("simulation_b1_gr1.pdf", plot = simulation_b1_gr1, device = "pdf", 
       path = "C:/Users/Jo/Desktop/simulation_data/plots_thesis", width = 6, height = 3)

# Compare to ground truth 2 ----------------------------------------------------

test_result_b1_250 <- data.frame()
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
      test_result_b1_250[row_id, "algorithm"] <- out_seq$data_frame[1]
      test_result_b1_250[row_id, "left"] <- out_seq$data_frame[2]
      test_result_b1_250[row_id, "rope"] <- out_seq$data_frame[3]
      test_result_b1_250[row_id, "right"] <- out_seq$data_frame[4]
      test_result_b1_250[row_id, "probabilities"] <- out_seq$data_frame[5]
      test_result_b1_250[row_id, "mu"] <- mu
      test_result_b1_250[row_id, "delta"] <- delta_mean
      test_result_b1_250[row_id, "sigma"] <- sigma
      test_result_b1_250[row_id, "start_iter"] <- 250
      test_result_b1_250[row_id, "repls"] <- 250
    }
  }
}

# merge data 
b_corr_250 <- subset(test_result_b1_250, select = c(algorithm, mu, delta, sigma, probabilities))

# rename column to merge 
colnames(b_corr_250)[colnames(b_corr_250) == "probabilities"] <- 
  "probabilities_250"

# merge 
b_corr_comp <- merge(b_corr, b_corr_250)

# compare (1 = wrong, 0 = right <- no differences found)
for (i in 1:nrow(b_corr_comp)) {
  if (identical(as.character(b_corr_comp$probabilities[i]), b_corr_comp$probabilities_250[i])){
    b_corr_comp$decision[i] <- 0 
  } else {
    b_corr_comp$decision[i] <- 1
  }
}

# PLOTS ------------------------------------------------------------------------

for (i in 1:nrow(b_corr_comp)) {
  b_corr_comp$time[i] <- 30 - b_corr_comp$repls[i]
}

errors <- list()
time_saved <- list()

for (i in b_corr_comp$start_iter) {
  number_errors <- subset(b_corr_comp, start_iter == i, select = c(decision))
  errors[i] <- colMeans(number_errors)
  subset_iter <- subset(b_corr_comp, start_iter == i, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/30
}

start_iter <- 1:30
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[-c(1),] 

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[-c(1),] 


error_plot <- ggplot(data=plot_error, aes(x=as.numeric(start_iter), 
                                          y=as.numeric(errors), group=1)) +
  geom_line()+
  geom_point() + 
  xlab("minimum number of replications") + ylab("error rate") + 
  ylim(0, 1)

time_plot <- ggplot(data=plot_time, aes(x=as.numeric(start_iter), 
                                        y=as.numeric(time_saved), group=1)) +
  geom_line()+
  geom_point() + 
  xlab("minimum number of replications") + ylab("time saving (%)") + 
  ylim(0, 1)


simulation_b1_gr2 <- grid.arrange(error_plot, time_plot, nrow = 1)

ggsave("simulation_b1_gr2.pdf", plot = simulation_b1_gr2, device = "pdf", 
       path = "H:/MA/simulation_data/plots_thesis", width = 6, height = 3)
