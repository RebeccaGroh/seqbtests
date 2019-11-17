

# Chapter 5 - Experiments ------------------------------------------------------

# Subsection 5.2 - Application on simulated data -------------------------------

# Subsubsection 5.2.2 - Bayesian signed rank test ------------------------------

# Ground Truth 1: decision is made based on the values of delta and Rope 
# Ground Truth 2: decision is made based on posterior after 250 Replications

# Packages 
library(data.table)
library(stringr)
library(gridExtra)

# create problem sets depending on mu --> same trends on each 
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

# test on simulated data -------------------------------------------------------
test_result_b2 <- data.frame()
for (start_iter in c(2, 5, 10, 15, 20, 25, 30)) {
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
      row_id <- paste(start_iter, delta_mean, sigma, sep = "_")
      test_result_b2[row_id, "algorithm"] <- out_seq$data_frame[1]
      test_result_b2[row_id, "left"] <- out_seq$data_frame[2]
      test_result_b2[row_id, "rope"] <- out_seq$data_frame[3]
      test_result_b2[row_id, "right"] <- out_seq$data_frame[4]
      test_result_b2[row_id, "repls"] <- out_seq$data_frame[5]
      test_result_b2[row_id, "probabilities"] <- out_seq$data_frame[6]
      test_result_b2[row_id, "delta"] <- delta_mean
      test_result_b2[row_id, "sigma"] <- sigma
      test_result_b2[row_id, "start_iter"] <- start_iter
    }
  }
}


# SAVE DATA 
# simulation_b_signed_multiple_data_mu <- test_result_b2
# setwd("H:/MA/simulation_data")
# write.csv(simulation_b_signed_multiple_data_mu, 
# file = "simulation_b_signed_multiple_data_mu.csv", row.names = FALSE)
b_signed <- simulation_b_signed_multiple_data_mu

# Compare to ground truth 1 ----------------------------------------------------

for (i in 1:nrow(b_signed)) {
  if (b_signed$delta[i] <= 0.01) {
    b_signed$probabilities_30[i] <- "P(Baseline = Algorithm) > 0.95"
  } else {
    b_signed$probabilities_30[i] <- "P(Baseline << Algorithm) > 0.95"
  }
  if (b_signed$delta[i] <= b_signed$sigma[i]) {
    b_signed$probabilities_30[i] <- "no decision"
  }
  if (identical(as.character(b_signed$probabilities[i]), 
                b_signed$probabilities_30[i])) {
    b_signed$decision[i] <- 0 
  } else {
    b_signed$decision[i] <- 1
  }
}

# PLOT -------------------------------------------------------------------------

for (i in 1:nrow(b_signed)) {
  b_signed$time[i] <- 30 - b_signed$repls[i]
}

errors <- list()
time_saved <- list()

for (i in b_signed$start_iter) {
  number_errors <- subset(b_signed, start_iter == i, select = c(decision))
  errors[i] <- colMeans(number_errors)
  subset_iter <- subset(b_signed, start_iter == i, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/30
}

start_iter <- 1:30
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[c(2, 5, 10, 15, 20, 25, 30),] 

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[c(2, 5, 10, 15, 20, 25, 30),,] 

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


simulation_b2_gr1 <- grid.arrange(error_plot, time_plot, nrow = 1)

# save plot 
ggsave("simulation_b2_gr1.pdf", plot = simulation_b2_gr1, device = "pdf", 
  path = "C:/Users/Jo/Desktop/simulation_data/plots_thesis", width = 6, height = 3)



# Compare to ground truth 2 ----------------------------------------------------

test_result_b2_250 <- data.frame()
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
    out_seq <- b_signed_rank_test(df, baseline = "algo_a")
    row_id <- paste(delta_mean, sigma, sep = "_")
    test_result_b2_250[row_id, "algorithm"] <- out_seq$data_frame[1]
    test_result_b2_250[row_id, "left"] <- out_seq$data_frame[2]
    test_result_b2_250[row_id, "rope"] <- out_seq$data_frame[3]
    test_result_b2_250[row_id, "right"] <- out_seq$data_frame[4]
    test_result_b2_250[row_id, "probabilities"] <- out_seq$data_frame[5]
    test_result_b2_250[row_id, "delta"] <- delta_mean
    test_result_b2_250[row_id, "sigma"] <- sigma
  }
}


# merge data 
b_signed_250 <- 
  subset(test_result_b2_250, select = c(algorithm, delta, sigma, probabilities))

# rename column to merge 
colnames(b_signed_250)[colnames(b_signed_250) == "probabilities"] <- 
  "probabilities_250"

b_signed_comp <- merge(b_signed, b_signed_250)

# check for errors 
for (i in 1:nrow(b_signed_comp)) {
  if (identical(as.character(b_signed_comp$probabilities[i]), 
    b_signed_comp$probabilities_250[i])){
    b_signed_comp$decision[i] <- 0 
  } else {
    b_signed_comp$decision[i] <- 1
  }
}

# PLOT -------------------------------------------------------------------------

for (i in 1:nrow(b_signed_comp)) {
  b_signed_comp$time[i] <- 30 - b_signed_comp$repls[i]
}

errors <- list()
time_saved <- list()

for (i in b_signed_comp$start_iter) {
  number_errors <- subset(b_signed_comp, start_iter == i, select = c(decision))
  errors[i] <- colMeans(number_errors)
  subset_iter <- subset(b_signed_comp, start_iter == i, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/30
}

start_iter <- 1:30
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[c(2, 5, 10, 15, 20, 25, 30),] 

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[c(2, 5, 10, 15, 20, 25, 30),,] 



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


simulation_b2_gr2 <- grid.arrange(error_plot, time_plot, nrow = 1)

#save plot
ggsave("simulation_b2_gr2.pdf", plot = simulation_b2_gr2, device = "pdf", 
       path = "C:/Users/Jo/Desktop/simulation_data/plots_thesis", width = 6, height = 3)


