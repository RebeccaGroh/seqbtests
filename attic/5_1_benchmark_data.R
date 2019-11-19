
# Chapter 5 - Experiments ------------------------------------------------------

# Subsection 5.1 - Application on real data ------------------------------------

# Packages 
library(stringr)
library(gridExtra)
# Set-up ----------------------------------------------------------------------- 

# Used Data: Benchmark Data 
# Tests: Bayesian Correlated t-test, Bayesian Signed Ranks Test
# Baseline = ranger.pow_wavelet_tune
# Ground Truth: Decision after the maximum number of replications

benchmark_data <- result  

benchmark_data$lrn.cl <- str_sub(benchmark_data$lrn.cl, start=9L, end=30L)
unique(benchmark_data$lrn.cl)
# paste algorithm, extract method and tune 
benchmark_data$algorithm <- 
  paste(benchmark_data$lrn.cl, benchmark_data$feat.extract.method, sep = "_")
benchmark_data$algorithm <- 
  paste(benchmark_data$algorithm, benchmark_data$tune, sep = "_")

# drop unnecessary variables (keep only tuned algorithms)
# benchmark_small <- subset(benchmark_data, tune == "tune", 
#   select= -c(time.queued, time.running, n, ntrain, ntest, length, nclasses, 
#     feat.extract.method, minorityclass_size, algo.type, ber, timeboth, job.id, 
#     tune, lrn.cl, algo.pars))

# drop unnecessary variables (keep all algorithms)
benchmark_small <- subset(benchmark_data,
  select= -c(time.queued, time.running, n, ntrain, ntest, length, nclasses,
    feat.extract.method, minorityclass_size, algo.type, ber, timeboth, job.id,
    tune, lrn.cl, algo.pars))


## check for duplicates (drops rows that are duplicated)
benchmark_small <- unique(benchmark_small)

# rename values and measure column value von ranger.pow vorher umbenennen 
colnames(benchmark_small)[colnames(benchmark_small) == "mmce"] <- "measure_mmce"
colnames(benchmark_small)[colnames(benchmark_small) == "repl"] <- "replication"

# drop NAs 
benchmark_small <- na_drop(df = benchmark_small, check_var = "algorithm") 
# 27030 obs. 

# 5.1.1 Bayesian correlated t-test ---------------------------------------------

b_corr_results <- list()
for (i in unique(benchmark_small$problem)) {
  for (start_iter in 2:10) {
    b_corr_out <- seq_b_corr_t_test(df = benchmark_small, 
      baseline = "ranger.pow_wavelet_tune", problem = i, min_repls = start_iter, 
      prob = 0.99, max_repls = 10)
    b_corr_out$data_frame$start_iter <- start_iter
    b_corr_out$data_frame$problem <- i
    b_corr_results <- rbind(b_corr_results, b_corr_out$data_frame)
  }
}
# 23.868 obs.

# SAVE DATA 
# benchmark_b_corr_results_all_data <- b_corr_results
# setwd("H:/MA/simulation_data")
# write.csv(benchmark_b_corr_results_all_data, file = "benchmark_b_corr_results_all_data.csv", row.names = FALSE)
# b_corr_results <- benchmark_b_corr_results_all_data 

# Compare to ground truth ------------------------------------------------------
ground_truth <- subset(b_corr_results, start_iter == 10, 
  select = c(problem, algorithm, probabilities))

# rename column to merge 
colnames(ground_truth)[colnames(ground_truth) == "probabilities"] <- 
  "probabilities_10"

# merge 
b_corr_comp <- merge(b_corr_results, ground_truth)

# compare (1 = wrong, 0 = right <- no differences found)
for (i in 1:nrow(b_corr_comp)) {
  if (identical(b_corr_comp$probabilities[i], b_corr_comp$probabilities_10[i])){
    b_corr_comp$decision[i] <- 0 
  } else {
    b_corr_comp$decision[i] <- 1
  }
}

# PLOTS ------------------------------------------------------------------------
# (aggregate over problemsets and algorithms)

for (i in 1:nrow(b_corr_comp)) {
  b_corr_comp$time[i] <- 10 - b_corr_comp$repls[i]
}

errors <- list()
time_saved <- list()
for (i in b_corr_comp$start_iter) {
  number_errors <- subset(b_corr_comp, start_iter == i, select = c(decision))
  errors[i] <- colMeans(number_errors)
  subset_iter <- subset(b_corr_comp, start_iter == i, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/10
}

start_iter <- 1:10
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
  xlab("minimum number of replication") + ylab("error rate") + 
  ylim(0, 1)

time_plot <- ggplot(data=plot_time, aes(x=as.numeric(start_iter), 
                      y=as.numeric(time_saved), group=1)) +
  geom_line()+
  geom_point() + 
  xlab("minimum number of replication") + ylab("time saving (%)") + 
  ylim(0, 1)


benchmark_b1 <- grid.arrange(error_plot, time_plot, nrow = 1)

ggsave("benchmark_b1.pdf", plot = benchmark_b1, device = "pdf", 
  path = "H:/MA/simulation_data/plots_thesis", width = 6, height = 3)


# 5.1.2 Bayesian Signed Ranks test ---------------------------------------------

b_signed_results <- list()
for (start_iter in 2:10) {
  b_signed_out <- seq_b_signed_rank_test(df = benchmark_small, 
    baseline = "ranger.pow_wavelet_tune", min_repls = start_iter, 
    prob = 0.95, max_repls = 10)
  b_signed_out$data_frame$start_iter <- start_iter
  b_signed_results <- rbind(b_signed_results, b_signed_out$data_frame)
}

# SAVE DATA 
# benchmark_b_signed_results_all_data <- b_signed_results
# setwd("H:/MA/simulation_data")
# write.csv(benchmark_b_signed_results_all_data, file = "benchmark_b_signed_results_all_data.csv", row.names = FALSE)
# b_signed_results <- benchmark_b_signed_results_all_data


# Compare to ground truth ------------------------------------------------------
ground_truth <- subset(b_signed_results, start_iter == 10, 
  select = c(algorithm, probabilities))

# rename column to merge 
colnames(ground_truth)[colnames(ground_truth) == "probabilities"] <- 
  "probabilities_10"

# merge 
b_signed_comp <- merge(b_signed_results, ground_truth)

# compare (1 = wrong, 0 = right <- no differences found)
for (i in 1:nrow(b_signed_comp)) {
  if (identical(b_signed_comp$probabilities[i], 
        b_signed_comp$probabilities_10[i])){
    b_signed_comp$decision[i] <- 0 
  } else {
    b_signed_comp$decision[i] <- 1
  }
}

# PLOTS ------------------------------------------------------------------------
# (aggregate over problemsets and algorithms)

for (i in 1:nrow(b_signed_comp)) {
  b_signed_comp$time[i] <- 10 - b_signed_comp$repls[i]
}

time_saved <- list()
errors <- list()
for (i in b_signed_comp$start_iter) {
  number_errors <- subset(b_signed_comp, start_iter == i, select = c(decision))
  errors[i] <- colMeans(number_errors)
  subset_iter <- subset(b_signed_comp, start_iter == i, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/10
}

start_iter <- 1:10
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[-c(1),] 

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[-c(1),] 
par(mgp = c(2, 1, 0))

error_plot <- ggplot(data=plot_error, aes(x=as.numeric(start_iter), 
                                          y=as.numeric(errors), group=1)) +
  geom_line()+
  geom_point() + 
  xlab("minimum number of replication") + ylab("error rate") + 
  ylim(0, 1)

time_plot <- ggplot(data=plot_time, aes(x=as.numeric(start_iter), 
                                        y=as.numeric(time_saved), group=1)) +
  geom_line()+
  geom_point() + 
  xlab("minimum number of replication") + ylab("time saving (%)") + 
  ylim(0, 1)


benchmark_b2 <- grid.arrange(error_plot, time_plot, nrow = 1)

ggsave("benchmark_b2.pdf", plot = benchmark_b2, device = "pdf", 
       path = "H:/MA/simulation_data/plots_thesis", width = 6, height = 3)
