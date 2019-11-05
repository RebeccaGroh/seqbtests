# Notes: start_iter die hier vorgegeben werden sind die mindestanzahl an Beobachtungen 
# die verwendet werden müssen oder die gemacht werden? Was genau will ich hierbei 
# überprüfen? Soll früher abgebrochen werden dürfen oder interessieren mich die 
# Ergebnisse wenn die volle Anzahl an Beobachtungen betrachtet wird. 
# Wie sieht die Zeitersparnis aus die sich dann dabei ergibt? 

# es wird immer der Datensatz mit 30 Beobachtungen verwendet und dann wird 
# geschaut wie es nach einer bestimmten Anzahl an Beobachtungen aussieht 
# first set seed. 
# set.seed(123456)

# for(start_iter in c(5, 10, 30)){
#   for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
#     for (sigma in c(0.01, 0.02, 0.05, 0.1, 0.2)) {
#       df <- generate_data(start_iter, mu, delta_mean, sigma)
#       out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", problem = "problem_1", max_repls = start_iter)
#     }
#   }
# }




# nochmal überprüfen wie der Datensatz erstellt wird ---------------------------
# start_iter = 5
# delta_mean = 0
# sigma =  0.01
# mu = 0.7
# data <- data.frame(algorithm = rep(c("algo_a", "algo_b"), start_iter),
#                    replications = rep(1:start_iter, times = 1),
#                    measure_accuracy = rnorm(n = start_iter, mean = c(mu, mu + delta_mean), sd = sigma))

#-------------------------------------------------------------------------------



# generate_data <- function(start_iter, mu, delta_mean, sigma){
#   data <- data.frame(algorithm = rep(c("algo_a", "algo_b"), start_iter), 
#                      replications = rep(1:start_iter, times = 1), 
#                      measure_accuracy = rnorm(n = start_iter, mean = c(mu, mu + delta_mean), sd = sigma))
#   data$problem <- "problem_1"
#   return(data)
# }
# mu = 0.7
# 
# data_sigma = list()
# data_delta = list()
# data_repls = list()
# for(start_iter in c(5, 10, 30)){
#   for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
#     for (sigma in c(0.001)) {
#       df <- generate_data(start_iter, mu, delta_mean, sigma)
#       out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", 
#         problem = "problem_1", max_repls = 2, compare = "better")
#       out_seq$data_frame$sigma <- sigma 
#       out_seq$data_frame$mu <- mu
#       out_seq$data_frame$delta <- delta_mean 
#       out_seq$data_frame$start_iter <- start_iter
#       data_sigma <- rbind(data_sigma, out_seq$data_frame)
#       row.names(data_sigma) <- NULL
#     }
#     data_delta <- rbind(data_delta, data_sigma)
#     data_sigma = list()
#   }
#   data_repls <- rbind(data_repls, data_delta)
#   data_delta = list()
# }
# data_repls
# View(df)
#
## weitere Sigma Werte 0.01, 0.02, 0.05, 0.1, 0.2

# nochmal drüber nachdenken welche abgelehnt werden sollten, wenn keine Varianzt 
# vorliegt? alle die diese 0.01 entfernt sind oder mehr/weniger? 




#------------------------------------------------------------------------------#
#---------------------------- normal distribution -----------------------------#
#------------------------------------------------------------------------------#

# normal distribution ----------------------------------------------------------

# ab hier ist der richtige Code

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


data = list()
data_sigma = list()
data_delta = list()
data_repls = list()
for(start_iter in 2:30){
  for (mu in c(0.4, 0.5, 0.6, 0.7)) {
    for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
      for (sigma in c(0.01, 0.02, 0.05, 0.1, 0.2)) {
        set.seed(123456)
        df <- generate_data(num = 30, mu, delta_mean, sigma)
        out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", 
          problem = "problem_1", min_repls = start_iter, max_repls = 30)
        out_seq$data_frame$sigma <- sigma 
        out_seq$data_frame$mu <- mu
        out_seq$data_frame$delta <- delta_mean 
        out_seq$data_frame$start_iter <- start_iter
        data_sigma <- rbind(data_sigma, out_seq$data_frame)
        row.names(data_sigma) <- NULL
      }
      data_delta <- rbind(data_delta, data_sigma)
      data_sigma = list()
    }
    data_repls <- rbind(data_repls, data_delta)
    data_delta = list()
  }
  data <- rbind(data, data_repls)
  data_repls = list()
}
# data
# View(data)
# save as csv 
# simulation_corr_test <- data 
# setwd("H:/MA/simulation_data")
# write.csv(simulation_corr_test, file = "simulation_corr_test.csv",row.names=FALSE)

# get number of wrong decisions 
for (i in 1:nrow(data)) {
  if (data$delta[i] < 0.01) {
    data$probabilities_30[i] <- "P(Baseline = Algorithm) > 0.95"
  } else {
    data$probabilities_30[i] <- "P(Baseline >> Algorithm) > 0.95"
  }
  if (identical(data$probabilities[i], data$probabilities_30[i])) {
    data$decision[i] <- 0 
  } else {
    data$decision[i] <- 1
  }
}

# plots ------------------------------------------------------------------------

# error rate 
errors <- list()
for (i in data$start_iter) {
  number_errors <- subset(data, start_iter == i & delta == 0.001, select = c(decision))
  errors[i] <- colMeans(number_errors)
}
errors
start_iter <- 1:30

test <- cbind(start_iter, errors)
test <- as.data.frame((test))
test <- test[-c(1),] 
plot(test, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "error rate")

# time rate 
for (i in 1:nrow(data)) {
  data$time[i] <- 30 - data$repls[i]
}

time_saved <- list()
for (i in data$start_iter) {
  subset_iter <- subset(data, start_iter == i & delta == 0.05, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/30
}
time_saved
start_iter <- 1:30

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[-c(1),] 
plot(plot_time, type="o", col="black", ylim = c(0,1), 
  xlab = "minimum number of iterations", ylab = "Time saved")

# Signed Rank Test -------------------------------------------------------------

data = list()
data_sigma = list()
data_delta = list()
data_repls = list()
for(start_iter in 2:30){
  for (mu in c(0.4, 0.5, 0.6, 0.7)) {
    for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
      for (sigma in c(0.01, 0.02, 0.05, 0.1, 0.2)) {
        set.seed(123456)
        df <- generate_data(num = 30, mu, delta_mean, sigma)
        out_seq <- seq_b_signed_rank_test(df = df, baseline = "algo_a", 
          min_repls = start_iter, max_repls = 30)
        out_seq$data_frame$sigma <- sigma 
        out_seq$data_frame$mu <- mu
        out_seq$data_frame$delta <- delta_mean 
        out_seq$data_frame$start_iter <- start_iter
        data_sigma <- rbind(data_sigma, out_seq$data_frame)
        row.names(data_sigma) <- NULL
      }
      data_delta <- rbind(data_delta, data_sigma)
      data_sigma = list()
    }
    data_repls <- rbind(data_repls, data_delta)
    data_delta = list()
  }
  data <- rbind(data, data_repls)
  data_repls = list()
}
# save as csv 
simulation_signed_ranks <- data
setwd("H:/MA/simulation_data")
write.csv(simulation_signed_ranks, file = "simulation_signed_ranks.csv",row.names=FALSE)

# Hierarchical Test ------------------------------------------------------------

data = list()
data_sigma = list()
data_delta = list()
data_repls = list()
for(start_iter in 2:30){
  for (mu in c(0.4, 0.5, 0.6, 0.7)) {
    for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
      for (sigma in c(0.01, 0.02, 0.05, 0.1, 0.2)) {
        df <- generate_data(num = 30, mu, delta_mean, sigma)
        out_seq <- seq_b_hierarchical_test(df = df, baseline = "algo_a", 
                                          min_repls = start_iter, max_repls = 30)
        out_seq$data_frame$sigma <- sigma 
        out_seq$data_frame$mu <- mu
        out_seq$data_frame$delta <- delta_mean 
        out_seq$data_frame$start_iter <- start_iter
        data_sigma <- rbind(data_sigma, out_seq$data_frame)
        row.names(data_sigma) <- NULL
      }
      data_delta <- rbind(data_delta, data_sigma)
      data_sigma = list()
    }
    data_repls <- rbind(data_repls, data_delta)
    data_delta = list()
  }
  data <- rbind(data, data_repls)
  data_repls = list()
}


# mehr Werte für Varianz testen (corr test) ------------------------------------



data = list()
data_sigma = list()
data_delta = list()
data_repls = list()
for(start_iter in 2:30){
  for (mu in c(0.4, 0.5, 0.6, 0.7)) {
    for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
      for (sigma in c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1)) {
        df <- generate_data(num = 30, mu, delta_mean, sigma)
        out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", 
                                     problem = "problem_1", min_repls = start_iter, max_repls = 30)
        out_seq$data_frame$sigma <- sigma 
        out_seq$data_frame$mu <- mu
        out_seq$data_frame$delta <- delta_mean 
        out_seq$data_frame$start_iter <- start_iter
        data_sigma <- rbind(data_sigma, out_seq$data_frame)
        row.names(data_sigma) <- NULL
      }
      data_delta <- rbind(data_delta, data_sigma)
      data_sigma = list()
    }
    data_repls <- rbind(data_repls, data_delta)
    data_delta = list()
  }
  data <- rbind(data, data_repls)
  data_repls = list()
}
# data
# View(data)


#------------------------------------------------------------------------------#
#----------------------- truncated normal distribution ------------------------#
#------------------------------------------------------------------------------#
# install.packages("truncnorm")
library(truncnorm)
set.seed(123456)

#
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

data = list()
data_sigma = list()
data_delta = list()
data_repls = list()
for(start_iter in 2:30){
  for (mu in c(0.4, 0.5, 0.6, 0.7)) {
    for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
      for (sigma in c(0.01, 0.02, 0.05, 0.1, 0.2)) {
        df <- generate_trunc_data(num = 30, mu, delta_mean, sigma)
        out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", 
          problem = "problem_1", min_repls = start_iter, max_repls = 30)
        out_seq$data_frame$sigma <- sigma 
        out_seq$data_frame$mu <- mu
        out_seq$data_frame$delta <- delta_mean 
        out_seq$data_frame$start_iter <- start_iter
        data_sigma <- rbind(data_sigma, out_seq$data_frame)
        row.names(data_sigma) <- NULL
      }
      data_delta <- rbind(data_delta, data_sigma)
      data_sigma = list()
    }
    data_repls <- rbind(data_repls, data_delta)
    data_delta = list()
  }
  data <- rbind(data, data_repls)
  data_repls = list()
}
# data
# View(data)


# get number of wrong decisions 
# richtige Entscheidung ist nicht in der letzten Beobachtung, sondern das muss 
# ich selbst am Anfang festlegen: 
for (i in 1:nrow(data)) {
  if (data$delta[i] < 0.01) {
    data$probabilities_30[i] <- "P(Baseline = Algorithm) > 0.95"
  } else {
    data$probabilities_30[i] <- "P(Baseline >> Algorithm) > 0.95"
  }
  if (identical(data$probabilities[i], data$probabilities_30[i])) {
    data$decision[i] <- 0 
  } else {
    data$decision[i] <- 1
  }
}

# plots ------------------------------------------------------------------------

# error rate 
errors <- list()
for (i in data$start_iter) {
  number_errors <- subset(data, start_iter == i & delta == 0.001, select = c(decision))
  errors[i] <- colMeans(number_errors)
}
errors
start_iter <- 1:30

test <- cbind(start_iter, errors)
test <- as.data.frame((test))
test <- test[-c(1),] 
plot(test, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "error rate")

# time rate 
for (i in 1:nrow(data)) {
  data$time[i] <- 30 - data$repls[i]
}

time_saved <- list()
for (i in data$start_iter) {
  subset_iter <- subset(data, start_iter == i & delta == 0.05, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/30
}
time_saved
start_iter <- 1:30

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[-c(1),] 
plot(plot_time, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "Time saved")

