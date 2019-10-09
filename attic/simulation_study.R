# Notes: start_iter die hier vorgegeben werden sind die mindestanzahl an Beobachtungen 
# die verwendet werden müssen oder die gemacht werden? Was genau will ich hierbei 
# überprüfen? Soll früher abgebrochen werden dürfen oder interessieren mich die 
# Ergebnisse wenn die volle Anzahl an Beobachtungen betrachtet wird. 
# Wie sieht die Zeitersparnis aus die sich dann dabei ergibt? 

# es wird immer der Datensatz mit 30 Beobachtungen verwendet und dann wird 
# geschaut wie es nach einer bestimmten Anzahl an Beobachtungen aussieht 
# first set seed. 
set.seed(123456)

# for(start_iter in c(5, 10, 30)){
#   for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
#     for (sigma in c(0.01, 0.02, 0.05, 0.1, 0.2)) {
#       df <- generate_data(start_iter, mu, delta_mean, sigma)
#       out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", problem = "problem_1", max_repls = start_iter)
#     }
#   }
# }




# nochmal überprüfen wie der Datensatz erstellt wird ---------------------------
start_iter = 5
delta_mean = 0
sigma =  0.01
mu = 0.7 
data <- data.frame(algorithm = rep(c("algo_a", "algo_b"), start_iter), 
                   replications = rep(1:start_iter, times = 1), 
                   measure_accuracy = rnorm(n = start_iter, mean = c(mu, mu + delta_mean), sd = sigma))

#-------------------------------------------------------------------------------



generate_data <- function( start_iter, mu, delta_mean, sigma){
  data <- data.frame(algorithm = rep(c("algo_a", "algo_b"), start_iter), 
                     replications = rep(1:start_iter, times = 1), 
                     measure_accuracy = rnorm(n = start_iter, mean = c(mu, mu + delta_mean), sd = sigma))
  data$problem <- "problem_1"
  return(data)
}
mu = 0.7

data_sigma = list()
data_delta = list()
data_repls = list()
for(start_iter in c(5, 10, 30)){
  for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
    for (sigma in c(0.001)) {
      df <- generate_data(start_iter, mu, delta_mean, sigma)
      out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", 
        problem = "problem_1", max_repls = 2, compare = "better")
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
data_repls
View(df)
#
## weitere Sigma Werte 0.01, 0.02, 0.05, 0.1, 0.2

# nochmal drüber nachdenken welche abgelehnt werden sollten, wenn keine Varianzt 
# vorliegt? alle die diese 0.01 entfernt sind oder mehr/weniger? 



# WICHTIG:----------------------------------------------------------------------
library(data.table)
# ab hier ist der richtige Code

generate_data <- function(num, mu, delta_mean, sigma){
  data <- data.frame(algorithm = rep(c("algo_a", "algo_b"), length = num*2), 
                     measure_accuracy = rnorm(n = num*2, mean = c(mu, mu + delta_mean), sd = sigma))
  data$problem <- "problem_1"
  data <- data.table::data.table(data)
  data[, replications := rowid(algorithm)]
  return(data)
}
mu = 0.7
data_sigma = list()
data_delta = list()
data_repls = list()
for(start_iter in 2:30){
  for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
    for (sigma in c(0)) {
      df <- generate_data(num = 30, mu, delta_mean, sigma)
      out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", 
        problem = "problem_1", min_num = start_iter, max_repls = 30)
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
data_repls
View(data_repls)


