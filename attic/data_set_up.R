library(stringr)
result$lrn.cl <- str_sub(result$lrn.cl, start=9L, end=30L)
unique(result$lrn.cl)

result$algorithm <- paste(result$lrn.cl, result$feat.extract.method, sep = "_")
result$algorithm <- paste(result$algorithm, result$tune, sep = "_")

# drop time_queued, time_running, n, ntrain, ntest, length, nclasses, minorityclass_size,
# ber, timeboth 
result_small <- subset(result, tune == "tune", 
                       select= -c(time.queued, time.running, n, ntrain, ntest, 
                                  length, nclasses, feat.extract.method, 
                                  minorityclass_size, algo.type,
                                  ber, timeboth, job.id, tune, lrn.cl, algo.pars))

## check for duplicates (drops rows that are duplicated)
result_small <- unique(result_small)

# rename values and measure column value von ranger.pow vorher umbenennen 
colnames(result_small)[colnames(result_small) == "mmce"] <- "measure_mmce"
colnames(result_small)[colnames(result_small) == "repl"] <- "replications"

# View(result)
# View(result_small)
unique(result_small$algorithm)
# knn_dtw_tuned | ranger_none_tuned | ranger_wavelet_tuned
# lrn.cl_feat.extract.method_tune? 

# Vorgehen:
# Wie viele Replikationen sind gegeben? Die höchste Anzahl wird im Test verwendet 
# ist ground truth --> daran wird orientiert ob falsche Entscheidungen getroffen 
# wurden 
# Ein Algorithmus wird als Baseline ausgesucht. Alle anderen Algorithmen werden 
# in allen Tests dagegen getestet. Für T1 muss ein Baseline Datensatz ausgewählt 
# werden 

# Baseline = ranger.pow_wavelet_tune
# Algorithmen = alle 
# Test = Sequential Correlated t test 

# drop NAs 
result_small <- na_drop(df = result_small, check_var = "algorithm")

problems <- unique(result_small[["problem"]])
problems
data <- list()

for (i in problems) {
  for (start_iter in 2:10) {
    out_seq <- seq_b_corr_t_test(df = result_small, baseline = "ranger.pow_wavelet_tune", 
                                 problem = i, max_repls = 10, min_repls = start_iter, prob = 0.95)
    out_seq$data_frame$start_iter <- start_iter
    out_seq$data_frame$problem <- i
    data <- rbind(data, out_seq$data_frame)
  }
}
# data
## Ergebnis nach 10 Replikationen ist das richtige! 
## Also muss das Ergebnis mit den vorherigen Ergebnissen verglichen werdne (für 
## alle Zeilen die sich nur anhand von der Anzahl an Replikationen unterscheiden)

## letzte Beobachtung extrahieren 
data_10 <- subset(data, start_iter == 10, 
                  select = c(problem, algorithm, probabilities))
# rename column to merge 
colnames(data_10)[colnames(data_10) == "probabilities"] <- "probabilities_10"

# merge 
data_new <- merge(data, data_10)

## compare results 
for (i in 1:nrow(data_new)) {
  if (identical(data_new$probabilities[i], data_new$probabilities_10[i])) {
    data_new$decision[i] <- 0 
  } else {
    data_new$decision[i] <- 1
  }
}

# plot error rate per iteration ------------------------------------------------

errors <- list()
for (i in data_new$start_iter) {
  number_errors <- subset(data_new, start_iter == i, select = c(decision))
  errors[i] <- colMeans(number_errors)
}
errors
start_iter <- 1:10

plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[-c(1),] 
plot(plot_error, type="o", col="black", ylim = c(0,1), 
  xlab = "minimum number of iterations", ylab = "error rate")

# plot saved time per iteration ------------------------------------------------


# wie wird die gesparte Zeit definiert? --> Die volle Zeit wäre 10 Repliaktionen 
# also ist die gesparte Zeit in jeder Reihe 10-repls 
for (i in 1:nrow(data_new)) {
  data_new$time[i] <- 10 - data_new$repls[i]
}
time_saved <- list()
for (i in data_new$start_iter) {
  subset_iter <- subset(data_new, start_iter == i, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/10
}
time_saved
start_iter <- 1:10

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[-c(1),] 
plot(plot_time, type="o", col="black", ylim = c(0,1), 
  xlab = "minimum number of iterations", ylab = "Time saved")

# Bayesian Signed Rank Test ----------------------------------------------------

data <- list()

for (start_iter in 2:10) {
  out_seq <- seq_b_signed_rank_test(df = result_small, baseline = "ranger.pow_wavelet_tune", 
                                    max_repls = 10, min_repls = start_iter, prob = 0.95)
  out_seq$data_frame$start_iter <- start_iter
  data <- rbind(data, out_seq$data_frame)
}



## letzte Beobachtung extrahieren 
data_10 <- subset(data, start_iter == 10, 
                  select = c(algorithm, probabilities))
# rename column to merge 
colnames(data_10)[colnames(data_10) == "probabilities"] <- "probabilities_10"

# merge 
data_new <- merge(data, data_10)

## compare results 
for (i in 1:nrow(data_new)) {
  if (identical(data_new$probabilities[i], data_new$probabilities_10[i])) {
    data_new$decision[i] <- 0 
  } else {
    data_new$decision[i] <- 1
  }
}
# error rate 
errors <- list()
for (i in data_new$start_iter) {
  number_errors <- subset(data_new, start_iter == i, select = c(decision))
  errors[i] <- colMeans(number_errors)
}
errors
start_iter <- 1:10

test <- cbind(start_iter, errors)
test <- as.data.frame((test))
test <- test[-c(1),] 
plot(test, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "error rate")

# es treten überhaupt keine Fehler auf weil nie eine Entscheidung getroffen wird
