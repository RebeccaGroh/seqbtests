test <- subset(b_corr_comp, decision == 1)

table(test$start_iter)
table(test$repls)


table(b_corr_comp$time)

# wenn 0.9 verwendet wird, steigt die Anzahl an Fehler etwas an, vor allem 
# nach den ersten Beobachtungen, weil es hier schneller zu einer Entscheidung 
# kommt als mit dem höheren Wert 

# wenn 0.99 verwendet wird, sinkt die Fehleranzahl. Die Anzahl an Fehlern die 
# in der ersten runde gleich gemacht werden sinkt, und dann treten eher fehler
# auf weil der Wert drum herum schwankt und dabei kann es auch sein, dass wenn 
# mehr als 10 beobvachtungen der wert wieder über den Threshold steigen würde

















# Notes ------------------------------------------------------------------------
b_hierarchical_out <- b_hierarchical_test(df = benchmark_small, 
  baseline = "ranger.pow_wavelet_tune")
b_hierarchical_out




b_signed_out <- b_signed_rank_test(df = benchmark_small, 
  baseline = "ranger.pow_wavelet_tune", algorithm = "xgboost_none_default")
b_signed_out

for (start_iter in 2:10) {
  b_hierarchical_out <- seq_b_hierarchical_test(df = benchmark_small, 
    baseline = "ranger.pow_wavelet_tune", min_repls = start_iter, prob = 0.95, 
    max_repls = 10)
  b_hierarchical_out$data_frame$start_iter <- start_iter
  b_hierarchical_results <- rbind(b_hierarchical_results, 
                                  b_hierarchical_out$data_frame)
}


## Datensatz umwandelen, dass scmamp getestet werden kann 

test <- tidyr::spread(benchmark_small, algorithm, measure_mmce)


sample.a <- matrix(test$ranger.pow_wavelet_tune, byrow=TRUE, nrow=10)
sample.b <- matrix(test$xgboost_bsignal_tune  , byrow=TRUE, nrow=10)


results <- scmamp::bHierarchicalTest(sample.a, sample.b, rho=0.1, rope=c(-0.01, 0.01), nsim=2000, nchains=5)
results$posterior.probabilities
# Ergebnisse stimmen mit denen von scmamp überein. Deswegen nochmal testen
# wie die Ergebnisse aussehen wenn frühzeitig abgebrochen wird. 



# 5 als Mindestanzahl vorgeben, vielleicht kommt man dann zu besseren 
# Ergebnissen bei denen nicht ständig Warnungen ausgegeben werden 
b_hierarchical_results <- list()
for (start_iter in 5:10) {
  b_hierarchical_out <- seq_b_hierarchical_test(df = benchmark_small, 
    baseline = "ranger.pow_wavelet_tune", min_repls = start_iter, prob = 0.95, 
    max_repls = 10)
  b_hierarchical_out$data_frame$start_iter <- start_iter
  b_hierarchical_results <- rbind(b_hierarchical_results, 
                                  b_hierarchical_out$data_frame)
}







  
    
     
        

test <- seq_b_hierarchical_test(df = benchmark_small, algorithm = "ranger.pow_dtwkernel_default",
                                baseline = "ranger.pow_wavelet_tune", 
                                min_repls = 8, prob = 0.95, max_repls = 10)
test

# für welche treten Probleme auf? 
# xgboost_multires_default
# ksvm_wavelet_default
# glmnet_wavelet_default
# xgboost_none_tune
# xgboost_multires_tune
# xgboost_wavelet_tune
# ranger.pow_wavelet_default

# da es sich bei dem hierarchischen um einen komplexeren test handelt, müssen 
# manchmal bestimmte Einstellungen vorgenommen werden um reliable posterior 
# estimates zu erzielen. Da dies den Rahmen der Arbeit sprengen würde, werden 
# hier nur die ALgorithmen betrachtet, bei denen mit Standardeinstellung 
# valide Ergebnisse erzielt werden können. 




unique(benchmark_small_bhier$algorithm)

benchmark_small_bhier <- subset(benchmark_small, algorithm!="xgboost_multires_default" & 
                                  algorithm!="ksvm_wavelet_default" &
                                  algorithm!="glmnet_wavelet_default" &
                                  algorithm!="xgboost_none_tune" &
                                  algorithm!="xgboost_multires_tune" &
                                  algorithm!="xgboost_wavelet_tune" &
                                  algorithm!="ranger.pow_wavelet_default")


unique(benchmark_small_bhier$algorithm)

b_hierarchical_out <- seq_b_hierarchical_test(df = benchmark_small_bhier, 
  baseline = "ranger.pow_wavelet_tune", 
  min_repls = 8, prob = 0.95, max_repls = 10)
b_hierarchical_out



#------------------------------------------------------------------------------#
# Bayesian Hierarchical correlated t-test --------------------------------------
#------------------------------------------------------------------------------#
# Überlegen ob vielleicht nur jede zweite Iteration durchgeführt werden sollte, 
# um Zeit zu sparen? 2, 4, 6, 8, 9, 10
benchmark_small_bhier <- subset(benchmark_small, algorithm!="xgboost_multires_default" & 
                                  algorithm!="ksvm_wavelet_default" &
                                  algorithm!="glmnet_wavelet_default" &
                                  algorithm!="xgboost_none_tune" &
                                  algorithm!="xgboost_multires_tune" &
                                  algorithm!="xgboost_wavelet_tune" &
                                  algorithm!="ranger.pow_wavelet_default")


b_hierarchical_results <- list()
for (start_iter in 2:10) {
  b_hierarchical_out <- seq_b_hierarchical_test(df = benchmark_small_bhier, 
                                                baseline = "ranger.pow_wavelet_tune", min_repls = start_iter, prob = 0.95, 
                                                max_repls = 10)
  b_hierarchical_out$data_frame$start_iter <- start_iter
  b_hierarchical_results <- rbind(b_hierarchical_results, 
                                  b_hierarchical_out$data_frame)
}
# setwd("H:/MA/simulation_data")
# write.csv(b_hierarchical_results, file = "b_hierarchical_results.csv", row.names = FALSE)



# Compare to ground truth ------------------------------------------------------
ground_truth <- subset(b_hierarchical_results, start_iter == 10, 
                       select = c(algorithm, probabilities))

# rename column to merge 
colnames(ground_truth)[colnames(ground_truth) == "probabilities"] <- 
  "probabilities_10"

# merge 
b_hierarchical_comp <- merge(b_hierarchical_results, ground_truth)

# compare (1 = wrong, 0 = right <- no differences found)
for (i in 1:nrow(b_hierarchical_comp)) {
  if (identical(b_hierarchical_comp$probabilities[i], 
                b_hierarchical_comp$probabilities_10[i])){
    b_hierarchical_comp$decision[i] <- 0 
  } else {
    b_hierarchical_comp$decision[i] <- 1
  }
}

# plot error rate per iteration ------------------------------------------------
# (average errors over problemsets )
par(mfrow=c(1,2))

errors <- list()
for (i in b_hierarchical_comp$start_iter) {
  number_errors <- subset(b_hierarchical_comp, start_iter == i, 
                          select = c(decision))
  errors[i] <- colMeans(number_errors)
}

start_iter <- 1:10
plot_error <- cbind(start_iter, errors)
plot_error <- as.data.frame((plot_error))
plot_error <- plot_error[-c(1),] 
plot(plot_error, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "error rate", 
     main = "Benchmark Data")


# plot time saved per iteration ------------------------------------------------
for (i in 1:nrow(b_hierarchical_comp)) {
  b_hierarchical_comp$time[i] <- 10 - b_hierarchical_comp$repls[i]
}

time_saved <- list()
for (i in b_hierarchical_comp$start_iter) {
  subset_iter <- subset(b_hierarchical_comp, start_iter == i, select = c(time))
  time_saved[i] <- colMeans(subset_iter)/10
}

plot_time <- cbind(start_iter, time_saved)
plot_time <- as.data.frame((plot_time))
plot_time <- plot_time[-c(1),] 
plot(plot_time, type="o", col="black", ylim = c(0,1), 
     xlab = "minimum number of iterations", ylab = "time saving (%)",
     main = "Bayesian Signed Ranks test")












