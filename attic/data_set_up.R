
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
colnames(result_small)[colnames(result_small) == "repls"] <- "replications"


# knn_dtw_tuned | ranger_none_tuned | ranger_wavelet_tuned
# lrn.cl_feat.extract.method_tune? 

# kann mit dem Datensatz jetzt gearbeitet werden? durchprobieren, welcher 
# Algorithmus als Baseline verwendet werden soll 

## Datensatz fertig aufbereitet 