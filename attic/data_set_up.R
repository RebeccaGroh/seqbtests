
# drop time_queued, time_running, n, ntrain, ntest, length, nclasses, minorityclass_size,
# ber, timeboth 
result_small <- subset(result, tune == "tune", 
                       select= -c(time.queued, time.running, n, ntrain, ntest, 
                                  length, nclasses, feat.extract.method, 
                                  minorityclass_size, algo.type,
                                  ber, timeboth, job.id, tune))

## check for duplicates (drops rows that are duplicated)
result_small <- unique(result_small)

View(result_small)

result_small$algo.pars[44]

# knn_dtw_tuned | ranger_none_tuned | ranger_wavelet_tuned
# lrn.cl_feat.extract.method_tune? 

# kann man vorher einfach die notwendigen Cols zusammen pasten und dann 
# alles andere löschen? dann muss man nicht auf die Liste in algo.pars zugreifen 

# relevante Variablen: lrn.cl, ft.extract.method, tune.ctrl.method

unique(result$tune)
unique(result$feat.extract.method)
unique(result$lrn.cl)

# drop none_tuned learners

require(stringr)
class(result$lrn.cl)
result$lrn.cl <- str_sub(result$lrn.cl, start=8L, end=30L)
unique(result$lrn.cl)
