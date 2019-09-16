
# drop time_queued, time_running, n, ntrain, ntest, length, nclasses, minorityclass_size,
# ber, timeboth 
result_small <- subset(result, select= -c(time_queued, time_running, n, ntrain, 
                                          ntest, length, nclasses, 
                                          minorityclass_size,
                                          ber, timeboth))

duplicated(result)
View(sysdata.rda)
