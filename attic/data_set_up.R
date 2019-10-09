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

View(result)
View(result_small)
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
data_final <- list()

for (datasets in problems) {
  for (start_iter in 2:10) {
    out_seq <- seq_b_corr_t_test(df = result_small, baseline = "ranger.pow_wavelet_tune", 
                                 problem = "Adiac", max_repls = 10, min_num = start_iter)
    out_seq$data_frame$start_iter <- start_iter
    out_seq$data_frame$problem <- datasets
    data <- rbind(data, out_seq$data_frame)
  }
  data_final <- rbind(data_final, data)
}
data_final
## Ergebnis nach 10 Replikationen ist das richtige! 
## Also muss das Ergebnis mit den vorherigen Ergebnissen verglichen werdne (für 
## alle Zeilen die sich nur anhand von der Anzahl an Replikationen unterscheiden)
## 

# for (start_iter in 2:10) {
#   out_seq <- seq_b_corr_t_test(df = result_small, baseline = "ranger.pow_wavelet_tune", 
#                                problem = "Adiac", max_repls = 10, min_num = start_iter)
#   out_seq$data_frame$start_iter <- start_iter
#   data <- rbind(data, out_seq$data_frame)
# }
# data

# funktioniert, allerdings wird der Datensatz beim Loop über alle Problemsets 
# riesig! --> in (vermutlich) allen Problemsets wird dieselbe Entscheidung 
# getroffen, also wäre es sinnvoll sich auf ein Problemset zu konzentrieren 
# muss ich aber wahrscheinlich absprechen 
# muss auch noch nachfragen, weil ich in dem Datensatz nur 10 Replikationen 
# vorliegen habe statt 20 

