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
                                 problem = datasets, max_repls = 10, min_num = start_iter)
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


# Start: nur für einen Datensatz um alles aufzubauen 
data <- list()
for (start_iter in 2:10) {
  out_seq <- seq_b_corr_t_test(df = result_small, baseline = "ranger.pow_wavelet_tune", 
                               problem = "Adiac", max_repls = 10, min_num = start_iter)
  out_seq$data_frame$start_iter <- start_iter
  out_seq$data_frame$problem <- "Adiac"
  data <- rbind(data, out_seq$data_frame)
}
data


# Neue Spalte erstellen in der der Wert 0 ist, wenn das Ergebnis mit dem Ergebnis 
# aus der letzten Repliaktion übereinstimmt und 1 wenn sie sich unterscheiden. 
# In der letzten Replikation sind alle Werte bei 0 
# zunächst Gruppen erstellen: Auf der Grundlage von "algorithm" (später kommt 
# noch "problem" dazu.)
# Bedingung formulieren: Innerhalb der festgelegten Gruppe, wenn probabilities 
# gleich probabilities in der letzten Beobachtung der  Gruppe, dann erstelle 
# data$decision = 0. Wenn sie sich unterscheiden, dann erstelle data$decision = 1. 
# So können am Ende die falschen Entscheidungen gezählt werden. Die Anzahl der 
# Entscheidungen wird durch die Algorithms bestimmt. 
for (i in 1:nrow(data)) {
  if (data$probabilities[i] == # hier muss das Ergebnis aus der letzten Replikation aufgerufen werden) {
      data$decision == 0
} else {
  data$decision == 1
  }
}
# für diese Gruppen muss dann die Anzahl an falschen Entscheidungen gezählt werden 
# dann kann der Plot aufgebaut werden 

# benötigt: Gruppen Variable (aber das ist eigentlich schond er Algorithmus) 
# (später kann man über paste aus dem Algorithmus und dem Problem eine Gruppenvariable erstellen)
# data <- data[order(data$start_iter),]
# data_10 <- by(data, data$start_iter, tail, n=1)
# highestd<-do.call("rbind", as.list(data_10))

data$diff <- ave(data$start_iter, data$algorithm, FUN=function(x) c(0, diff(x)))


highest<-by(hsb2.s, hsb2.s$prog, tail, n=1)
# so kann man die letzten Replikationen bekommen und die probabilities daraus werden 
# dann einfach wieder den anderen Daten als column hinzugefügt (dafür Datensatz 
# reduzieren, sodass nur noch algorithm und problem da sind, weil man hierdurch 
# alle Beobachtungen zuordnen kann)