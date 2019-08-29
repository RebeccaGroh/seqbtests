results <- b_corr_t_test(df= test_benchmark_small, problemset = "problem_a",
                         baseline = "algo_1")
results
class(results)


## plot für den Hierarchischen Test 
sample.a <- matrix(data.kcv.example$AlgC, byrow=TRUE, nrow=10)
sample.b <- matrix(data.kcv.example$AlgD, byrow=TRUE, nrow=10)


## welche Ergebnisse werden dafür genutzt und kann man vielleicht auch den Plot 
## von rNPBST dafür anwenden, wenn man die Variablen umbennent? 
results <- scmamp::bHierarchicalTest(sample.a, sample.b, rho=0.1, rope=c(-0.01, 0.01), nsim=2000, nchains=5)
results$posterior


scmamp::plotSimplex(results, A="Alg. C", B="Alg. D", posterior.label=TRUE, alpha=0.5)



## wie sehen dieselben Ergebnisse bei rNPBST aus, kann man das einfach umbennen 
## und dann trotzdem denselben Plot verwenden? 


ebo <- unlist(select(filter(cec17.final, Algorithm == "EBO", Dimension == 10), Result), use.names = F)
jso <- unlist(select(filter(cec17.final, Algorithm == "jSO", Dimension == 10), Result), use.names = F)


bst.results <- rNPBST::bayesianSign.test(ebo, jso,
                                         rope.min = -10, rope.max = 10)
bst.results
plot(bst.results, num.points = 10000) +
  ggplot2::labs(x = "jSO", z = "EBO")



## vielleicht noch eine Legende hinzufügen, bei der die Werte für Right left und rope gezeigt werden 

## was wichtig ist!! das kann man nur durchführen, wenn nur 2 Algorithmen miteinander verglichen werden!
## ansonsten muss eine Fehlermeldung ausgegeben werden 

#rNPBST
 # relevante Variable heißt "sample" und es ist ein Datensatz mit 3 Zeilen für left rope right 
 # und vielen Zeilen 
# in scmamp heißt die Variable $posterior --> kann man das einfach so übernehmen ? 
results_test <- b_corr_t_test(df= test_benchmark_small, problemset = "problem_a", 
                         baseline = "algo_1", algorithm = "algo_2")
results_test$extra[2]
plot_posterior(results = results_test, method = "b_corr_t_test")


plot_posterior <- function(results, method, points = 1000, plot_rope = TRUE, 
                           plot_samples = TRUE, alpha = NULL){
  if (method == "b_corr_t_test") {
    num.points <- points
    plot.rope <- plot_rope
    plot.samples <- plot_samples
    results[["additional"]] <- results$extra[1]
    results[["approximate"]] <- results$extra[2]
    results[["parameters"]] <- results$extra[3]
    results[["posterior"]] <- results$extra[4]
    scmamp::plotPosterior(results)
  }
  # if (method == "b_sign_test") {
  #   
  # }
  # if (method == "b_signed_rank_test") {
  #   
  # }
  # if (method == "b_hierarchical_test") {
  #   
  # }
}

## einezlen vorgehen -----------------------------------------------------------
# wie kommt man an die Ergebnisse: 
db <- 5
sample.a <- data.kcv.example[data.kcv.example$DB==db, "AlgA"]
sample.b <- data.kcv.example[data.kcv.example$DB==db, "AlgB"]
results <- scmamp::bCorrelatedTtest(x=sample.a, y=sample.b, rho=0.1, rope=c(-0.01, 0.01))
results
