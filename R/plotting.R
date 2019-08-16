









posterior_triangels <- function(results, num_points = nrow(results$sample),...) {
  x <- results
  num.points <- num_points 
  # plot.PosteriorDirichlet from rNPBST
  rNPBST::plot.PosteriorDirichlet(x, num.points,....)
}


posterior_triangels(bst.results)
















bst.results <- rNPBST::bayesianSign.test(EBO, jSO,
                                         rope.min = -10, rope.max = 10)
plot(bst.results, num.points = 10000) +
  ggplot2::labs(x = "jSO", z = "EBO")

results <- b_sign_test(df= test_benchmark_small, 
                      problemset = 'problem_a', 
                      learner_a = 'algo_1', learner_b = 'algo_2')
plot()
plot(results, num.points = 10000) +
  ggplot2::labs(x = "EBO", z = "jSO")


# posterior Triangles 
# from rnpsbt 


#------------------------------------------------------------------------------#

# Boxplot 
box_plot <- function(df, measure = NULL) {
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  ggplot(df, aes_string(x = as.factor(df[["algorithm"]]), y = measure)) +
    geom_boxplot(aes(fill = df[["algorithm"]])) + 
    scale_y_continuous(name = "Measure") +
    scale_x_discrete(name = "Algorithms") +
    ggtitle("Boxplot of mean measure by algorithms") +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(. ~ problem)
}
#box_plot(df = test_benchmark_small)

#' @title Posterior densities plot 
#' @description This function plots, univariately, the posterior densities 
#' obtained in Bayesian analyses. 
#' @param results A list containing, at least three elements, one named 
#' \code{approximate}, which is a logical value indicating whether the posterior 
#' is a function or a sample, \code{rope}, a two-dimensional vector with the 
#' minimum and maximum values of the rope and \code{posterior}, either a one 
#' parameter function or a matrix (or data.frame) where each row is a sample and 
#' each column a sampled parameter
#' @param parameter Either a string or a number indicating, in case the 
#' posterior is approximated, the parameter to be plotted (i.e., the name or the 
#' index of a column in the sample matrix)
#' @param ... Additional parameters to the Rgraphviz function. This is mainly 
#' to change the layout of the graph
#' @param plot.rope  A logical value indicating whether the rope has to be 
#' plotted or not. Note that not for all
#' parameter the rope makes sense
#' @param num.points Number of points used to plot the functions
#' @param plot.samples A logical value. If true, the samples are plotted (only 
#' when the posterior is approximate)
#' @param alpha Numeric value for the transparency of the points, only 
#' applicable if \code{plot.samples} is true
#' @return An object of class \linkS4class{ggplot} with the plot
#' @details The test has first been implemented in scmamp.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @export
plot_posteriror <- function(results, parameter = 1, num.points = 1000, plot.rope = TRUE, plot.samples = TRUE, alpha = NULL, 
    ...) {
    scmamp::plotPosterior(results, parameter = 1, num.points = 1000, plot.rope = TRUE, plot.samples = TRUE, alpha = NULL, 
        ...)
}



#' @title Critical differences plot 
#' @description This function plots the critical differences plot. 
#' @param df Input data frame.
#' @param measure Measure column. 
#' @param alpha Significance level to get the critical difference.
#' @param cex Numeric value to control the size of the font. 
#' @details The test has first been implemented in scmamp.
#' Note that the default value for measure is the first measure column in the 
#' data frame. By default, the alpha value is 0.05 and the default for cex is 
#' 0.75.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @9examples
#' plot_cd(test_benchmark)
#' @export
plot_cd <- function(df, measure = NULL, alpha = 0.05, cex = 0.75, ...) {
    checkmate::assert_true(check_structure(df))
    if (is.null(measure)) {
        measure <- get_measure_columns(df)[1]
    }
    algo_names <- unique(df$algorithm)
    data_wide <- tidyr::spread(df, algorithm, measure)
    sum_data <- aggregate(data_wide[, algo_names], by = list(data_wide[["problem"]]), FUN = mean)
    # define dataset
    results.matrix <- data.frame(sum_data[, -1], row.names = sum_data[, 1])
    # plot CD
    scmamp::plotCD(results.matrix, alpha, cex)
}
## oder soll der Plot lieber direkt auf ein Ergebnis angewendet werden? 
## bzw. die Plots sollten halt einheitlich von der Eingabe gehalten werden 
plot_cd(test_benchmark)

EBO <- unlist(select(filter(cec17.final, Algorithm == "EBO", Dimension == 10), Result), use.names = F)
jSO <- unlist(select(filter(cec17.final, Algorithm == "jSO", Dimension == 10), Result), use.names = F)





## alle Plots von scmamp testen 

algorithms <- names(data.kcv.example)[4:7]
db <- 5
plotDensities (data=data.kcv.example[data.kcv.example$DB==db, algorithms], size=1.1)


db <- 5
sample.a <- data.kcv.example[data.kcv.example$DB==db, "AlgA"]
sample.b <- data.kcv.example[data.kcv.example$DB==db, "AlgB"]
results <- bCorrelatedTtest(x=sample.a, y=sample.b, rho=0.1, rope=c(-0.01, 0.01))
results$posterior.probabilities
plotPosterior(results, plot.rope=TRUE)


db <- 5
summarized.data <- aggregate(data.kcv.example[, algorithms], 
                             by=data.kcv.example[, 1:2], FUN=mean)
sample.a <- summarized.data[summarized.data$DB==db, "AlgC"]
sample.b <- summarized.data[summarized.data$DB==db, "AlgD"]

results <- bSignedRankTest(x=sample.a, y=sample.b,rope=c(-0.01, 0.01))
results$posterior.probabilities
plotSimplex(results, A="Algorithm C", B="Algorithm D")
