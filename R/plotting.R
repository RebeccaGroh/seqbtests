
#' @title Posterior denities plot 
#' @description This function plots, univariately, the posterior densities 
#' obtained in Bayesian analyses. 
#' @param results A list containing, at least three elements, one named 
#' \code{approximate}, which is a logical value indicating whether the posterior 
#' is a function or a sample, \code{rope}, a two dimensional vector with the 
#' minimum and maximum values of the rope and \code{posterior}, either a one 
#' parameter function or a matrix (or data.frame) where each row is a sample and 
#' each column a sampled parameter
#' @param parameter Either a string or a number indicating, in case the 
#' posterior is approximated, the parameter to be ploted (i.e., the name or the 
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
plot_posteriror <- function(results, parameter=1, num.points=1000, plot.rope=TRUE, plot.samples=TRUE, alpha=NULL, ...) {
  scmamp::plotPosterior(results, parameter=1, num.points=1000, plot.rope=TRUE, plot.samples=TRUE, alpha=NULL, ...)
}



#' @title Critical differences plot 
#' @description This function plots the critical differences plot. 
#' @param df Input data frame.
#' @param measure Measure column. 
#' @param alpha Significance level to get the critical difference.
#' @param cex Numeric value to control the soze of the font. 
#' @details The test has first been implemented in scmamp.
#' Note that the default value for measure is the first measure column in the 
#' data frame. By default the alpha value is 0.05 and the default for cex is 
#' 0.75.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @9examples
#' plot_cd(test_benchmark)
#' @export
plot_cd <- function(df, measure = NULL, alpha = 0.05, cex = 0.75,...) {
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  } 
  algo_names <- unique(df$algorithm)
  data_wide <- spread(df, algorithm, measure)
  sum_data <- aggregate(data_wide[, algo_names],
                        by = list(data_wide[["problem"]]), FUN = mean)
  # define dataset
  results.matrix <- data.frame(sum_data[,-1], row.names=sum_data[,1])
  # plot  CD
  plotCD(results.matrix, alpha, cex)
}


