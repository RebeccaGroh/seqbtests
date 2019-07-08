
#' @title Posterior denities plot 
#' @description This function plots, univariately, the posterior densities 
#' obtained in Bayesian analyses. 
#' @param results A list containing, at least three elements, one named \code{approximate}, which is
#' a logical value indicating whether the posterior is a function or a sample, \code{rope}, a two dimensional
#' vector with the minimum and maximum values of the rope and \code{posterior}, either a one parameter function or
#' a matrix (or data.frame) where each row is a sample and each column a sampled parameter
#' @param parameter Either a string or a number indicating, in case the posterior is approximated, the parameter
#' to be ploted (i.e., the name or the index of a column in the sample matrix)
#' @param ... Additional parameters to the Rgraphviz function. This is mainly to change the layout of the graph
#' @param plot.rope  A logical value indicating whether the rope has to be plotted or not. Note that not for all
#' parameter the rope makes sense
#' @param num.points Number of points used to plot the functions
#' @param plot.samples A logical value. If true, the samples are plotted (only when the posterior is approximate)
#' @param alpha Numeric value for the transparency of the points, only applicable if \code{plot.samples} is true
#' @return An object of class \linkS4class{ggplot} with the plot
#' @export

plot_posteriror <- function(results, parameter=1, num.points=1000, plot.rope=TRUE, plot.samples=TRUE, alpha=NULL, ...) {
  scmamp::plotPosterior(results, parameter=1, num.points=1000, plot.rope=TRUE, plot.samples=TRUE, alpha=NULL, ...)
}


