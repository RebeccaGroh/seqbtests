


### plot posterior for bayesian correlated t test 

plot_posterior <- function(df, problemset, learner_a, learner_b, measure = NULL, 
                           parameter_algorithm = NULL, rho = 0.1, 
                           rope = c(-0.01, 0.01), parameter=1, points=1000, 
                           plot_rope=TRUE, plot_samples=TRUE, alpha=NULL) {
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problemset, learner_a, learner_b, 
                                     measure = NULL, 
                                     parameter_algorithm = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  # define samples
  x <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_a, measure]
  y <- df[df[["problem"]] == problemset 
          & df[["algorithm"]] == learner_b, measure]
  # check numbers in sample
  checkmate::assert_true(get_replications_count(x, y))
  # Bayesian correlated t Test
  results <- scmamp::bCorrelatedTtest(x, y, rho, rope)
  num.points <- points
  plot.rope <- plot_rope
  plot.samples <- plot_samples
  scmamp::plotPosterior(results)
}

#plot_posterior(df= test_benchmark_small, problemset = 'problem_c', 
#               learner_a = 'algo_1', learner_b = 'algo_2')



#------------------------------------------------------------------------------#

## plot posterior triangles for bayesian sign and signed rank test 
# Triangles for Sign and Signed Rank test 
plot_triangles <- function(df, method = "Sign", problemset = NULL, learner_a, learner_b, 
                           measure = NULL, parameter_algorithm = NULL, 
                           s = 0.5, z_0 = 0, weights = NULL, 
                           mc_samples = 1e+05, rope = c(-0.01, 0.01), points = 10000) {
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the rope 
            (min, max), but the values are not orderd. They will be swapped to
            follow with the procedure")
    rope <- sort(rope)
  }
  rope.min <- rope[1]
  rope.max <- rope[2]
  checkmate::assert_true(check_structure(df))
  checkmate::assert_true(check_names(df, problemset = NULL, learner_a, 
                                     learner_b, measure = NULL, 
                                     parameter_algorithm = NULL))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  # define samples when testing on multiple datasets
  if (is.null(problemset)) {
    data_wide <- tidyr::spread(df, algorithm, measure)
    sum_data <- aggregate(data_wide[, c(learner_a, learner_b)], 
                          by = list(data_wide[["problem"]]), FUN = mean)
    x <- sum_data[, learner_a]
    y <- sum_data[, learner_b]
  } else {
    # define samples when testing on a single dataset
    x <- df[df[["problem"]] == problemset 
            & df[["algorithm"]] == learner_a, measure]
    y <- df[df[["problem"]] == problemset 
            & df[["algorithm"]] == learner_b, measure]
  }
  if (method == "sign" || is.null(method)) {
    n.samples <- mc_samples
    # Bayesian Sign Test
    results <- rNPBST::bayesianSign.test(x, y, s, z_0, rope.min, rope.max, 
                                         weights, n.samples)
  } else if (method == "signed_rank") {
    mc.samples <- mc_samples
    # Bayesian signed rank test
    results <- rNPBST::bayesianSignedRank.test(x, y, s, z_0, 
                                               rope.min, rope.max, 
                                               weights, mc.samples)
  }
  num.points <- points
  plot(results)
}

#plot_triangles(df= test_benchmark_small, learner_a = 'algo_1', learner_b = 'algo_2')





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
