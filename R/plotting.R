#' @title Plot Densities 
#' @description This function estimates and plots the densities of the 
#' performances of each algorithm in the data frame  
#' @param df Input data frame.
#' @param measure Measure column. 
#' @return A \code{\linkS4class{ggplot}} object.
#' @details The test has first been implemented in scmamp.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @example 
#'     plot_densities(test_benchmark_small)
#' @export
plot_densities <- function(df, measure = NULL) {
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algo_names <- unique(df$algorithm)
  data_wide <- tidyr::spread(df, algorithm, measure)
  sum_data <- aggregate(data_wide[, algo_names], 
                        by = list(data_wide[["problem"]]), FUN = mean)
  # define dataset
  results_matrix <- data.frame(sum_data[, -1], row.names = sum_data[, 1])
  # plot densities 
  scmamp::plotDensities(results_matrix)
}



#' @title Boxplot  
#' @description This function plots the performance of every algorithm in each 
#'     data frame in boxplots.
#' @param df Input data frame. 
#' @param measure Measure column. 
#' @return A \code{\linkS4class{ggplot}} object.
#' @details The test has first been implemented in scmamp.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @example 
#'     plot_boxplot(df = test_benchmark_small)
#' @export
plot_boxplot <- function(df, measure = NULL) {
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  ggplot(df, aes_string(x = as.factor(df[["algorithm"]]), y = measure)) +
    geom_boxplot(aes(fill = df[["algorithm"]])) + 
    scale_fill_discrete(name="Algorithms") +
    scale_y_continuous(name = "Measure") +
    scale_x_discrete(name = "Algorithms") +
    ggtitle("Boxplot of mean measure by algorithms") +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(. ~ problem) 
}


#' @title Critical differences plot 
#' @description This function implements the critical difference plots 
#'     introduced in Demsar (2006).
#' @param df Input data frame.
#' @param measure Measure column. 
#' @param alpha Significance level to get the critical difference.
#' @param cex Numeric value to control the size of the font. 
#' @return A \code{\linkS4class{ggplot}} object.
#' @details 
#'     The test has first been implemented in scmamp. 
#'     Note that if no measure column is defined per default the first column 
#'     defined as measure_* in the data frame is used. By default, the alpha 
#'     value is 0.05 and the default for cex is 0.75.
#' @references \url{https://github.com/b0rxa/scmamp}
#' @examples 
#'     plot_cd(test_benchmark)
#' @export
plot_cd <- function(df, measure = NULL, alpha = 0.05, cex = 0.75, ...) {
  checkmate::assert_true(check_structure(df))
  if (is.null(measure)) {
    measure <- get_measure_columns(df)[1]
  }
  algo_names <- unique(df$algorithm)
  data_wide <- tidyr::spread(df, algorithm, measure)
  sum_data <- aggregate(data_wide[, algo_names], 
                        by = list(data_wide[["problem"]]), FUN = mean)
  # define dataset
  results_matrix <- data.frame(sum_data[, -1], row.names = sum_data[, 1])
  # plot CD
  scmamp::plotCD(results_matrix, alpha, cex)
}


#' @title Plotting the posterior densities in Bayesian analyses 
#' @description This function plots the posterior densities obtained in Bayesian 
#'     analyses. It plots either univariately the posterior densities obtained 
#'     via Bayesian correlated t tests. Or, if either a Bayesian Sign, Signed 
#'     Rank or Hierarchical correlated t test have been used, it plots the 
#'     projection of 3-simplex points into a 2D triangle. 
#' @param results Results of the Bayesian analysis performed before. 
#' @param method The Bayesian test that has been performed. Either
#'     "b_corr_t_test", b_sign_test", "b_signed_rank_test" or 
#'     "b_hierarchical_test".
#' @param points Number of points used to plot the function. 
#' @return A \code{\linkS4class{ggplot}} object.
#' @details 
#'     The plot for Bayesian correlated t tests has first been implemented in 
#'     scmamp. The plots for the other Bayesian tests have first been 
#'     implemented in rNPBST.
#' @references 
#'     \url{https://github.com/b0rxa/scmamp}
#'     \url{https://github.com/JacintoCC/rNPBST}
#' @example 
#'     results <- b_corr_t_test(df= test_benchmark_small, 
#'                              problem = "problem_a", 
#'                              baseline = "algo_1", algorithm = "algo_2")
#'     plot_posterior(results, method = "b_corr_t_test")
#' @export
plot_posterior <- function(results, method, points = 1000){
  if (method == "b_corr_t_test") {
    test <- list()
    tdist.df <- as.numeric(results[["extra"]][7])
    tdist.mean <- as.numeric(results[["extra"]][8])
    tdist.sd <- as.numeric(results$extra[9])
    ppos <- function(mu) {
      #Standarize the value and get the density
      x <- (mu-tdist.mean)/tdist.sd
      return(pt(x,tdist.df))
    }
    qpos <- function(q) {
      return(qt(q,tdist.df) * tdist.sd + tdist.mean)
    }
    dpos <- function(mu) {
      #Standarize the value and get the density
      x <- (mu-tdist.mean)/tdist.sd
      return(dt(x,tdist.df))
    }
    test[["additional"]] <- list(pposterior = ppos, 
                                 qposterior = qpos, 
                                 posterior.df = tdist.df,  
                                 posterior.mean = tdist.mean, 
                                 posterior.sd = tdist.sd)
    test[["approximate"]] <- as.logical(results[["extra"]][2])
    test[["parameters"]] <- as.data.frame(results[["extra"]][3])
    test[["posterior"]] <- dpos
    scmamp::plotPosterior(results = test, num.points = points, plot.rope = TRUE, 
                          plot.samples = TRUE, alpha)
  } else if (method == "b_sign_test" | method == "b_signed_rank_test") {
    test <- list()
    test[["sample"]] <- as.data.frame(results[["extra"]])
    plot_triangles(x  = test, num.points = points )
  } else if (method == "b_hierarchical_test") {
    results[["sample"]] <- as.data.frame(results[["extra"]][4])
    plot_triangles(x  = results, num.points = points )
  } else {
    warning("Method must be correctly specified.")
  }
}

results <- b_corr_t_test(df= test_benchmark_small, 
                         problem = "problem_a", 
                         baseline = "algo_1", algorithm = "algo_2")
plot_posterior(results, method = "b_corr_t_test")
