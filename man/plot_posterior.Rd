% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_posterior}
\alias{plot_posterior}
\title{Plotting the posterior densities in Bayesian analyses}
\usage{
plot_posterior(results, method, points = 1000)
}
\arguments{
\item{results}{(`character`)\cr Results of the Bayesian analysis performed 
before.}

\item{method}{(`character`)\cr The Bayesian test that has been performed. 
Either "b_corr_t_test", b_sign_test", "b_signed_rank_test" or 
"b_hierarchical_test".}

\item{points}{(`double`)\cr Number of points used to plot the function.}
}
\value{
(`list`) \cr List containing a `gg` object.
}
\description{
This function plots the posterior densities obtained in Bayesian 
    analyses. It plots either univariately the posterior densities obtained 
    via Bayesian correlated t tests. Or, if either a Bayesian Sign, Signed 
    Rank or Hierarchical correlated t test have been used, it plots the 
    projection of 3-simplex points into a 2D triangle.
}
\details{
The plot for Bayesian correlated t tests has first been implemented in 
    scmamp. The plots for the other Bayesian tests have first been 
    implemented in rNPBST.
}
\examples{
    \dontrun{
      results <- b_signed_rank_test(df= test_benchmark_small,
        baseline = "algo_1", algorithm = "algo_2")
      plot_posterior(results, method = "b_signed_rank_test")
    }
}
\references{
\url{https://github.com/b0rxa/scmamp}
    \url{https://github.com/JacintoCC/rNPBST}
}
