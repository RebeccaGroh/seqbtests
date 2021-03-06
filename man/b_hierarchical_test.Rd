% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesian_tests.R
\name{b_hierarchical_test}
\alias{b_hierarchical_test}
\title{Bayesian hierarchical correlated t-test}
\usage{
b_hierarchical_test(
  df,
  baseline,
  algorithm = NULL,
  measure = NULL,
  rho = 0.1,
  compare = NULL,
  std.upper = 1000,
  d0.lower = NULL,
  d0.upper = NULL,
  prob = 0.95,
  alpha.lower = 0.5,
  alpha.upper = 5,
  beta.lower = 0.05,
  beta.upper = 0.15,
  rope = c(-0.01, 0.01),
  nsim = 2000,
  parallel = TRUE,
  stan.output.file = NULL,
  nchains = 8,
  seed = as.numeric(Sys.time()),
  adapt_delta = 0.8,
  max_treedepth = 10
)
}
\arguments{
\item{df}{(`list`)\cr Data frame containing the performane measure.}

\item{baseline}{(`character`)\cr First algorithm. Value in 'algorithm'
column.}

\item{algorithm}{(`character`)\cr Second algorithm. Value in 'algorithm' 
column. If not defined, the baseline is tested against all algorithms 
in the data frame.}

\item{measure}{(`character`)\cr Name of the 'measure' column. If not 
defined, the first 'measure' column in the data frame is used.}

\item{rho}{(`double`)\cr Correlation factor. Default is 0.1.}

\item{compare}{(`character`)\cr Defines if one algorithm needs to perform 
better ({\code{better}}) for decisions based on the posterior 
distribution or whether it is sufficient to perform not worse 
({\code{equal}}).}

\item{std.upper}{(`double`)\cr Factor to set the upper bound for both sigma_i 
and sigma_0. Default is 1000.}

\item{d0.lower}{(`any`)\cr Lower bound for the prior for mu_0. If not 
provided, the smallest observed difference is used.}

\item{d0.upper}{(`any`)\cr Upper bound for the prior for mu_0. If not 
provided, the biggest observed difference is used.}

\item{prob}{(`double`)\cr Threshold probability that decision rely on. 
Default is 0.95.}

\item{alpha.lower}{(`double`)\cr Lower bound for the (uniform) prior for the 
alpha hyperparameter. Default is 0.5.}

\item{alpha.upper}{(`double`)\cr Upper bound for the (uniform) prior for the 
alpha hyperparameter. Default is 0.5.}

\item{beta.lower}{(`double`)\cr Lower bound for the (uniform) prior for the
beta hyperparameter. Default is 0.5.}

\item{beta.upper}{(`double`)\cr Upper bound for the (uniform) prior for the
beta hyperparameter. Default is 0.5.}

\item{rope}{(`double`)\cr Region of practical equivalence. Default is 
c(-0.01, 0.01).}

\item{nsim}{(`double`)\cr Number of samples (per chain) used to estimate the 
posterior distribution. Note that, by default, half the simulations are 
used for the burn-in.}

\item{parallel}{(`logical`)\cr If \code{true}, Stan code is executed in 
parallel.}

\item{stan.output.file}{(`character`)\cr String containing the base name for 
the output files produced by Stan. If \code{NULL}, no files are stored.}

\item{nchains}{(`double`)\cr Number of MC chains to be simulated. As half the 
simulations are used for the warm-up, the total number of simulations  
will be \code{nchain}*\code{nsim}/2.}

\item{seed}{(`double`)\cr Optional parameter used to fix the random seed.}

\item{adapt_delta}{(`double`)\cr Average proposal acceptance probability 
during Stan’s adaptation period.}

\item{max_treedepth}{(`double`)\cr  Maximum treedepth parameter.}
}
\value{
(`list`)\cr A list containing the following components:
\itemize{
    \item{\code{measure}} (`character`)\cr A string with the name of the 
        measure column. 
    \item{\code{method}} (`character`)\cr A string with the name of the 
        method. 
    \item{\code{baseline}} (`character`)\cr A string with the name of the 
        first algorithm. Value in 'algorithm' column. 
    \item{\code{data_frame}} (`list`)\cr  A list containing the following 
        components:
    \item{\code{algorithm}} (`character`)\cr Second algorithm. Value in 
        'algorithm' column. If not defined, the baseline is tested against 
        all algorithms in the data frame. 
    \item{\code{left}} (`double`)\cr Left probability. 
    \item{\code{rope}} (`double`)\cr Rope probability. 
    \item{\code{right}} (`double`)\cr Right probability. 
    \item{\code{probabilities}} (`character`)\cr Decisions based on posterior 
        probabilities and threshold probability. 
}
}
\description{
This function implements a Bayesian hierarchical test. The performance of 
    one baseline algorithm on multiple data set is compared to either one or
    multiple algorithms.
}
\details{
The test has first been implemented in scmamp. 
    Note that if no measure column is defined per default the first column 
    defined as measure_* in the data frame is used. The default of rho is 
    0.1.
}
\examples{
    \dontrun{
      results <- b_hierarchical_test(df= test_benchmark_small, 
        baseline = "algo_1", algorithm = "algo_3",  rho=0.1, 
        rope=c(-0.01, 0.01))
    }
}
\references{
\url{https://github.com/b0rxa/scmamp}
}
