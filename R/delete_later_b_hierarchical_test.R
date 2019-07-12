


x.matrix First sample, a matrix with the results obtained by the first algorithm (each dataset in a row)


## versuchen den Sample zu erstellen: 
## --> die samples für die einzelnen problemsets in einer zeile statt eines Vektors speicher 
## und dann mit rbind zu einer Matrix zusammenfügen  

x <- benchmark_test_full[benchmark_test_full$problem == "Adiac" 
                         & benchmark_test_full$algorithm == "classif.ksvm", 
                         "measure_mmce"]
y <- benchmark_test_full[benchmark_test_full$problem == "Adiac" 
                         & benchmark_test_full$algorithm == "classif.glmnet", 
                         "measure_mmce"]
## zusammenfügen zu matrix
typeof(x)
is.vector(x)

test <- rbind(x, y)

# über eine Schleife kann man vielleicht festlegen dass für alle Werte von dem 
# algorithmus ein Vektor erstellt wird und dann wird über rbind die Matrix gebaut 

problems <- unique(benchmark_test_full[["problem"]])



for (k in 1:length(problems)) {
  sample <- list()
  sample[problems[[k]], k] <- benchmark_test_full[benchmark_test_full$problem == k 
                                   & benchmark_test_full$algorithm == "classif.ksvm", 
                                   "measure_mmce"]
  test <- rbind(sample)
  
}
sample <- matrix(length(problems),)


## dann funktioniert aber auch das mit dem Pasten nicht mehr!! wenn man Learner_a
## und Learner_B erstellen mmöchte... dafür muss auch eine elegantere Lösung gefunden werden 
#-------------------------------------------------------------------------------


bHierarchicalTest <- function(x.matrix, y.matrix=NULL, rho, std.upper=1000, d0.lower=NULL, d0.upper=NULL, 
                              alpha.lower=0.5, alpha.upper=5, beta.lower=0.05, beta.upper=0.15, 
                              rope=c(-0.01, 0.01), nsim=2000, nchains=8, parallel=TRUE, stan.output.file=NULL,
                              seed=as.numeric(Sys.time()), ...) {
  
  
  
  if (!require(rstan)) {
    stop("This function requires the rstan package. Please install it and try again.")
  }  
  
  if (!require(metRology)) {
    stop("This function requires the metRology package. Please install it and try again.")
  }
  
  
  if (parallel) {
    rstan_options(auto_write = TRUE)
    options(mc.cores = parallel::detectCores())
  }
  
  if (!is.null(stan.output.file)) {
    rstan_options(auto_write = TRUE)
    if (!dir.exists("./stan_out")) {
      dir.create("./stan_out")
    }
    stan.output.file <- paste0("./stan_out/",stan.output.file,".StanOut")
  }
  
  if (rope[2] < rope[1]) {
    warning("The rope paremeter has to contain the ordered limits of the rope
            (min, max), but the values are not orderd. They will be swapped to
            follow with the procedure")
    
    rope <- sort(rope)
  }
  
  if (is.null(y.matrix)) {
    sample.matrix <- x.matrix
  } else {
    sample.matrix <- x.matrix - y.matrix
  }
  
  # Check the input data (we need a matrix or a data.frame)
  if (class(sample.matrix) == "numeric") {
    sample.matrix <- matrix(sample.matrix, nrow=1)
  }
  
  
  # Code inherited from BayesianTestML/tutoria/hierarchical/hierarchical_test
  # Not sure the reason for this check (in the context of the original code, x
  # values should be bounded to the (-1, 1) interval)
  if ((max(sample.matrix))>1 & rope[2] < 0.02) {
    stop('value of rope_max  not compatible with scale of provided x')
  }
  
  num.samples  <- ncol(sample.matrix)
  num.datasets <- nrow(sample.matrix)
  
  # Scale the problem according to the mean standard deviation of all the datasets
  dataset.sds <- apply(sample.matrix, MARGIN=1, FUN=sd)
  mean.dataset.sd <- mean(dataset.sds)
  
  # Save this value for undoing the scaling in the results
  scale.factor <- mean.dataset.sd
  
  sample.matrix <- sample.matrix / mean.dataset.sd
  
  # Update also the rope
  rope <- rope / mean.dataset.sd
  
  # Also update the limits for d0 in case they are provided
  if (!is.null(d0.lower)) {
    d0.lower <- d0.lower / mean.dataset.sd
  }
  
  if (!is.null(d0.upper)) {
    d0.upper <- d0.upper / mean.dataset.sd
  }
  
  
  # In case there is any dataset with 0 variance, add a small value to avoid problems
  # taking care to not alter the mean value
  for (id in which(dataset.sds==0)) {
    noise <- runif(num.samples/2, rope[1], rope[2])
    sample.matrix[id, 1:(num.samples/2)] <- sample.matrix[id, 1:(num.samples/2)] + noise
    sample.matrix[id, (num.samples/2 + 1):num.samples] <- sample.matrix[id, (num.samples/2 + 1):num.samples] - noise
  }
  
  # Just in case, as the sd of those datasets with sd=0 has changed
  # The mean sd of the dasets is related with the upper bound of the sd
  dataset.sds     <- apply(sample.matrix, MARGIN=1, FUN=sd)
  mean.dataset.sd <- mean(dataset.sds)
  
  # For the upper bound of the sd0, we get the sd of the mean values per
  # dataset. In case we only have one, the upper bound is set using its sd
  if (num.samples==1) {
    dataset.mean.sd <- sd(sample.matrix)
  } else {
    dataset.mean.sd <- sd(apply(sample.matrix, MARGIN=1, FUN=mean))
  }
  
  if (is.null(d0.lower)) {
    d0.lower <- -max(abs(sample.matrix))
  }
  
  if (is.null(d0.upper)) {
    d0.upper <- max(abs(sample.matrix))
  }
  
  data <- list()
  
  data$deltaLow   <- d0.lower
  data$deltaHi    <- d0.upper
  data$stdLow     <- 0
  data$stdHi      <- mean.dataset.sd*std.upper
  data$std0Low    <- 0
  data$std0Hi     <- dataset.mean.sd*std.upper
  data$Nsamples   <- num.samples
  data$q          <- num.datasets 
  data$x          <- sample.matrix 
  data$rho        <- rho
  data$upperAlpha <- alpha.upper
  data$lowerAlpha <- alpha.lower
  data$upperBeta  <- beta.upper
  data$lowerBeta  <- beta.lower
  
  stan.program <- system.file("stan/hierarchical_t_test.stan", package="scmamp")
  
  stan.fit <-  stan(file=stan.program, data=data, iter=nsim, chains=nchains,
                    seed=seed, sample_file=stan.output.file, ...)
  
  stan.results<- extract(stan.fit, permuted=TRUE)
  
  # Remove irrelevant variables
  stan.results$diff<-NULL
  stan.results$diagQuad<-NULL
  stan.results$oneOverSigma2<-NULL
  stan.results$nuMinusOne<-NULL
  stan.results$log_lik<-NULL
  
  
  # Once simulated we now get the relevant probabilities, starting with each dataset
  aux <- apply(stan.results$delta, MARGIN=2, 
               FUN=function(i){
                 res <- data.frame(Left=mean(i<rope[1]),
                                   Right=mean(i>rope[2]),
                                   Rope=mean(i<=rope[2] & i>=rope[1]))
               })
  
  probs.per.dataset <- do.call(rbind, aux)
  
  # Analyse the posterior distribution of delta parameter to check the most probable 
  # outcome for the next delta (future datasets)
  aux <- pt.scaled(rope[2], df=stan.results$nu, mean=stan.results$delta0, sd=stan.results$std0)
  cum.left <- pt.scaled(rope[1], df=stan.results$nu, mean=stan.results$delta0, sd=stan.results$std0)
  cum.rope <- aux - cum.left
  cum.right <- 1 - aux
  
  posterior.distribution <- data.frame("Left"=cum.left, "Rope"=cum.rope, "Right"=cum.right)
  
  left.wins <- cum.left > cum.right & cum.left > cum.rope
  right.wins <- cum.right > cum.left & cum.right > cum.rope
  # Difference with the original code in BayesianTestsML/tutorial. In case of ties
  # the point goes to the rope, in order to be more conservative
  rope.wins <- !(left.wins | right.wins)
  
  positive.d0 <- stan.results$delta0 > 0 
  
  # Get the probabilities according to the counts
  prob.left.win  <- mean(left.wins)
  prob.right.win <- mean(right.wins)
  prob.rope.win  <- mean(rope.wins)
  
  prob.positive  <- mean(positive.d0)
  prob.negative  <- 1 - prob.positive
  
  # Get the results ready
  
  # Remember that the differences had been scaled, so we need to revert that scaling
  per.dataset <- cbind("mean.delta" = colMeans(stan.results$delta)*scale.factor,
                       probs.per.dataset)
  
  global.sign <- c(prob.negative, prob.positive)
  names(global.sign) <- c("Negative", "Positive")
  
  global.wins <- c(prob.left.win, prob.rope.win, prob.right.win)
  names(global.wins) <- c("Left",  "Rope", "Right")
  
  parameters <- list(rho=rho, std.upper=std.upper, d0.lower=d0.lower, d0.upper=d0.upper,
                     alpha.lower=alpha.lower, alpha.upper=alpha.upper, 
                     beta.lower=beta.lower, beta.upper=beta.upper,
                     rope=rope, nsim=nsim, nchains=nchains)
  
  additional <- list(per.dataset=per.dataset, global.sign=global.sign, stan.results=stan.results)
  
  
  results <- list()
  results$method                  <- "Hierarchical Bayesian correlated model"
  results$parameters              <- parameters
  results$posterior.probabilities <- global.wins
  results$approximate             <- TRUE
  results$posterior               <- posterior.distribution
  results$additional              <- additional
  
  
  return(results)
}