
# first set seed. 
set.seed(123456)

# for(start_iter in c(5, 10, 30)){
#   for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
#     for (sigma in c(0.01, 0.02, 0.05, 0.1, 0.2)) {
#       df <- generate_data(start_iter, mu, delta_mean, sigma)
#       out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", problem = "problem_1", max_repls = start_iter)
#     }
#   }
# }



start_iter = 5
delta_mean = 0
sigma =  0.01



generate_data <- function( start_iter, mu, delta_mean, sigma){
  data <- data.frame(algorithm = rep(c("algo_a", "algo_b"), start_iter), 
                     replications = rep(1:start_iter, times = 2), 
                     measure_accuracy = rnorm(n = start_iter, mean = c(mu, mu + delta_mean), sd = sigma))
  data$problem <- "problem_1"
  return(data)
}
mu = 0.7

data_sigma = list()
data_delta = list()
data_repls = list()
for(start_iter in c(5, 10, 30)){
  for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
    for (sigma in c(0.01)) {
      df <- generate_data(start_iter, mu, delta_mean, sigma)
      out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", problem = "problem_1", max_repls = start_iter)
      out_seq$data_frame$sigma <- sigma 
      out_seq$data_frame$mu <- mu
      out_seq$data_frame$delta <- delta_mean 
      out_seq$data_frame$start_iter <- start_iter
      data_sigma <- rbind(data_sigma, out_seq$data_frame)
      row.names(data_sigma) <- NULL
    }
    data_delta <- rbind(data_delta, data_sigma)
    data_sigma = list()
  }
  data_repls <- rbind(data_repls, data_delta)
  data_delta = list()
}
data_repls
#
## weitere Sigma Werte , 0.02, 0.05, 0.1, 0.2

# nochmal drüber nachdenken welche abgelehnt werden sollten, wenn keine Varianzt 
# vorliegt? alle die diese 0.01 entfernt sind oder mehr/weniger? 


# for (delta_mean in c(0, 0.001, 0.01, 0.05, 0.1, 0.2)) {
#   for (sigma in c(0.01, 0.02, 0.05, 0.1, 0.2)) {
#     df <- generate_data(start_iter, mu, delta_mean, sigma)
#     out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", problem = "problem_1", max_repls = start_iter)
#     out_seq$data_frame$sigma <- sigma 
#     out_seq$data_frame$mu <- mu
#     out_seq$data_frame$delta <- delta_mean 
#     out_seq$data_frame$start_iter <- start_iter
#     data_sigma <- rbind(data_sigma, out_seq$data_frame)
#     row.names(data_sigma) <- NULL
#   }
#   data_delta <- rbind(data_delta, data_sigma)
# }
delta_mean = 0
for (sigma in c(0.01, 0.02, 0.05, 0.1, 0.2)) {
  df <- generate_data(start_iter, mu, delta_mean, sigma)
  out_seq <- seq_b_corr_t_test(df = df, baseline = "algo_a", problem = "problem_1", max_repls = start_iter)
  out_seq$data_frame$sigma <- sigma 
  out_seq$data_frame$mu <- mu
  out_seq$data_frame$delta <- delta_mean 
  out_seq$data_frame$start_iter <- start_iter
  data_sigma <- rbind(data_sigma, out_seq$data_frame)
  row.names(data_sigma) <- NULL
}
data_sigma
data_delta
data_repls

rm(data_delta)
rm(data_sigma)
rm(data_repls)
#-------------------------------------------------------------------------------






library(dplyr)
data$replications <- data %>% group_indices(algorithm) 
data 

install.packages("splitstackshape")
library(splitstackshape)
data <- getanID(data, id.vars = "algorithm")
df$id <- df %>% group_indices(IDFAM)
data$replications <- count(data, "algorithm")
count(data, "algorithm")
#-------------------------------------------------------------------------------
measure_accuracy <- rnorm(n = start_iter, mean = c(mu, mu+delta_mean), sd = sigma)
measure_accuracy
algorithm <- rep(c("algo_a", "algo_b"), start_iter)
algorithm <- as.character(algorithm)
algorithm
data <- data.frame(algorithm, measure_accuracy)
data 
data$problem <- "problem_1"
data 
data$replications <- seq(rle(data$algorithm)$lengths)
mode(data$algorithm)
measure_accuracy <- rnorm(n = start_iter, mean = c(mu, mu+delta_mean), sd = sigma)
algorithm <- rep(c("algo_a", "algo_b"), start_iter)
algorithm <- as.character(algorithm)
data <- data.frame(algorithm, measure_accuracy)
data$problem <- "problem_1"
#data$replications <- sequence(rle(data$algorithm)$lengths)
data[1, "replications"] <- 1
data[2, "replications"] <- 1
data[3, "replications"] <- 2
data[4, "replications"] <- 2
data[5, "replications"] <- 3
data[6, "replications"] <- 3
data[7, "replications"] <- 4
data[8, "replications"] <- 4
data[9, "replications"] <- 5
data[10, "replications"] <- 5
return(data)


#-------------------------------------------------------------------------------


# create data frame:
# 30 Problemsets 2 Algorithms
# (for comparisons on one problemset, one just can use one there?)
# first set seed. 
set.seed(123456)
# get numbers from a standard-normal distribution 
rnorm(10, mean = 0.7, sd = 0.1)


x = rnorm(n = 10, mean = 0, sd = 1)
y = rnorm(n = 10, mean = 0, sd = 1)
plot(y ~ x)

# creates strictly positive values between 0 and 1 from the uniform distribution 
runif(n = 5, min = 0, max = 1)

# generating character vectors 
algorithms = rep(letters[1:2], 10)
algorithms

# create data frame 
accuracy = rnorm(n = 20, mean = c(0.7, 0.7), sd =  0.1)
accuracy

simulation <- data.frame(algorithms,
                         accuracy)

# create data frame for 30 problem sets 
simulation <- data.frame()
algorithms = rep(letters[1:2], 10)
list1 = list()
for (i in 1:30) { # Indicate number of iterations with "i"
  list1[[i]] <- rnorm(n = 10, mean = 0.7, sd = 0.1) # Save output in list for each iteration
}
list1


simlist = replicate(n = 30, 
                    expr = data.frame(group = rep(letters[1:2], each = 10),
                                      response = rnorm(n = 20, mean = c(0.7, 0.7), sd =  0.1)),
                    simplify = FALSE)

# der Name für die Problemsets muss noch hinzugefügt werden 
# for (i in i:30) {
#   simlist <- as.data.frame(simlist[[i]]) 
#   simlist$problem <- "problem_"[[i]]
# }

data_30 <- rbind(as.data.frame(simlist[1]), as.data.frame(simlist[2]), as.data.frame(simlist[3]), as.data.frame(simlist[4]), as.data.frame(simlist[5]), 
                 as.data.frame(simlist[6]), as.data.frame(simlist[7]), as.data.frame(simlist[8]), as.data.frame(simlist[9]), as.data.frame(simlist[10]), 
                 as.data.frame(simlist[11]), as.data.frame(simlist[12]), as.data.frame(simlist[13]), as.data.frame(simlist[14]), as.data.frame(simlist[15]),
                 as.data.frame(simlist[16]), as.data.frame(simlist[17]), as.data.frame(simlist[18]), as.data.frame(simlist[19]), as.data.frame(simlist[20]),
                 as.data.frame(simlist[21]), as.data.frame(simlist[22]), as.data.frame(simlist[23]), as.data.frame(simlist[24]), as.data.frame(simlist[25]),
                 as.data.frame(simlist[26]), as.data.frame(simlist[27]), as.data.frame(simlist[28]), as.data.frame(simlist[29]), as.data.frame(simlist[30]))
data_30
#-------------------------------------------------------------------------------
for (i in 1:30) {
  algorithms = rep(letters[1:2], 10)
  accuracy = rnorm(n = 20, mean = c(0.7, 0.7), sd =  0.1)
  simulation[[i]] <- data.frame(algorithms,
                           accuracy)
}

# oder ist es sinnvoller die Differenzen zu simulieren ? 
# nein: wir stellen am Anfang die Annahme, die beiden Algorithmen 
# sind gleichverteilt, haben den selben Mittelwert und sollten
# deswegen auch nicht als unterschiedlich angesehen werden 
# Was ist die Erwartung wenn der Mittelwert gleich ist, aber die 
# Varianzen verschieden ? die Erwartung ist immer das was der volle Test ausgibt 


## Beta Verteilung

x <- seq(0, 1, length = 21)
dbeta(x, 1, 1)
pbeta(x, 1, 1)

## Visualization, including limit cases:
pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length = 1025)
  }
  fx <- cbind(dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b))
  f <- fx; f[fx == Inf] <- 1e100
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
          main = sprintf("[dpq]beta(x, a=%g, b=%g)", a,b))
  abline(0,1,     col="gray", lty=3)
  abline(h = 0:1, col="gray", lty=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}
pl.beta(3,1)

pl.beta(2, 4)
pl.beta(3, 7)
pl.beta(3, 7, asp=1)

# Binomial Verteilung 
require(graphics)
# Compute P(45 < X < 55) for X Binomial(100,0.5)
sum(dbinom(46:54, 100, 0.5))

## Using "log = TRUE" for an extended range :
n <- 2000
k <- seq(0, n, by = 20)
plot (k, dbinom(k, n, pi/10, log = TRUE), type = "l", ylab = "log density",
      main = "dbinom(*, log=TRUE) is better than  log(dbinom(*))")
lines(k, log(dbinom(k, n, pi/10)), col = "red", lwd = 2)
## extreme points are omitted since dbinom gives 0.
mtext("dbinom(k, log=TRUE)", adj = 0)
mtext("extended range", adj = 0, line = -1, font = 4)
mtext("log(dbinom(k))", col = "red", adj = 1)
