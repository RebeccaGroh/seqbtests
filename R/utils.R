#' @title Main columns 
#' @description Get the list of names of the main columns within the data frame, 
#'   namely 'problem', 'algorithm' and 'replications'. 
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @return (`list`)\cr A vector containing all main column names.
get_main_columns <- function(df) {
  main_columns <- c("problem", "algorithm", "replications")
  return(intersect(names(df), main_columns))
}


#' @title Parameter columns 
#' @description Get the list of names for all parameter columns within the 
#'     data frame. 
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @return (`list`)\cr A vector containing all parameter columns.
get_parameter_columns = function(df) {
  checkmate::assert_data_frame(df)
  return(subset(names(df), startsWith(names(df), "parameter_")))
}


#' @title Measure columns 
#' @description Get the list of names for all columns containing performance 
#'     measures of the algorithms within the data frame.
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @return (`list`)\cr A vector containing all measure columns. 
get_measure_columns <- function(df) {
  checkmate::assert_data_frame(df)
  return(subset(names(df), startsWith(names(df), "measure_")))
}


#' @title Count main columns 
#' @description Count the number of main columns in the data frame. 
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @return (`list`)\cr A numeric vector displaying the number of main columns. 
get_main_columns_count <- function(df) {
  return(length(get_main_columns(df)))
}


#' @title Count parameter columns 
#' @description Get the number parameter columns in the data frame. 
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @return (`list`)\cr A numeric vector displaying the number of parameter 
#'     columns. 
get_parameter_columns_count <- function(df) {
  return(length(get_parameter_columns(df)))
}


#' @title Count measure columns 
#' @description Get the number parameter columns in the data frame. 
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @return (`list`)\cr A numeric vector displaying the number of columns  
#'     containing performance measures.  
get_measure_columns_count <- function(df) {
  return(length(get_measure_columns(df)))
}

#' @title Build replications
#' @description When not defined by user, it calls the replications in a 
#' complete data frame provided by the user. Can otherwise be used to build 
#' replications during testing procedure.
#' @param i (`double`)\cr Number of replications in data frame.  
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @return (`list`)\cr Data frame containing defined the number of replications.   
get_replications <- function(i, df) {
  df[df[["replications"]] <= i, ]
}

#' @title Check number of replications 
#' @description  Check if number of observations in the first and second 
#'     sample are equal.
#' @param x (`list`)\cr First sample.
#' @param y (`list`)\cr Second sample.
#' @return (`logical`)\cr TRUE if both samples are of same length. 
get_replications_count <- function(x, y) {
  checkmate::assert_true(length(x) == length(y))
}

#' @title Data transformation
#' @description Extract a data matrix out of the original dataset. The matrix 
#'     contains all observations for each replication in each problem set. 
#'     The replications are stored in the rows, while the columns are according  
#'     to the problem sets. 
#' @param df (`list`)\cr Data frame containing the performane measure. 
#' @param algorithm (`character`)\cr Second algorithm. Value in 'algorithm' 
#'     column. If not defined, the baseline is tested against all algorithms 
#'     in the data frame. 
#' @param measure (`character`)\cr Name of the 'measure' column. If not 
#'     defined, the first 'measure' column in the data frame is used. 
#' @return (`matrix`)\cr Matrix. 
data_transformation <- function(df, algo, measure) {
  keep_algo <- subset(df, df[["algorithm"]] == algo)
  data_wide <- tidyr::spread(keep_algo, replications, measure)
  # columns need to be dropped
  drop_cols <- setdiff(colnames(data_wide), unique(df[["replications"]]))
  # columns to keep
  names.use <- names(data_wide)[!(names(data_wide) %in% drop_cols)]
  # subset
  subset_df <- data_wide[, names.use]
  return(subset_df)
}


#' @title Get test results (Bayesian tests)
#' @description This function collects the results of the Bayesian tests. 
#' @param baseline (`character`)\cr First algorithm. Value in 'algorithm'  
#'     column. 
#' @param measure (`character`)\cr Name of the 'measure' column. If not 
#'     defined, the first 'measure' column in the data frame is used. 
#' @param method (`character`)\cr Bayesian test. 
#' @param data (`list`)\cr Data frame containing the posterior probabilities. 
#' @param extra (`any`)\cr Any extra arguments needed to build the plots. 
#' @return (`list`)\cr List. 
get_results <- function(baseline, method, measure, data = NULL, extra = NULL) {
  output <- list(baseline = baseline, method = method, measure = measure, 
                 data_frame = data, extra = extra)
  class(output) <- "b_test"
  return(output)
}


#' @title Get test results (Frequentist tests)
#' @description This function collects the results of the Frequentist tests. 
#' @param baseline (`character`)\cr First algorithm. Value in 'algorithm'  
#'     column. 
#' @param measure (`character`)\cr Name of the 'measure' column. If not 
#'     defined, the first 'measure' column in the data frame is used. 
#' @param method (`character`)\cr Bayesian test. 
#' @param data (`list`)\cr Data frame containing the posterior probabilities. 
#' @param matrix (`matrix`)\cr A matrix with all the pair wise differences of 
#'     average rankings.
#' @return (`list`)\cr List. 
get_results_htest <- function(baseline = NULL, method, measure, data = NULL, 
                              matrix = NULL) {
  output <- list(baseline = baseline, method = method, measure = measure, 
                 data_frame = data, matrix = matrix)
  return(output)
}

#' @title Get test results (for data frame)
#' @description This function collects the part of the results shown in the data 
#'     frame. 
#' @param k (`character`)\cr Algorithm tested against the baseline.    
#' @param posterior (`character`)\cr Call for the posterior probabilities of the 
#'     Bayesian tests.  
#' @return (`list`)\cr List. 
get_data_frame <- function(k, posterior) {
  result <- data.frame()
  result[k, "algorithm"] <- k
  result[k, "left"] <- posterior[1]
  result[k, "rope"] <- posterior[2]
  result[k, "right"] <- posterior[3]
  return(result)
}


#' @title Get test results (for sequential tests)
#' @description This function collects the part of the results shown in the data 
#'     frame. 
#' @param k (`character`)\cr Algorithm tested against the baseline.    
#' @param posterior (`character`)\cr Call for the posterior probabilities of the 
#'     Bayesian tests. 
#' @param repls (`double`)\cr Number of replications used until a decision is made. 
#' @return (`list`)\cr List. 
get_data_frame_seq <- function(k, posterior, repls) {
  result <- data.frame()
  result[k, "algorithm"] <- k
  result[k, "left"] <- posterior[1]
  result[k, "rope"] <- posterior[2]
  result[k, "right"] <- posterior[3]
  result[k, "repls"] <- repls
  return(result)
}

#' @title Get test results (for frequentist tests)
#' @description This function collects the part of the results shown in the data 
#'     frame. 
#' @param k (`character`)\cr Algorithm tested against the baseline.    
#' @param p_value (`double`)\cr The p-value for the test.
#' @param test (`character`)\cr Hypothesis test. 
#' @param statistic (`double`)\cr Value of the statistic used in the test.
#' @return (`list`)\cr List. 
get_data_frame_htest <- function(k, p_value, test, statistic) {
  result <- data.frame()
  result[k, "algorithm"] <- k
  result[k, "p_value"] <- p_value
  result[k, "test"] <- test
  result[k, "statistic"] <- statistic
  return(result)
}

#' @title Get test results (for frequentist tests)
#' @description This function collects the part of the results shown in the data 
#'     frame. Not depending on algorithms.  
#' @param p_value (`double`)\cr The p-value for the test.
#' @param test (`character`)\cr Hypothesis test. 
#' @param statistic (`double`)\cr Value of the statistic used in the test.
#' @return (`list`)\cr List. 
get_data_frame_htest_small <- function(p_value, test, statistic) {
  result <- data.frame()
  result[1, "p_value"] <- p_value
  result[1, "test"] <- test
  result[1, "statistic"] <- statistic
  return(result)
}

#' @title Get extras from tests (for tests from scmamp)
#' @description This function collects additional information from the test 
#'     summed up as "extras". 
#' @param x (`list`)\cr Test results. 
#' @return (`list`)\cr List. 
get_extras_scmamp <- function(x, ...) {
  extras <- list(x$additional, x$approximate, x$parameters, x$posterior,
                 x$additional$pposterior, x$additional$qposterior, 
                 x$additional$posterior.df, x$additional$posterior.mean, 
                 x$additional$posterior.sd)
  return(extras)
}

#' @title Get relevant rows in data frame (for sequential tests)
#' @description This function drops the non-final results during the sequential 
#'     testing process. 
#' @param result (`list`)\cr Data frame with all the results within in the 
#'     tests. 
#' @return (`list`)\cr Data frame with just the relevant rows. 
get_rows <- function(result) {
  sorted <- result[order(result$algorithm, -result$repls),]
  sorted <- sorted[!duplicated(sorted$algorithm),]
  row.names(sorted) <- NULL
  return(sorted)
}

#' @title Get probabilities 
#' @description This function defines the probabilities resulting from the
#'     Bayesian tests for each algorithm that is tested against the baseline. 
#' @param result (`list`)\cr Data frame with all the results within in the 
#'     tests. 
#' @param compare (`character`)\cr Defines if one algorithm needs to perform 
#'     better ({\code{better}}) for decisions based on the posterior 
#'     distribution or whether it is sufficient to perform not worse 
#'     ({\code{equal}}). 
#' @param prob (`double`)\cr Threshold probability that decision rely on. 
#'     Default is 0.95.
#' @return (`list`)\cr Column of result data frame. 
get_probabilities <- function(result, compare, prob) {
  for (i in 1:nrow(result)) {
    if (compare == "better") {
      threshold <- result[i, "left"]
      threshold_vv <- result[i, "right"]
      decision_base <- ">>"
      decision_algo <- "<<"
    }
    if (compare == "equal") {
      threshold <- result[i, "left"] + result[i, "rope"]
      threshold_vv <- result[i, "right"] + result[i, "rope"]
      decision_base <- ">="
      decision_algo <- "<="
    }
    if (threshold > prob) {
      result$probabilities[i] <- paste("P(Baseline", decision_algo,  
        "Algorithm) >", prob, sep = " ")
    } else if (threshold_vv > prob) {
      result$probabilities[i] <- paste("P(Baseline", decision_base, 
        "Algorithm) >", prob, sep = " ")
    } else if (result[i, "rope"] > prob) {
      result$probabilities[i] <- paste("P(Baseline = Algorithm) >", prob)
    } else {
      result$probabilities[i] <- "no decision"
    }
  }
  return(result)
}


#' @title Get threshold 
#' @description This function defines the threshold for making any decisions 
#'     depending on the calculated posterior probabilities. 
#' @param posterior (`character`)\cr Call for the posterior probabilities of the 
#'     Bayesian tests. 
#' @param compare (`character`)\cr Defines if one algorithm needs to perform 
#'     better ({\code{better}}) for decisions based on the posterior 
#'     distribution or whether it is sufficient to perform not worse 
#'     ({\code{equal}}). 
#' @return (`list`)\cr List. 
get_threshold <- function(posterior, compare) {
  thresholds <- list()
  if (compare == "better") { 
    thresholds[1] <- posterior[1]
    thresholds[2] <- posterior[2]
    thresholds[3] <- posterior[3]
  } else if (compare == "equal") {
    thresholds[1] <- posterior[2] + 
      posterior[1]
    thresholds[2] <- posterior[2]
    thresholds[3] <- posterior[2] + 
      posterior[3]
  }
  return(thresholds)
}
