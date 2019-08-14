#' @title Main Columns 
#' @description 
#'   Get the list of names of the main columns within the data frame, 
#'   namely 'problem', 'algorithm' and 'replications'. 
#' @param df input data frame
#' @return A vector containing all main column names.
get_main_columns <- function(df) {
    main_columns <- c("problem", "algorithm", "replications")
    return(intersect(names(df), main_columns))
}


#' @title Parameter Columns 
#' @description 
#' Get the list of names for all parameter columns within the data frame. 
#' @param df input data frame
#' @return A vector containing all parameter columns.
get_parameter_columns = function(df) {
    checkmate::assert_data_frame(df)
    return(subset(names(df), startsWith(names(df), "parameter_")))
}


#' @title Measure Columns 
#' @description 
#' Get the list of names for all measure columns within the data frame.
#' @param df input data frame 
#' @return A vector containing all measure columns. 
get_measure_columns <- function(df) {
    checkmate::assert_data_frame(df)
    return(subset(names(df), startsWith(names(df), "measure_")))
}


#' @title Count Main Columns 
#' @description 
#' Count the number of main columns in the data frame. 
#' @param df input data frame 
#' @return A numeric vector displaying the number of main columns. 
get_main_columns_count <- function(df) {
    return(length(get_main_columns(df)))
}


#' @title Count Parameter Columns 
#' @description 
#' Get the number parameter columns in the data frame. 
#' @param df input data frame 
#' @return A numeric vector displaying the number of parameter columns. 
get_parameter_columns_count <- function(df) {
    return(length(get_parameter_columns(df)))
}


#' @title Count Measure Columns 
#' @description 
#' Get the number parameter columns in the data frame. 
#' @param df input data frame 
#' @return A numeric vector displaying the number of columns containing measures.  
get_measure_columns_count <- function(df) {
    return(length(get_measure_columns(df)))
}

#' @title Build Replications
#' @description 
#' When not defined by user, it calls the replications in a complete dataset.  
#' Can be used to build replications during the testing.
#' @param i number of replications used 
#' @param df input data frame 
#' @return Dataframe containing defined number of replications.   
get_replications <- function(i, df) {
    df[df[["replications"]] <= i, ]
}

#' @title Check number of replications 
#' @describtion
#' Check if number of observations in the first and second sample are equal.
#' @param x First sample.
#' @param y Second sample.
#' @return TRUE if both samples are of same length. 
get_replications_count <- function(x, y) {
    checkmate::assert_true(length(x) == length(y))
}


#' @title Paste algorithm and parameter
#' @description 
#' If there is a parameter additional to the algorithm, both can be combined, 
#' when using this function. 
#' @param algorithm Algorithm in data frame.
#' @param parameter_algorithm Algorithm parameter in data frame. 
#' @return New algorithm name, combining algorithm and its parameter. 
paste_algo_pars <- function(algorithm, parameter_algorithm) {
    algorithm <- paste(algorithm, parameter_algorithm, sep = "_")
}


#' @title Data transformation
#' @description Extract a data matrix out of the original dataset. The matrix 
#' contains all observations for each replication in each problemset. 
#' The replications are stored in the rows, while the columns are according to 
#' the problemsets. 
#' @param df Input data frame. 
#' @param algo Name of the algorithm that shall be compared. 
#' @param measure Measure column. 
#' @return Matrix. 
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



#------------------------------------------------------------------------------#

# Ausgabe der Ergebnisse 

mean <- function (x, ...) {
    UseMethod("mean", x)
}

test_to_tex <- function(test){
    UseMethod("test_to_tex")
}

## für die Bayesianischen Tests 
test_to_tex.bayesian <- function(test) {
    
    
}





\begin{center}
\begin{tabular}{ c c c }
cell1 & cell2 & cell3 \\ 
cell4 & cell5 & cell6 \\  
cell7 & cell8 & cell9    
\end{tabular}
\end{center}



## wie sieht der Test von rNPBST aus? 

library(rNPBST)
htest2Tex(cd.test(results))

## und wie sieht die Ausgabe dann tatsächlich aus? 


EBO <- unlist(select(filter(cec17.final, Algorithm == "EBO", Dimension == 10), Result), use.names = F)
jSO <- unlist(select(filter(cec17.final, Algorithm == "jSO", Dimension == 10), Result), use.names = F)


sign.results <- rNPBST::binomialSign.test(cbind(EBO, jSO))
wilcoxon.results <- rNPBST::wilcoxon.test(cbind(EBO, jSO))
wilcoxon.results


jso <- filter(cec17.final, Algorithm == "jSO", Dimension == 10) %>%
    select(Result) %>% unlist()
ebo <- filter(cec17.final, Algorithm == "EBO", Dimension == 10) %>%
    select(Result) %>% unlist()
bst.results <- rNPBST::bayesianSign.test(ebo, jso,
                                         rope.min = -10, rope.max = 10)
bst.results


print.btest = function(x, ...) {print("Hi"); print(x[[1]])}
print.btest
a = list("b")
class(a) = "btest"
a


?print.htest
