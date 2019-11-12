[![Build Status](https://travis-ci.org/RebeccaGroh/seqbtests.svg?branch=master)](https://travis-ci.org/RebeccaGroh/seqbtests)

# seqbtests: A Sequential Bayesian Approach for Performance Comparison of Machine Learning Algorithms. 

This is a package for sequential Bayesian tests. 

## Getting Started

Package installation:

```r
if (!require("devtools")) {
  install.packages("devtools")
}

install_github("RebeccaGroh/seqbtests")
```

## Documentation

For an overview of the package functionality the package includes a vignette. 

library("seqbtests")
browseVignettes("seqbtests")

Once you install the package, you can access individual man pages by a call to, e.g., `?b_corr_t_test`.


## Package 

This package provides some Bayesian approaches to compare classifiers in either single or multiple datasets. 
The tests [without any adaptions] in this package are modifications of tests provided by other packages, available at [github](https://github.com/br0rxa/scmamp), [github](https://github.com/JacintoCC/rNPBST).

## Data Frame 


| problem | algorithm | replication | measure_\* |
|:------:|:------:|:------:|:------:|
| character | character |  integer | numeric |
| mandatory | mandatory |  mandatory | mandatory |


 