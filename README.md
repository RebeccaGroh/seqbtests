# seqbtests: A Sequential Bayesian Approach for Performance Comparison of Machine Learning Algorithms. 

This is a package for sequential bayesian tests. 

## Getting Started

Package installation:

```r
if (!require("devtools")) {
  install.packages("devtools")
}

install_github("RebeccaGroh/seqbtests")
```

This package provides some Bayesian approaches to compare classifiers in either single or multiple datasets. 
The tests [without any adaptions] in this package are modifications of tests provided by other packages, available at [github](https://github.com/br0rxa/scmamp), [github](https://github.com/JacintoCC/rNPBST).

## Data Frame 


| problem | algorithm | replication | measure_\* |
|:------:|:------:|:------:|:------:|
| character | character |  integer | numeric |
| mandatory | mandatory |  mandatory | mandatory |


 