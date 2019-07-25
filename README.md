# seqbtests: A Sequential Bayesian Approach for Performance Comparison of Machine Learning Algorithms. 

This is a package for sequential bayesian tests. 

## Getting Started

Package installation:

```r
library(devtools)
install_github("RebeccaGroh", "seqbtests")
```

This package provides some Bayesian approaches to compare classifiers in either single or multiple datasets. 
The tests [without any adaptions] in this package are modifications of tests provided by other packages, available at [github](https://github.com/br0rxa/scmamp), .

## Data Frame 


| problem | algorithm | parameter_algorithm| replications | measure_\* |
|:------:|:------:|:------:|:------:|:------:|
| character | character | character | numeric | numeric |
| mandatory | mandatory | optional | mandatory | mandatory |
