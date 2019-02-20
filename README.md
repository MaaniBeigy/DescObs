# DescObs
Tools for Versatile Exploration of Data.

`DescObs` stands for **descriptive observation**. 
We do not want just to see, we *observe*! 

# Overview
## Background
There are lots of R packages with statistical functions of particular use cases.
Despite the growing body of these packages and repositories which have been
quite useful for scientists, the redundancy and multiplicity of them
make it sometimes very difficult to choose between them. For instance, various
confidence intervals for a dispersion measure like coefficient of variation or
coefficient of quartile variation (cqv) are not achievable by a 
**single function**. Morover, adding methods and features to those functions 
are not feasible due to noncompliance of some packages with the well-known 
conventions of *functional and/or OO programming*. 

## Aims
The authors' intention is to create a toolbox to facilitate the use of various 
descriptive statistical functions and resources in favor of an easier, 
scientifically recognized standard for implementing R statistical applications. 
We will try to provide **multi-control knob tools**, by means of 
**various options**  and incorporating the most available rigorous methods. 

# Getting started

The `DescObs` is available on GitHub. To install it, use:  

```r
devtools::install_github('MaaniBeigy/DescObs')
```
If you are an ubuntu user, you are going to need `libgsl-dev` package:   
`sudo apt install libgsl-dev`

```r
# load required libraries
require("DescObs", "dplyr", "SciView", "boot", "MBESS")
```

