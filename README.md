# DescObs
Tools for Versatile Exploration of Data.

`DescObs` stands for **descriptive observation**. 
We do not want just to see, we *observe*! 

# Overview
## Background
There are lots of R packages with statistical functions of particular use cases.
Despite the growing body of these packages and repositories which have been
quite useful for scientists, the redundancy and multiplicity of them
make it sometimes very difficult to choose between them. For instance, the 
abundant methods available for the calculation of confidence intervals of a 
dispersion measure like coefficient of variation (cv) or coefficient of quartile variation (cqv) are not achievable by a **single function**.
## Aims
The authors' intention is to create a toolbox to facilitate the use of various 
descriptive statistical functions and resources in favor of an easier, 
scientifically recognized standard for implementing R statistical applications. 
We will try to provide **multi-control knob tools**, by means of 
**various options** and incorporating the most available rigorous methods. 

# Getting started
If you are an ubuntu user, you are going to need these non-R packages:    
```linux
sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev libgsl-dev   
```   

The `DescObs` is available on github. To install it in `R`, use:  

```r
devtools::install_github('MaaniBeigy/DescObs')  
```
\* Note that this package is still under development.   
\* Pull requests are welcomed.   
\* This package is highly inspired by `dplyr`, `SciView`, `boot`, and `MBESS`.   

```r
# load required libraries  
require("dplyr", "SciView", "boot", "MBESS", "DescObs")  
```

