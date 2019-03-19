# DescObs ![DescObs](eye.png)
Tools for Versatile Exploration of Data.   

`DescObs` stands for **descriptive observation**.    
We do not just see, we *observe*!   

# Overview   

## Background    

There are lots of R packages with statistical functions of particular use cases.
Despite the growing body of these packages and repositories which have been
quite useful for scientists, the redundancy and multiplicity of them
make it sometimes very difficult to choose between them. For instance, the 
abundant methods available for the calculation of confidence intervals of a 
dispersion measure like coefficient of variation (cv) or coefficient of quartile variation (cqv) have not yet achieved.    

## Aims    

The authors' intention is to create a toolbox to facilitate the use of various 
descriptive statistical functions and resources in favor of an easier, 
scientifically recognized standard for implementing R statistical applications. 
We will try to provide **multi-control knob tools**, by means of 
**various options** and incorporating the most available rigorous methods.   

### Convention       

We are bound by the high standards of functional programming (FP) and object-oriented programming (OOP). The majority of tools provided by `DescObs` are developed as both FP functions and R6 classes, for sake of versatility, portability and efficiency.

# Getting started    

If you are an ubuntu user, you are going to need these non-R packages:    
```linux
sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev libgsl-dev   
```   

The `DescObs` is available on github. To install it in `R`, use:    

```r
devtools::install_github('MaaniBeigy/DescObs')  
```
\* Note that this package is still in-development.   
`cv`, `cqv` and `rm.versatile` are the functions written in FP framework.   
\* Pull request welcome.     
\* This package is inspired by `dplyr`, `R6`, `SciView`, `boot`, and `MBESS`.     
## Dependencies     

To load the required libraries, run:    

```r
# define required libraries
required.packages <- c("dplyr", "SciViews", "boot", "MBESS", "R6")
# function of installing the uninstalled required packages
if (length(setdiff(required.packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(required.packages, rownames(installed.packages())))
} 
# add `DescObs` to the required.packages
required.packages <- c(required.packages, "DescObs")
# loading the required packages
lapply(required.packages, require, character.only = TRUE)
```
## Examples
Here, we want to observe **all** available *confidence intervals* for the `cv` 
of variable *x*:     
```r
x <- c(
    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
)
cv(x, na.rm = TRUE, digits = 3, method = "all", correction = FALSE, alpha = 0.05)
## $method
## [1] "All methods"
## 
## $statistics
##                         est  lower   upper
## kelley               57.774 41.467  98.510
## mckay                57.774 41.441 108.482
## miller               57.774 34.053  81.494
## vangel               57.774 41.264 105.424
## mahmoudvand_hassani  57.774 43.476  82.857
## equal_tailed         57.774 43.936  84.382
## shortest_length      57.774 42.014  81.012
## normal_approximation 57.774 44.533  85.272
## norm                 57.774 39.382  78.482
## basic                57.774 37.679  77.757
##                                                        description
## kelley                                       cv with Kelley 95% CI
## mckay                                         cv with McKay 95% CI
## miller                                       cv with Miller 95% CI
## vangel                                       cv with Vangel 95% CI
## mahmoudvand_hassani             cv with Mahmoudvand-Hassani 95% CI
## equal_tailed                           cv with Equal-Tailed 95% CI
## shortest_length                     cv with Shortest-Length 95% CI
## normal_approximation           cv with Normal Approximation 95% CI
## norm                 cv with Normal Approximation Bootstrap 95% CI
## basic                               cv with Basic Bootstrap 95% CI
```
Next, we want to find **all** of the available *confidence intervals* for the `cqv` of variable *x*:  
```r
cqv(x, na.rm = TRUE, digits = 3, method = "all")
## $method
## [1] "All Bootstrap methods"
## 
## $statistics
##           est lower upper                                         description
## bonett  45.62 24.78 77.33                              cqv with Bonett 95% CI
## norm    45.62 20.59 69.47                cqv with normal approximation 95% CI
## basic   45.62 20.81 73.91                     cqv with basic bootstrap 95% CI
## percent 45.62 17.33 70.43                cqv with bootstrap percentile 95% CI
## bca     45.62 23.00 78.56 cqv with adjusted bootstrap percentile (BCa) 95% CI
```
## Documentation    
Download the [DescObs_0.1.0.tar.gz](
https://github.com/MaaniBeigy/DescObs/raw/master/DescObs_0.1.0.tar.gz). Install the source package `DescObs` from the Packages >> Install >> Package Archive 
File (.tar.gz) >> Browse >> DescObs_0.1.0.tar.gz. Or run an installation code like:     
```r
install.packages("~/DescObs_0.1.0.tar.gz", repos = NULL, type = "source")
```
Then, browse for vignettes:

```r
browseVignettes("DescObs")
```
Then, select `html` to show the vignette.
