# DescObs ![DescObs](eye.png)
Tools for Versatile Exploration of Data.   

`DescObs` stands for **descriptive observation**.    
We do not just see, we *observe*!   

# Overview   

### Background    

There are lots of R packages with statistical functions of particular use cases.
Despite the growing body of these packages and repositories which have been
quite useful for scientists, the redundancy and multiplicity of them
make it sometimes very difficult to choose between them. For instance, the 
abundant methods available for the calculation of **confidence intervals** of a 
dispersion measure like coefficient of variation (`cv`) or coefficient of quartile variation (`cqv`) have not yet achieved.    

### Aims    

The authors' intention is to create a toolbox to facilitate the use of various 
descriptive statistical functions and resources in favor of an easier, 
scientifically recognized standard for implementing R statistical applications. 
We will try to provide **multi-control knob tools**, by means of 
**various options** and incorporating the most available rigorous methods.   

### Convention       

We are bound by the high standards of functional programming (FP) and object-oriented programming (OOP). The majority of tools provided by `DescObs` are developed as both FP tools and [R6](https://github.com/r-lib/R6) classes, for sake of versatility, portability and efficiency.

# Getting started    

If you are an ubuntu user, you are going to need these non-R packages:    
```linux
sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev libgsl-dev   
```   

The `DescObs` is available on github. To install it in `R`, use:    

```r
devtools::install_github('MaaniBeigy/DescObs')  
```
\* Note that this package is still in-development. Currently, these tools are
available:    


+--------------------+---------------+-------------------------------------------------------------+
| name               | `is.R6()`     | Description        |
+====================+===============+=============================================================+
| `CoefVar`          | TRUE          | Coefficient of Variation (cv)                           |
+--------------------+---------------+-------------------------------------------------------------+
| `CoefQuartVar`     | TRUE          | Coefficient of Quartile Variation (cqv)                 |
+--------------------+---------------+-------------------------------------------------------------+
| `CoefVarCI`        | TRUE          | Confidence Intervals for cv                             |
+--------------------+---------------+-------------------------------------------------------------+
| `CoefQuartVarCI`   | TRUE          | Confidence Intervals for cqv                            |
+--------------------+---------------+-------------------------------------------------------------+
| `SampleQuantiles`  | TRUE          | Sample Quantiles                                        |
+--------------------+---------------+-------------------------------------------------------------+
| `cv`               | FALSE         | Coefficient of Variation                                |
+--------------------+---------------+-------------------------------------------------------------+
| `cqv`              | FALSE         | Coefficient of Quartile Variation                       |
+--------------------+---------------+-------------------------------------------------------------+
| `BootCoefVar`      | TRUE          | Bootstrap Resampling for cv                             |
+--------------------+---------------+-------------------------------------------------------------+
| `BootCoefQuartVar` | TRUE          | Bootstrap Resampling for cqv                            |
+--------------------+---------------+-------------------------------------------------------------+
| `rm.versatile`     | FALSE         | Versatile Function for Removing Objects                 |
+--------------------+---------------+-------------------------------------------------------------+ 

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
results <- CoefVarCI$new(x, digits = 3)$all_ci()  # R6 class
# or alternatively: 
results <- cv(x, digits = 3, method = "all")  # functional programming
```
The `results` will be:    

|                     |    est|  lower|   upper|description                                        |
|:--------------------|------:|------:|-------:|:--------------------------------------------------|
|kelley               | 57.774| 41.287|  97.894|cv with Kelley 95% CI                              |
|mckay                | 57.774| 41.441| 108.483|cv with McKay 95% CI                               |
|miller               | 57.774| 34.053|  81.495|cv with Miller 95% CI                              |
|vangel               | 57.774| 41.264| 105.426|cv with Vangel 95% CI                              |
|mahmoudvand_hassani  | 57.774| 43.476|  82.857|cv with Mahmoudvand-Hassani 95% CI                 |
|equal_tailed         | 57.774| 43.937|  84.383|cv with Equal-Tailed 95% CI                        |
|shortest_length      | 57.774| 42.015|  81.013|cv with Shortest-Length 95% CI                     |
|normal_approximation | 57.774| 44.533|  85.272|cv with Normal Approximation 95% CI                |
|norm                 | 57.774| 38.799|  78.937|cv with Normal Approximation Bootstrap 95% CI      |
|basic                | 57.774| 35.055|  78.167|cv with Basic Bootstrap 95% CI                     |
|perc                 | 57.774| 38.879|  79.174|cv with Bootstrap Percentile 95% CI                |
|bca                  | 57.774| 40.807|  82.297|cv with Adjusted Bootstrap Percentile (BCa) 95% CI |

Next, we want to find **all** of the available *confidence intervals* for the `cqv` of variable *x*:  

```r
results <- CoefQuartVarCI$new(x, digits = 3)$all_ci()  # R6 class
# or alternatively:
results <- cqv(x, , digits = 3, method = "all")  # functional programming

```
The `results` will be:   

|        |    est|  lower|  upper|description                                     |
|:-------|------:|------:|------:|:-----------------------------------------------|
|bonett  | 45.625| 24.785| 77.329|cqv with Bonett CI                              |
|norm    | 45.625| 19.957| 70.840|cqv with normal approximation CI                |
|basic   | 45.625| 18.992| 73.917|cqv with basic bootstrap CI                     |
|percent | 45.625| 17.122| 68.683|cqv with bootstrap percentile CI                |
|bca     | 45.625| 24.273| 83.264|cqv with adjusted bootstrap percentile (BCa) CI |

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
