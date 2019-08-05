---
title: "toolmaRk - R package for same-source toolmark matching"
author: "Jeremy Hadler, Heike Hofmann"
date: "August 05, 2019"
output: 
  html_document:
    keep_md: true
---



Were two striation marks made by the same tool or by different tools? The R package toolmaRk provides a range of statistical tests for that.

[![CRAN Status](http://www.r-pkg.org/badges/version/toolmaRk)](https://cran.r-project.org/package=toolmaRk) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/toolmaRk)](http://www.r-pkg.org/pkg/toolmaRk) 

[![Travis-CI Build Status](https://travis-ci.org/jrhadler/toolmaRk.svg?branch=master)](https://travis-ci.org/jrhadler/toolmaRk)


# Installation

`toolmaRk` is available from CRAN:


```r
install.packages("toolmaRk")
```


The development version of `toolmaRk` is available from Github:


```r
# install.packages("devtools")
devtools::install_github("jrhadler/toolmaRk", build_vignettes = TRUE)
```

# Getting Started

Load the library


```r
library(toolmaRk)
```

```
## Loading required package: plyr
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Loading required package: reshape2
```

Load a dataset ...

