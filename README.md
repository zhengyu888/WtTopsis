
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WtTopsis

<!-- badges: start -->
<!-- badges: end -->

The goal of WtTopsis is to provide a Weighted TOPSIS method for
multiple-criteria decision making (MCDM).

## Installation

You can install WtTopsis like so:

``` r
devtools::install_github("zhengyu888/WtTopsis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(WtTopsis)
mth<-c("ZH","ZJ","QJ","ZH","FU","ZH","QJ","FU","FU","ZH")
zmth<-c(NA,NA,NA,NA,"CZ",NA,NA,"DS","CZ",NA)
xbest<-c(NA,4,NA,NA,NA,NA,NA,NA,NA,NA)
qup<-c(NA,NA,5,NA,NA,NA,5,NA,NA,NA)
qlow<-c(NA,NA,3,NA,NA,NA,3,NA,NA,NA)
maxdata<-data2max(sampleData,mth,zmth,xbest,qlow,qup)
normaldata<-DataNormal(maxdata)
weightdata<-GetWeight(normaldata)
Cvalue1<-WtTopsisN(normaldata)
Cvalue2<-WtTopsis(sampleData,mth,zmth,xbest,qlow,qup)
## basic example code
```
