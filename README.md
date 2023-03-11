# populR

Population Downscaling Using Areal Interpolation - A Comparative Analysis
author:  Marios Batsaris 

## Introduction

Areal Interpolation may be defined as the process of transforming data reported
over a set of spatial units (source) to another (target). Its application to population 
data has attracted considerable attention during the last few decades. A massive amount 
of methods have been reported in the scientific literature. Most of them focus 
on the improvement of the accuracy by using more sophisticated techniques rather 
than developing standardized methods. As a result, only a few implementation 
tools exists within the R community.

## Download and install 

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("mbatsaris/populR")
```
## Example

This is a basic example which shows you how to solve a common problem:
``` r
library(populR)
library(areal)
library(sf)
# load data
data('src')
data('trg')
source <- src
target <- trg

# populR - awi
awi <- pp_estimate(target = target, source = source, spop = pop, sid = sid, 
                   method = awi)

# populR - vwi
vwi <- pp_estimate(target = target, source = source, spop = pop, sid = sid, 
                   volume = floors, method = vwi)

# areal - sum weights
aws <- aw_interpolate(target, tid = tid, source = source, sid = 'sid', 
                      weight = 'sum', output = 'sf', extensive = 'pop')

# areal - total weights
awt <- aw_interpolate(target, tid = tid, source = source, sid = 'sid', 
                      weight = 'total', output = 'sf', extensive = 'pop')

# sf - total weights
sf <- st_interpolate_aw(source['pop'], target, extensive = TRUE)


# sum initial values
sum(source$pop)

# populR - awi
sum(awi$pp_est)

# populR - vwi
sum(vwi$pp_est)

# areal - awt
sum(awt$pop)

# areal - aws
sum(aws$pop)

# sf
sum(sf$pop)

```
