################################################################################
### Temporary R script to convert Rd files to Roxygen files
## 
## Created on: 2014-02-09
## Author: Kazuki Yoshida
################################################################################


### Prepare environment
################################################################################

## Load package
library(Rd2roxygen)
## Configure the format
options(roxygen.comment = "##' ")


### Conversion
## Set wd to man dir
setwd("~/Documents/statistics/package_development/tableone/man/")

## Obtain file names
rdFilesToConvert <- dir(pattern = "*.Rd$")

## Create a list of converted data
listConvertedData <- lapply(rdFilesToConvert, parse_file)

## Show title element
lapply(listConvertedData, getElement, "title")

## Check conversion of the first file visually
cat(create_roxygen(listConvertedData[[1]]), sep = "\n")

parse_and_save
Rd2roxygen::
    reformat_code
