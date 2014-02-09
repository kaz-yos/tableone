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


### Direct file conversion
## Rd2roxygen
## Convert all the Rd files of a package to roxygen comments
## Description:
##      This function takes a package root directory, parses all its Rd
##      files under the man directory and update the corresponding R
##      source code by inserting roxygen documentation in to the R
##      scripts.
## Usage:
##      Rd2roxygen(pkg, nomatch, usage = FALSE)
## Note:
##      ESS users may use ‘options(roxygen.comment = "##' ")’ to ensure
##      the generated roxygen comments begin with ‘"##' "’, which is the
##      default setting in Emacs/ESS.
##      Re-run this function on a package will remove the previous roxygen
##      comments before functions in R scripts.
Rd2roxygen(pkg = "~/Documents/statistics/package_development/tableone/",
           usage = TRUE)
## ##------ Sun Feb  9 05:49:44 2014 ------##
## parsed: CreateCatTable.Rd
## looking for the object 'CreateCatTable' in:
##   CreateCatTable.R: line 4
## ~/Documents/statistics/package_development/tableone//R/CreateCatTable.R updated


## ##------ Sun Feb  9 05:49:44 2014 ------##
## parsed: CreateContTable.Rd
## looking for the object 'CreateContTable' in:
##   CreateContTable.R: line 4
## ~/Documents/statistics/package_development/tableone//R/CreateContTable.R updated


## ##------ Sun Feb  9 05:49:44 2014 ------##
## parsed: print.CatTable.Rd
## looking for the object 'print.CatTable' in:
##   print.CatTable.R: line 2
## ~/Documents/statistics/package_development/tableone//R/print.CatTable.R updated


## ##------ Sun Feb  9 05:49:44 2014 ------##
## parsed: print.ContTable.Rd
## looking for the object 'print.ContTable' in:
##   print.ContTable.R: line 2
## ~/Documents/statistics/package_development/tableone//R/print.ContTable.R updated


## ##------ Sun Feb  9 05:49:44 2014 ------##
## parsed: ShowRegTable.Rd
## looking for the object 'ShowRegTable' in:
##   ShowRegTable.R: line 1
## ~/Documents/statistics/package_development/tableone//R/ShowRegTable.R updated


## ##------ Sun Feb  9 05:49:44 2014 ------##
## parsed: summary.CatTable.Rd
## looking for the object 'summary.CatTable' in:
##   summary.CatTable.R: line 1
## ~/Documents/statistics/package_development/tableone//R/summary.CatTable.R updated


## ##------ Sun Feb  9 05:49:44 2014 ------##
## parsed: summary.ContTable.Rd
## looking for the object 'summary.ContTable' in:
##   summary.ContTable.R: line 1
## ~/Documents/statistics/package_development/tableone//R/summary.ContTable.R updated


## ##------ Sun Feb  9 05:49:44 2014 ------##
## parsed: tableone_dummy-package.Rd
## unmatched object 'tableone-package' written into ~/Documents/statistics/package_development/tableone//R/tableone-package.R
