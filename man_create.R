## !/usr/bin/Rscript
################################################################################
### .Rd creater 
## Creates .Rd from in-source Roxygen2 docs in .R files
## Created on: 2014-02-10
## Author: Kazuki Yoshida
################################################################################


### Prepare environment
################################################################################

### Load packages
library(devtools)



### Prepare docs
################################################################################

## Create .Rd (clean = TRUE erase non-.Rd files!)
devtools::document("~/Documents/statistics/package_development/tableone/")
## Check
devtools::check_doc("~/Documents/statistics/package_development/tableone/")

