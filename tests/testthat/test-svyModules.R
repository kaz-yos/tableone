################################################################################
### Unit tests for the modules
## Reference: http://adv-r.had.co.nz/Testing.html
## Created on: 2015-07-24
## Author: Kazuki Yoshida
################################################################################

### Structure
## expectations within tests within context

### Prepare environment
################################################################################
library(testthat)
library(tableone)


### Context (1 for each file)
################################################################################
context("Unit tests for the survey-related modules")



### Provide data
################################################################################



### Tests
################################################################################
## A test should group together expectations for one functionality.


test_that("indicator functions work correctly", {

    ## one for counting
    expect_equal(one(c(NA,NA,NA)), c(1,1,1))
    expect_equal(one(c(1,2,3)), c(1,1,1))

    ## missing value indicator
    expect_equal(miss(c(NA,NA,NA)), c(1,1,1))
    expect_equal(miss(c(1,2,3)), c(0,0,0))
    expect_equal(miss(c(1,NA,3)), c(0,1,0))
})


test_that("variable names are transformed to correct formula", {
    
    ## Expectations
    expect_equal(FormulaString(c("a","b","c")), " ~ a + b + c")
    expect_equal(FormulaString("a"), " ~ a")    
})

test_that("survey functions for counting", {
    
    ## Expectations
    expect_equal(NA,  NA)
})
