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

## Three-group dataset with matching weight (Li & Greene 2013)
datMw <- structure(list(E = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), C = c(1L, 
1L, 2L, 2L, 3L, 3L, 4L, 4L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 1L, 
1L, 2L, 2L, 3L, 3L, 4L, 4L), Y = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 
1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1), C1 = c(0, 0, 0, 
0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1
), C2 = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 
0, 1, 1, 0, 0, 1, 1), n = c(252L, 28L, 105L, 45L, 12L, 28L, 1L, 
9L, 72L, 8L, 63L, 27L, 24L, 56L, 1L, 9L, 36L, 4L, 42L, 18L, 24L, 
56L, 8L, 72L), Mw = c(0.1429, 0.1429, 0.4, 0.4, 1, 1, 1, 1, 0.5, 
0.5, 0.6667, 0.6667, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 0.5, 0.5, 0.125, 
0.125)), class = "data.frame", .Names = c("E", "C", "Y", "C1", 
"C2", "n", "Mw"), row.names = c(NA, -24L))

datMw <- datMw[rep(seq_along(datMw$n), datMw$n),]


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
