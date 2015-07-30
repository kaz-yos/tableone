################################################################################
### Unit tests for the modules
## Reference: http://adv-r.had.co.nz/Testing.html
## Created on: 2014-06-01
## Author: Kazuki Yoshida
################################################################################

### Structure
## expectations within tests within context

### Prepare environment
################################################################################
library(testthat)

### Context (1 for each file)
context("Unit tests for the modules")


### Tests for ModuleTestSafe
## Tests for ModuleTestSafe, a wrapper for test functions such as oneway.test and chisq.test

## Create a dataset for a table
dat <- read.table(header = TRUE, text = "
rowVar  colVar
m       s
f       s
m       d
f       d
")

## Increase numbers to avoid "cells are too small" warnings
dat1   <- dat[rep(seq_len(nrow(dat)), 100), ]

## Create a null cross table
xtabs1 <- xtabs( ~ rowVar + colVar, data = dat1)

## Create a non-null cross table by replacement
xtabs2      <- xtabs1
xtabs2[2,2] <- 50

test_that("P-values are returned for appropriate 2x2 xtabs", {
    ## chisq.test
    expect_that(ModuleTestSafe(xtabs1, chisq.test),  equals(chisq.test(xtabs1)$p.value))
    expect_that(ModuleTestSafe(xtabs2, chisq.test),  equals(chisq.test(xtabs2)$p.value))
    ## fisher.test
    expect_that(ModuleTestSafe(xtabs1, fisher.test), equals(fisher.test(xtabs1)$p.value))
    expect_that(ModuleTestSafe(xtabs2, fisher.test), equals(fisher.test(xtabs2)$p.value))
})


## 1xM equal size
xtabs3 <- xtabs1[2,,drop = FALSE]
class(xtabs3) <- c("xtabs", "table")
## 1xM unequal size
xtabs4 <- xtabs2[2,,drop = FALSE]
class(xtabs4) <- c("xtabs", "table")

test_that("P-values should be NA for 1xM xtabs", {
    ## chisq.test
    expect_that(ModuleTestSafe(xtabs3, chisq.test),  equals(NA))
    expect_that(ModuleTestSafe(xtabs4, chisq.test),  equals(NA))
    ## fisher.test
    expect_that(ModuleTestSafe(xtabs3, fisher.test), equals(NA))
    expect_that(ModuleTestSafe(xtabs4, fisher.test), equals(NA))
})


