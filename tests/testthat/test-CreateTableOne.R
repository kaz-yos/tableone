################################################################################
### Unit tests for the CreateTableOne function
## Reference: http://adv-r.had.co.nz/Testing.html
## Created on: 2014-06-01
## Author: Kazuki Yoshida
################################################################################

### Structure
## expectations within tests within context

### Prepare environment
################################################################################
library(testthat)
library(tableone)

### Context (1 for each file)
context("Unit tests for the CreateTableOne function")


### Tests for
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
## m's only dataset but level f is retained
dat2 <- dat1[dat1$rowVar == "m", ]
## m's only dataset without level f
dat3 <- dat2
dat3$rowVar <- factor(dat3$rowVar)
## m's only dataset without level f. Strata imbalance
dat4 <- dat3[c(1:100, seq(101,200,2)), ]


test_that("P-values are returned for appropriate 2x2 xtabs, even with an empty factor level.", {

    ## Create tables
    tab1 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat1)
    tab2 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat2)

    ## Create corresponding xtabs
    xtabs1 <- xtabs(~ rowVar + colVar, dat1)
    xtabs2 <- xtabs(~ rowVar + colVar, dat2)

    ## chisq.test
    expect_that(attributes(tab1)$pValues["rowVar","pApprox"], equals(chisq.test(xtabs1)$p.value))
    expect_that(attributes(tab2)$pValues["rowVar","pApprox"], equals(chisq.test(xtabs2)$p.value))
    ## fisher.test
    expect_that(attributes(tab1)$pValues["rowVar","pExact"],  equals(fisher.test(xtabs1)$p.value))
    expect_that(attributes(tab2)$pValues["rowVar","pExact"],  equals(fisher.test(xtabs2)$p.value))
})


test_that("P-values should be NA for 1xM xtabs", {

    ## Create a table
    tab3 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat3)
    tab4 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat4)

    ## Create corresponding xtabs
    xtabs3 <- xtabs(~ rowVar + colVar, dat3)
    xtabs4 <- xtabs(~ rowVar + colVar, dat4)

    ## chisq.test
    expect_that(attributes(tab3)$pValues["rowVar","pApprox"], equals(NA))
    expect_that(attributes(tab4)$pValues["rowVar","pApprox"], equals(NA))
    ## fisher.test
    expect_that(attributes(tab3)$pValues["rowVar","pExact"],  equals(NA))
    expect_that(attributes(tab4)$pValues["rowVar","pExact"],  equals(NA))
})


