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

### Context (1 for each file)
context("Unit tests for the CreateTableOne function")


### Load data

library(survival)
data(pbc)

## Make categorical variables factors
varsToFactor <- c("status","trt","ascites","hepato","spiders","edema","stage")
pbc[varsToFactor] <- lapply(pbc[varsToFactor], factor)

## Create a variable list
vars <- c("time","status","age","sex","ascites","hepato",
          "spiders","edema","bili","chol","albumin",
          "copper","alk.phos","ast","trig","platelet",
          "protime","stage")
varsContOnly <- c("time","age","protime")
varsCatOnly  <- c("status","trt","sex")

### Tests for data checkers

test_that("abnormal data are correctly detected", {

    ## Expectations
    ## Use regular expressions

    ## Error in ModuleStopIfNoVarsLeft(vars) : No valid variables.
    ## In addition: Warning message:
    ## In ModuleReturnVarsExist(vars, data) :
    ##   The data frame does not have: non-existent  Dropped
    expect_error(expect_warning(CreateTableOne(vars = "non-existent", data = pbc),
                                "The data frame does not have: non-existent  Dropped"),
                 "No valid variables.")

    ## Error in ModuleStopIfNotDataFrame(data) :
    ##   The data argument needs to be a data frame (no quote).
    expect_error(CreateTableOne(data = "not a data frame"),
                 "The data argument needs to be a data frame")

    ## Error in ModuleReturnStrata(strata, data) :
    ##   None of the stratifying variables are present in the data frame.
    ## In addition: Warning message:
    ## In ModuleReturnVarsExist(strata, data) :
    ##   The data frame does not have: non-existent  Dropped
    expect_error(expect_warning(CreateTableOne(vars = vars, strata = c("non-existent"), data = pbc),
                                "The data frame does not have: non-existent  Dropped"),
                 "None of the stratifying variables are present in the data frame")

})


### Tests for ModuleTestSafe, a wrapper for test functions such as oneway.test and chisq.test

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
    expect_that(attributes(tab1$CatTable)$pValues["rowVar","pApprox"],
                equals(chisq.test(xtabs1)$p.value))
    expect_warning(expect_that(attributes(tab2$CatTable)$pValues["rowVar","pApprox"],
                               equals(chisq.test(xtabs2)$p.value)),
                   "Chi-squared approximation may be incorrect")
    ## fisher.test
    expect_that(attributes(tab1$CatTable)$pValues["rowVar","pExact"],
                equals(fisher.test(xtabs1)$p.value))
    expect_that(attributes(tab2$CatTable)$pValues["rowVar","pExact"],
                equals(fisher.test(xtabs2)$p.value))
})


test_that("P-values should be NA for 1xM xtabs", {

    ## Create a table
    tab3 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat3)
    tab4 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat4)

    ## Create corresponding xtabs
    xtabs3 <- xtabs(~ rowVar + colVar, dat3)
    xtabs4 <- xtabs(~ rowVar + colVar, dat4)

    ## chisq.test
    expect_that(attributes(tab3$CatTable)$pValues["rowVar","pApprox"],
                equals(NA))
    expect_that(attributes(tab4$CatTable)$pValues["rowVar","pApprox"],
                equals(NA))
    ## fisher.test
    expect_that(attributes(tab3$CatTable)$pValues["rowVar","pExact"],
                equals(NA))
    expect_that(attributes(tab4$CatTable)$pValues["rowVar","pExact"],
                equals(NA))
})



### Regression tests
################################################################################

## Create a table to test
pbcOverall  <- CreateTableOne(vars = vars, data = pbc)
pbcInclNa   <- CreateTableOne(vars = vars, data = pbc, includeNA = TRUE)
pbcByTrt    <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc)
pbcByTrtSex <- CreateTableOne(vars = vars, strata = c("trt","sex"), data = pbc)
pbcContOnlyByTrtSex <- CreateTableOne(vars = varsContOnly, strata = c("trt","sex"), data = pbc)
pbcCatOnlyByTrtSex  <- CreateTableOne(vars = varsCatOnly, strata = c("trt","sex"), data = pbc)


## Specify variables for special handling
nonnormalVars <- c("bili","chol","copper","alk.phos","trig")
exactVars <- c("status","stage")


test_that("TableOne objects are always returned", {

    expect_equal(class(pbcOverall),          "TableOne")
    expect_equal(class(pbcInclNa),           "TableOne")
    expect_equal(class(pbcByTrt),            "TableOne")
    expect_equal(class(pbcByTrtSex),         "TableOne")
    expect_equal(class(pbcContOnlyByTrtSex), "TableOne")
    expect_equal(class(pbcCatOnlyByTrtSex),  "TableOne")

})


test_that("printing of a TableOne object does not regress", {

    ## Expectations
    expect_equal_to_reference(print(pbcByTrt, printToggle = TRUE),
                              "ref-TableOne_defaultPrint")

    expect_equal_to_reference(print(pbcOverall, printToggle = TRUE),
                              "ref-TableOne_overallPrint")

    expect_equal_to_reference(print(pbcInclNa, printToggle = TRUE),
                              "ref-TableOne_IncludeNA")

    expect_equal_to_reference(print(pbcByTrtSex, printToggle = TRUE),
                              "ref-TableOne_2StrataVars")

    expect_equal_to_reference(print(pbcByTrt, catDigits = 3, contDigits = 4, pDigits = 5, printToggle = TRUE),
                              "ref-TableOne_digits")

    expect_equal_to_reference(print(pbcByTrt, test = FALSE, printToggle = TRUE),
                              "ref-TableOne_noTests")

    expect_equal_to_reference(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, printToggle = TRUE),
                              "ref-TableOne_nonnormal_exact")

    expect_equal_to_reference(print(pbcByTrt, nonnormal = nonnormalVars, minMax = TRUE, printToggle = TRUE),
                              "ref-TableOne_nonnormal_minMax")

    expect_equal_to_reference(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, noSpaces = TRUE, printToggle = TRUE),
                              "ref-TableOne_noSpaces")

    expect_equal_to_reference(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, showAllLevels = TRUE, printToggle = TRUE),
                              "ref-TableOne_showAllLevels")

    expect_equal_to_reference(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, noSpaces = TRUE, showAllLevels = FALSE, quote = TRUE, printToggle = TRUE),
                              "ref-TableOne_noSpaces_showAllLevels_quote")

    expect_equal_to_reference(print(pbcContOnlyByTrtSex),
                              "ref-TableOne_ContOnly")

    expect_equal_to_reference(print(pbcCatOnlyByTrtSex),
                              "ref-TableOne_CatOnly")

})


test_that("printing of a TableOne$CatTable object do not regress", {

    ## Expectations
    expect_equal_to_reference(print(pbcByTrt$CatTable, printToggle = TRUE),
                              "ref-CatTable_defaultPrint")

    expect_equal_to_reference(print(pbcOverall$CatTable, printToggle = TRUE),
                              "ref-CatTable_overallPrint")

    expect_equal_to_reference(print(pbcInclNa$CatTable, printToggle = TRUE),
                              "ref-CatTable_IncludeNA")

    expect_equal_to_reference(print(pbcByTrtSex$CatTable, printToggle = TRUE),
                              "ref-CatTable_2StrataVars")

    expect_equal_to_reference(print(pbcByTrtSex$CatTable, digits = 3, pDigits = 5, printToggle = TRUE),
                              "ref-CatTable_digits")

    expect_equal_to_reference(print(pbcByTrtSex$CatTable, test = FALSE, printToggle = TRUE),
                              "ref-CatTable_noTests")

    expect_equal_to_reference(print(pbcByTrt$CatTable, noSpaces = TRUE, printToggle = TRUE),
                              "ref-CatTable_noSpaces")

    expect_equal_to_reference(print(pbcByTrt$CatTable, showAllLevels = TRUE, printToggle = TRUE),
                              "ref-CatTable_showAllLevels")

    expect_equal_to_reference(print(pbcByTrt$CatTable, explain = FALSE, printToggle = TRUE),
                              "ref-CatTable_explain")

    expect_equal_to_reference(print(pbcByTrt$CatTable, format = "f", printToggle = TRUE),
                              "ref-CatTable_format_f")

    expect_equal_to_reference(print(pbcByTrt$CatTable, format = "p", printToggle = TRUE),
                              "ref-CatTable_format_p")

    expect_equal_to_reference(print(pbcByTrt$CatTable, format = "pf", printToggle = TRUE),
                              "ref-CatTable_format_pf")

    expect_equal_to_reference(print(pbcByTrt$CatTable, cramVars = "sex", printToggle = TRUE),
                              "ref-CatTable_cramVars")

    expect_equal_to_reference(print(pbcByTrt$CatTable, noSpaces = TRUE, showAllLevels = TRUE, quote = TRUE, printToggle = TRUE),
                              "ref-CatTable_noSpaces_showAllLevels_quote")

    ## gmodels::CrossTable
    print(pbcByTrt$CatTable, CrossTable = TRUE)
    expect_output(print(pbcByTrt$CatTable, CrossTable = TRUE),
"|-------------------------|
|                       N |
| Chi-square contribution |
|           N / Row Total |
|           N / Col Total |
|         N / Table Total |
|-------------------------|")

})


test_that("printing of a TableOne$ContTable object do not regress", {

    ## Expectations
    expect_equal_to_reference(print(pbcByTrt$ContTable, printToggle = TRUE),
                              "ref-ContTable_defaultPrint")

    expect_equal_to_reference(print(pbcOverall$ContTable, printToggle = TRUE),
                              "ref-ContTable_overallPrint")

    expect_equal_to_reference(print(pbcByTrtSex$ContTable, printToggle = TRUE),
                              "ref-ContTable_2StrataVars")

    expect_equal_to_reference(print(pbcByTrt$ContTable, digits = 3, pDigits = 5, printToggle = TRUE),
                              "ref-ContTable_digits")

    expect_equal_to_reference(print(pbcByTrt$ContTable, test = FALSE, printToggle = TRUE),
                              "ref-ContTable_noTests")

    expect_equal_to_reference(print(pbcByTrt$ContTable, nonnormal = nonnormalVars, exact = exactVars, printToggle = TRUE),
                              "ref-ContTable_nonnormal_exact")

    expect_equal_to_reference(print(pbcByTrt$ContTable, nonnormal = nonnormalVars, minMax = TRUE, printToggle = TRUE),
                              "ref-ContTable_nonnormal_minMax")

    expect_equal_to_reference(print(pbcByTrt$ContTable, noSpaces = TRUE, printToggle = TRUE),
                              "ref-ContTable_noSpaces")

    expect_equal_to_reference(print(pbcByTrt$ContTable, explain = FALSE, printToggle = TRUE),
                              "ref-ContTable_explain")

    expect_equal_to_reference(print(pbcByTrt$ContTable, noSpaces = TRUE, showAllLevels = TRUE, quote = TRUE, printToggle = TRUE),
                              "ref-ContTable_noSpaces_showAllLevels_quote")
})


test_that("summary method works without errors", {

    ## Expectations
    summary(pbcOverall)
    expect_output(summary(pbcOverall), "strata: Overall")
    summary(pbcInclNa)
    expect_output(summary(pbcInclNa), "<NA>")
    summary(pbcByTrt)
    expect_output(summary(pbcByTrt), "Standardize mean differences")
    summary(pbcByTrtSex)
    expect_output(summary(pbcByTrtSex),
                  "Standardize mean differences")
    summary(pbcContOnlyByTrtSex)
    expect_output(summary(pbcContOnlyByTrtSex),
                  "### Summary of continuous variables ###")
    summary(pbcCatOnlyByTrtSex)
    expect_output(summary(pbcCatOnlyByTrtSex), "3 vs 4")

})
