#!/usr/bin/Rscript

################################################################################
### Test ExtractSmd function
##
## Created on: 2015-08-06
## Author: Kazuki Yoshida
################################################################################


### Structure
## expectations within tests within context

### Prepare environment
################################################################################

### Context (1 for each file)
context("Unit tests for the ExtractSmd function")


### Unweighted table

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
pbcByTrtSex <- CreateTableOne(vars = vars, strata = c("trt","sex"), data = pbc)

## Difficult objects
## SMD turned off
pbcByTrtSexOff <- CreateTableOne(vars = vars, strata = c("trt","sex"), data = pbc, smd = FALSE)
## No strata
pbcByOne <- CreateTableOne(vars = vars, data = pbc)
## Cont only
pbcContByTrtSex <- CreateTableOne(vars = varsContOnly, strata = c("trt","sex"), data = pbc)
## Cat only
pbcCatByTrtSex <- CreateTableOne(vars = varsCatOnly, strata = c("trt","sex"), data = pbc)


### Abnormal object test
test_that("Anomalous/difficult objects are handled correctly", {

    expect_warning(ExtractSmd(1),
                   "Unsupported object of class")

    ## Unstratified object necessarily returns NULL
    expect_equal(ExtractSmd(pbcByOne),
                 NULL)

    ## Object not containing SMD necessarily returns NULL
    expect_equal(ExtractSmd(pbcByTrtSexOff),
                 NULL)

    ## Continuous only
    expect_equal(ExtractSmd(pbcContByTrtSex),
                 attr(pbcContByTrtSex$ContTable, "smd"))

    ## Categorical only
    expect_equal(ExtractSmd(pbcCatByTrtSex),
                 attr(pbcCatByTrtSex$CatTable, "smd"))

})


test_that("ExtractSmd work on unweighted data", {

    ## Expectations
    ## Correct variable order
    print(ExtractSmd(pbcByTrtSex))
    expect_equal(rownames(ExtractSmd(pbcByTrtSex)),
                 pbcByTrtSex$MetaData$vars)

    ## Work on sub tables
    expect_equal(ExtractSmd(pbcByTrtSex$ContTable),
                 attr(pbcByTrtSex$ContTable, "smd"))
    expect_equal(ExtractSmd(pbcByTrtSex$CatTable),
                 attr(pbcByTrtSex$CatTable, "smd"))

    ## Regression test
    expect_equal_to_reference(ExtractSmd(pbcByTrtSex),
                              "ref-ExtractSmd_TableOne")

})



### Weighted table

library(survey)

## Create a weighted survey design object
data(nhanes)
nhanesSvy <- svydesign(ids = ~ SDMVPSU, strata = ~ SDMVSTRA, weights = ~ WTMEC2YR,
                       nest = TRUE, data = nhanes)

## Create a table object
## factorVars are converted to factors; no need for variables already factors
## strata will stratify summaries; leave it unspecified for overall summaries
nhanesByRace <- svyCreateTableOne(vars = c("HI_CHOL","race","agecat","RIAGENDR"),
                                  strata = "race", data = nhanesSvy,
                                  factorVars = c("race","RIAGENDR"))


test_that("ExtractSmd work on weighted data", {

    ## Expectations
    ## Correct variable order
    print(ExtractSmd(nhanesByRace))
    expect_equal(rownames(ExtractSmd(nhanesByRace)),
                 nhanesByRace$MetaData$vars)

    ## Work on sub tables
    expect_equal(ExtractSmd(nhanesByRace$ContTable),
                 attr(nhanesByRace$ContTable, "smd"))
    expect_equal(ExtractSmd(nhanesByRace$CatTable),
                 attr(nhanesByRace$CatTable, "smd"))

    ## Regression test
    expect_equal_to_reference(ExtractSmd(nhanesByRace),
                              "ref-ExtractSmd_svyTableOne")

})
