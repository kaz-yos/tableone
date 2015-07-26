################################################################################
### Unit tests for the svyCreateTableOne
## Reference: http://adv-r.had.co.nz/Testing.html
## Created on: 2015-07-24
## Author: Kazuki Yoshida
################################################################################

### Structure
## expectations within tests within context

### Prepare environment
################################################################################
library(testthat)
library(survey)


### Context (1 for each file)
################################################################################
context("Unit tests for svy* user functions")



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

## Create individual level data
datMw <- datMw[rep(seq_along(datMw$n), datMw$n),]
## Introduce missingness
datMw[c(1,150), "Y"] <- NA
## Create a survey design object
datSvy <- svydesign(ids = ~ 1, data = datMw, weights = ~ Mw)

## Create a variable list
vars <- c("E", "C", "Y", "C1", "C2")
factorVars <- c("Y","C1")


### Tests
################################################################################
## A test should group together expectations for one functionality.


### Error handling

test_that("data assessment detects anomalies", {

    ## Error in ModuleStopIfNoVarsLeft(vars) (from modules.R#52) : No valid variables.
    ## In addition: Warning message:
    ## In ModuleReturnVarsExist(vars, data$variables) :
    ##   The data frame does not have: none  Dropped
    expect_warning(expect_error(svyCreateTableOne(vars = "non-existent", data = datSvy),
                                "No valid variables"),
                   "The data frame does not have: non-existent  Dropped")


    ## Error in StopIfNotSurveyDesign(data) :
    ##   The data argument needs to be a survey design object.
    expect_error(svyCreateTableOne(vars = c("E"), data = "not a svy object"),
                 "The data argument needs to be a survey design object")

    ## Data frame is not allowed
    expect_error(svyCreateTableOne(vars = c("E"), data = data.frame(E = c(1,2,3))),
                 "The data argument needs to be a survey design object")

    ## Error in ModuleReturnStrata(strata, data$variable) (from modules.R#84) :
    ##   None of the stratifying variables are present in the data frame.
    ## In addition: Warning message:
    ## In ModuleReturnVarsExist(strata, data) :
    ##   The data frame does not have: non-existent  Dropped
    expect_warning(expect_error(svyCreateTableOne(vars = vars, strata = c("non-existent"), data = datSvy),
                                "None of the stratifying variables are present in the data frame"),
                   "The data frame does not have: non-existent  Dropped")

})


### User-level functionalities

## Make categorical variables factors

## Create a table to test
mwOverall  <- svyCreateTableOne(vars = vars, data = datSvy, factorVars = factorVars)
mwByTrt    <- svyCreateTableOne(vars = vars, strata = c("E"), data = datSvy, factorVars = factorVars)
mwByTrtSex <- svyCreateTableOne(vars = vars, strata = c("E","C1"), data = datSvy, factorVars = factorVars)

## Specify variables for special handling
nonnormalVars <- c("C")
exactVars <- c("Y")


test_that("printing of a svyTableOne object does not regress", {

    ## Expectations
    expect_equal_to_reference(print(mwByTrt, printToggle = FALSE),
                              "ref-svyTableOne_defaultPrint")

    expect_equal_to_reference(print(mwOverall, printToggle = FALSE),
                              "ref-svyTableOne_overallPrint")

    expect_equal_to_reference(print(mwByTrtSex, printToggle = FALSE),
                              "ref-svyTableOne_2StrataVars")

    expect_equal_to_reference(print(mwByTrt, catDigits = 3, contDigits = 4, pDigits = 5, printToggle = FALSE),
                              "ref-svyTableOne_digits")

    expect_equal_to_reference(print(mwByTrt, test = FALSE, printToggle = FALSE),
                              "ref-svyTableOne_noTests")

    expect_equal_to_reference(print(mwByTrt, nonnormal = nonnormalVars, exact = exactVars, printToggle = FALSE),
                              "ref-svyTableOne_nonnormal_exact")

    expect_equal_to_reference(print(mwByTrt, nonnormal = nonnormalVars, minMax = TRUE, printToggle = FALSE),
                              "ref-svyTableOne_nonnormal_minMax")

    expect_equal_to_reference(print(mwByTrt, catDigits = 3, noSpaces = TRUE, printToggle = FALSE),
                              "ref-svyTableOne_noSpaces")

    expect_equal_to_reference(print(mwByTrt, nonnormal = nonnormalVars, exact = exactVars, showAllLevels = TRUE, printToggle = FALSE),
                              "ref-svyTableOne_showAllLevels")

    expect_equal_to_reference(print(mwByTrt, catDigits = 3, nonnormal = nonnormalVars, exact = exactVars, noSpaces = TRUE, showAllLevels = FALSE, quote = TRUE, printToggle = FALSE),
                              "ref-svyTableOne_noSpaces_showAllLevels_quote")
})


## These will fail when p-values are implemented
test_that("printing of a svyTableOne$CatTable object do not regress", {

    ## Expectations
    expect_equal_to_reference(print(mwByTrt$CatTable, printToggle = FALSE),
                              "ref-svyCatTable_defaultPrint")

    expect_equal_to_reference(print(mwOverall$CatTable, printToggle = FALSE),
                              "ref-svyCatTable_overallPrint")

    expect_equal_to_reference(print(mwByTrtSex$CatTable, printToggle = FALSE),
                              "ref-svyCatTable_2StrataVars")

    ## 2015-07-25 p values not implemented yet
    expect_equal_to_reference(print(mwByTrtSex$CatTable, digits = 3, pDigits = 5, printToggle = FALSE),
                              "ref-svyCatTable_digits")

    ## 2015-07-25 p not implemented, but will be correct without it
    expect_equal_to_reference(print(mwByTrtSex$CatTable, test = FALSE, printToggle = FALSE),
                              "ref-svyCatTable_noTests")

    ## In this case, noSpaces does not make any difference anyway
    expect_equal_to_reference(print(mwByTrt$CatTable, digits = 3, noSpaces = TRUE, printToggle = FALSE),
                              "ref-svyCatTable_noSpaces")

    expect_equal_to_reference(print(mwByTrt$CatTable, showAllLevels = TRUE, printToggle = FALSE),
                              "ref-svyCatTable_showAllLevels")

    expect_equal_to_reference(print(mwByTrt$CatTable, explain = FALSE, printToggle = FALSE),
                              "ref-svyCatTable_explain")

    expect_equal_to_reference(print(mwByTrt$CatTable, format = "f", printToggle = FALSE),
                              "ref-svyCatTable_format_f")

    expect_equal_to_reference(print(mwByTrt$CatTable, format = "p", printToggle = FALSE),
                              "ref-svyCatTable_format_p")

    expect_equal_to_reference(print(mwByTrt$CatTable, format = "pf", printToggle = FALSE),
                              "ref-svyCatTable_format_pf")

    expect_equal_to_reference(print(mwByTrt$CatTable, cramVars = "Y", printToggle = FALSE),
                              "ref-svyCatTable_cramVars")

    expect_equal_to_reference(print(mwByTrt$CatTable, noSpaces = TRUE, showAllLevels = TRUE, quote = TRUE, printToggle = FALSE),
                              "ref-svyCatTable_noSpaces_showAllLevels_quote")
})


test_that("printing of a svyTableOne$ContTable object do not regress", {

    ## Expectations
    expect_equal_to_reference(print(mwByTrt$ContTable, printToggle = FALSE),
                              "ref-svyContTable_defaultPrint")

    expect_equal_to_reference(print(mwOverall$ContTable, printToggle = FALSE),
                              "ref-svyContTable_overallPrint")

    expect_equal_to_reference(print(mwByTrtSex$ContTable, printToggle = FALSE),
                              "ref-svyContTable_2StrataVars")

    ## 2015-07-25 p values are not implemented yet
    expect_equal_to_reference(print(mwByTrt$ContTable, digits = 3, pDigits = 5, printToggle = FALSE),
                              "ref-svyContTable_digits")

    ## 2015-07-25 p values are not implemented yet, thus correct
    expect_equal_to_reference(print(mwByTrt$ContTable, test = FALSE, printToggle = FALSE),
                              "ref-svyContTable_noTests")

    expect_equal_to_reference(print(mwByTrt$ContTable, nonnormal = nonnormalVars, exact = exactVars, printToggle = FALSE),
                              "ref-svyContTable_nonnormal_exact")

    expect_equal_to_reference(print(mwByTrt$ContTable, nonnormal = nonnormalVars, minMax = TRUE, printToggle = FALSE),
                              "ref-svyContTable_nonnormal_minMax")

    ## This does not make a difference here
    expect_equal_to_reference(print(mwByTrt$ContTable, noSpaces = TRUE, printToggle = FALSE),
                              "ref-svyContTable_noSpaces")


    expect_equal_to_reference(print(mwByTrt$ContTable, explain = FALSE, printToggle = FALSE),
                              "ref-svyContTable_explain")

    expect_equal_to_reference(print(mwByTrt$ContTable, noSpaces = TRUE, showAllLevels = TRUE, quote = TRUE, printToggle = FALSE),
                              "ref-svyContTable_noSpaces_showAllLevels_quote")
})



### p value calculations

test_that("p values are correctly calculated", {

    ## svychisq
    pValuesTestChisq <- c(svychisq( ~ Y + E, datSvy)$p.value, svychisq( ~ C1 + E, datSvy)$p.value)
    ## Drop names X-squared X-squared
    names(pValuesTestChisq) <- NULL
    expect_equal(attr(mwByTrt$CatTable, "pValues")[, "pApprox"], pValuesTestChisq)

    ## no exact tests
    expect_equal(attr(mwByTrt$CatTable, "pValues")[, "pExact"], c(NA, NA))

    ## svyglm to do ANOVA equivalent
    pValuesTestNormal <-
    c(svyTestNormal("E ~ factor(E)", datSvy, test.terms = "factor(E)", method = "Wald")$p.value,
             svyTestNormal("C ~ factor(E)", datSvy, test.terms = "factor(E)", method = "Wald")$p.value,
             svyTestNormal("C2 ~ factor(E)", datSvy, test.terms = "factor(E)", method = "Wald")$p.value)
    expect_equal(attr(mwByTrt$ContTable, "pValues")[, "pNormal"], pValuesTestNormal)

    ## svyranktest
    pValuesTestNonNormal <-
    c(svyTestNonNormal("E ~ factor(E)", datSvy)$p.value,
      svyTestNonNormal("C ~ factor(E)", datSvy)$p.value,
      svyTestNonNormal("C2 ~ factor(E)", datSvy)$p.value)
    expect_equal(attr(mwByTrt$ContTable, "pValues")[, "pNonNormal"], pValuesTestNonNormal)

})
