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
mwInclNa   <- svyCreateTableOne(vars = vars, data = datSvy, factorVars = factorVars, includeNA = TRUE)
mwByE    <- svyCreateTableOne(vars = vars, strata = c("E"), data = datSvy, factorVars = factorVars)
mwByEC1 <- svyCreateTableOne(vars = vars, strata = c("E","C1"), data = datSvy, factorVars = factorVars)
mwContOnlyByEC1 <- svyCreateTableOne(vars = c("E","C"), strata = c("E","C1"), data = datSvy, factorVars = factorVars)
mwCatOnlyByEC1 <- svyCreateTableOne(vars = c("Y","C1"), strata = c("E","C1"), data = datSvy, factorVars = factorVars)

## Specify variables for special handling
nonnormalVars <- c("C")
exactVars <- c("Y")


test_that("svyTableOne objects are always returned", {

    expect_equal(class(mwOverall),       c("svyTableOne","TableOne"))
    expect_equal(class(mwInclNa),        c("svyTableOne","TableOne"))
    expect_equal(class(mwByE),           c("svyTableOne","TableOne"))
    expect_equal(class(mwByEC1),         c("svyTableOne","TableOne"))
    expect_equal(class(mwContOnlyByEC1), c("svyTableOne","TableOne"))
    expect_equal(class(mwCatOnlyByEC1),  c("svyTableOne","TableOne"))

})


test_that("printing of a svyTableOne object does not regress", {

    ## Expectations
    expect_equal_to_reference(print(mwByE, printToggle = TRUE),
                              "ref-svyTableOne_defaultPrint")

    expect_equal_to_reference(print(mwOverall, printToggle = TRUE),
                              "ref-svyTableOne_overallPrint")

    expect_equal_to_reference(print(mwInclNa, printToggle = TRUE),
                              "ref-svyTableOne_IncludeNA")

    expect_equal_to_reference(print(mwByEC1, printToggle = TRUE),
                              "ref-svyTableOne_2StrataVars")

    expect_equal_to_reference(print(mwByE, catDigits = 3, contDigits = 4, pDigits = 5, printToggle = TRUE),
                              "ref-svyTableOne_digits")

    expect_equal_to_reference(print(mwByE, test = FALSE, printToggle = TRUE),
                              "ref-svyTableOne_noTests")

    expect_equal_to_reference(print(mwByE, nonnormal = nonnormalVars, exact = exactVars, printToggle = TRUE),
                              "ref-svyTableOne_nonnormal_exact")

    expect_equal_to_reference(print(mwByE, nonnormal = nonnormalVars, minMax = TRUE, printToggle = TRUE),
                              "ref-svyTableOne_nonnormal_minMax")

    expect_equal_to_reference(print(mwByE, catDigits = 3, noSpaces = TRUE, printToggle = TRUE),
                              "ref-svyTableOne_noSpaces")

    expect_equal_to_reference(print(mwByE, nonnormal = nonnormalVars, exact = exactVars, showAllLevels = TRUE, printToggle = TRUE),
                              "ref-svyTableOne_showAllLevels")

    expect_equal_to_reference(print(mwByE, catDigits = 3, nonnormal = nonnormalVars, exact = exactVars, noSpaces = TRUE, showAllLevels = FALSE, quote = TRUE, printToggle = TRUE),
                              "ref-svyTableOne_noSpaces_showAllLevels_quote")

    expect_equal_to_reference(print(mwContOnlyByEC1),
                              "ref-svyTableOne_ContOnly")

    expect_equal_to_reference(print(mwCatOnlyByEC1),
                              "ref-svyTableOne_CatOnly")

})


test_that("printing of a svyTableOne$CatTable object do not regress", {

    ## Expectations
    expect_equal_to_reference(print(mwByE$CatTable, printToggle = TRUE),
                              "ref-svyCatTable_defaultPrint")

    expect_equal_to_reference(print(mwOverall$CatTable, printToggle = TRUE),
                              "ref-svyCatTable_overallPrint")

    expect_equal_to_reference(print(mwInclNa$CatTable, printToggle = TRUE),
                              "ref-svyCatTable_IncludeNA")

    expect_equal_to_reference(print(mwByEC1$CatTable, printToggle = TRUE),
                              "ref-svyCatTable_2StrataVars")

    expect_equal_to_reference(print(mwByEC1$CatTable, digits = 3, pDigits = 5, printToggle = TRUE),
                              "ref-svyCatTable_digits")

    expect_equal_to_reference(print(mwByEC1$CatTable, test = FALSE, printToggle = TRUE),
                              "ref-svyCatTable_noTests")

    expect_equal_to_reference(print(mwByE$CatTable, digits = 3, noSpaces = TRUE, printToggle = TRUE),
                              "ref-svyCatTable_noSpaces")

    expect_equal_to_reference(print(mwByE$CatTable, showAllLevels = TRUE, printToggle = TRUE),
                              "ref-svyCatTable_showAllLevels")

    expect_equal_to_reference(print(mwByE$CatTable, explain = FALSE, printToggle = TRUE),
                              "ref-svyCatTable_explain")

    expect_equal_to_reference(print(mwByE$CatTable, format = "f", printToggle = TRUE),
                              "ref-svyCatTable_format_f")

    expect_equal_to_reference(print(mwByE$CatTable, format = "p", printToggle = TRUE),
                              "ref-svyCatTable_format_p")

    expect_equal_to_reference(print(mwByE$CatTable, format = "pf", printToggle = TRUE),
                              "ref-svyCatTable_format_pf")

    expect_equal_to_reference(print(mwByE$CatTable, cramVars = "Y", printToggle = TRUE),
                              "ref-svyCatTable_cramVars")

    expect_equal_to_reference(print(mwByE$CatTable, noSpaces = TRUE, showAllLevels = TRUE, quote = TRUE, printToggle = TRUE),
                              "ref-svyCatTable_noSpaces_showAllLevels_quote")

    ## gmodels::CrossTable
    print(mwByEC1$CatTable, CrossTable = TRUE)
    expect_output(print(mwByEC1$CatTable, CrossTable = TRUE),
"|-------------------------|
|                       N |
| Chi-square contribution |
|           N / Row Total |
|           N / Col Total |
|         N / Table Total |
|-------------------------|")

})


test_that("printing of a svyTableOne$ContTable object do not regress", {

    ## Expectations
    expect_equal_to_reference(print(mwByE$ContTable, printToggle = TRUE),
                              "ref-svyContTable_defaultPrint")

    expect_equal_to_reference(print(mwOverall$ContTable, printToggle = TRUE),
                              "ref-svyContTable_overallPrint")

    expect_equal_to_reference(print(mwByEC1$ContTable, printToggle = TRUE),
                              "ref-svyContTable_2StrataVars")

    expect_equal_to_reference(print(mwByE$ContTable, digits = 3, pDigits = 5, printToggle = TRUE),
                              "ref-svyContTable_digits")

    expect_equal_to_reference(print(mwByE$ContTable, test = FALSE, printToggle = TRUE),
                              "ref-svyContTable_noTests")

    expect_equal_to_reference(print(mwByE$ContTable, nonnormal = nonnormalVars, printToggle = TRUE),
                              "ref-svyContTable_nonnormal")

    expect_equal_to_reference(print(mwByE$ContTable, nonnormal = nonnormalVars, minMax = TRUE, printToggle = TRUE),
                              "ref-svyContTable_nonnormal_minMax")

    ## This does not make a difference here
    expect_equal_to_reference(print(mwByE$ContTable, noSpaces = TRUE, printToggle = TRUE),
                              "ref-svyContTable_noSpaces")


    expect_equal_to_reference(print(mwByE$ContTable, explain = FALSE, printToggle = TRUE),
                              "ref-svyContTable_explain")

    expect_equal_to_reference(print(mwByE$ContTable, noSpaces = TRUE, showAllLevels = TRUE, quote = TRUE, printToggle = TRUE),
                              "ref-svyContTable_noSpaces_showAllLevels_quote")
})



### p value calculations

test_that("p values are correctly calculated", {

    ## svychisq
    pValuesTestChisq <- c(svychisq( ~ Y + E, datSvy)$p.value, svychisq( ~ C1 + E, datSvy)$p.value)
    ## Drop names X-squared X-squared
    names(pValuesTestChisq) <- NULL
    expect_equal(attr(mwByE$CatTable, "pValues")[, "pApprox"], pValuesTestChisq)

    ## no exact tests
    expect_equal(attr(mwByE$CatTable, "pValues")[, "pExact"], c(NA, NA))

    ## svyglm to do ANOVA equivalent (first is removed to avoid singularity error on i386)
    pValuesTestNormal <-
    c(svyTestNormal("C ~ factor(E)", datSvy, test.terms = "factor(E)", method = "Wald")$p.value,
      svyTestNormal("C2 ~ factor(E)", datSvy, test.terms = "factor(E)", method = "Wald")$p.value)
    expect_equal(attr(mwByE$ContTable, "pValues")[, "pNormal"][-1], pValuesTestNormal)

    ## svyglm to do ANOVA equivalent (only solvable on non-i386 systems)
    if (R.Version()$arch != "i386") {
        expect_equal(attr(mwByE$ContTable, "pValues")[, "pNormal"][1],
                     svyTestNormal("E ~ factor(E)", datSvy, test.terms = "factor(E)", method = "Wald")$p.value)           }

    ## svyranktest
    pValuesTestNonNormal <-
    c(svyTestNonNormal("E ~ factor(E)", datSvy)$p.value,
      svyTestNonNormal("C ~ factor(E)", datSvy)$p.value,
      svyTestNonNormal("C2 ~ factor(E)", datSvy)$p.value)
    expect_equal(attr(mwByE$ContTable, "pValues")[, "pNonNormal"], pValuesTestNonNormal)

})



### Correctness of data object and final print out
################################################################################

## Create svydesign and TableOne object
data(nhanes)
nhanesSvy <- svydesign(ids = ~ SDMVPSU, strata = ~ SDMVSTRA, weights = ~ WTMEC2YR, nest = TRUE, data = nhanes)
tab1 <- svyCreateTableOne(vars = c("HI_CHOL","race","agecat","RIAGENDR"),
                          strata = "RIAGENDR", data = nhanesSvy,
                          factorVars = c("race","RIAGENDR"))

### Gold standar data
## Sample size
outTotals <- svyby(formula = ~ tableone:::one(HI_CHOL), by = ~ RIAGENDR, design = nhanesSvy,
                   FUN = svytotal, na.rm = TRUE)
## Means
outMeans <- svyby(formula = ~ HI_CHOL, by = ~ RIAGENDR, design = nhanesSvy, FUN = svymean, na.rm = TRUE)
## Variances (for SD)
outVar <- svyby(formula = ~ HI_CHOL, by = ~ RIAGENDR, design = nhanesSvy, FUN = svyvar, na.rm = TRUE)
## Quantiles
outQt1 <- svyquantile( ~ HI_CHOL, subset(nhanesSvy, RIAGENDR == 1), quantiles = c(0.5,0.25,0.75,0,1), na.rm = TRUE)
outQt2 <- svyquantile( ~ HI_CHOL, subset(nhanesSvy, RIAGENDR == 2), quantiles = c(0.5,0.25,0.75,0,1), na.rm = TRUE)
## Tests
pTTest <- sprintf(" %.7f", as.vector(svyttest(HI_CHOL ~ RIAGENDR, nhanesSvy)$p.value))
pRankTest <- sprintf(" %.7f", as.vector(svyranktest(HI_CHOL ~ RIAGENDR, nhanesSvy)$p.value))

test_that("continuous data object and final print outs are numerically correct", {

### Continuous part
### Sample sizes
## Correctness of one() function for sample size against total weights
expect_equal(as.vector(by(nhanes$WTMEC2YR, nhanes$RIAGENDR, sum)),
             outTotals[,2])
## Numerical correctness
expect_equal(c(tab1$ContTable[[1]][,"n"], tab1$ContTable[[2]][,"n"]),
             outTotals[,2])
## print out correctness
expect_equal(as.vector(print(tab1$ContTable, printToggle = TRUE)[1,1:2]),
             sprintf("%.2f", outTotals[,2]))

### Means
## Numerical correctness
expect_equal(c(tab1$ContTable[[1]][,"mean"], tab1$ContTable[[2]][,"mean"]),
             outMeans[,2])

### Standard deviations
## Numerically
expect_equal(c(tab1$ContTable[[1]][,"sd"], tab1$ContTable[[2]][,"sd"]),
             sqrt(outVar[,2]))
## print out (mean and sd)
expect_equal(as.vector(print(tab1$ContTable, digits = 2, printToggle = TRUE)[2,1:2]),
             sprintf("%.2f (%.2f)", outMeans[,2], sqrt(outVar[,2])))

### Quantiles
## median [p25, p75]
expect_equal(as.vector(print(tab1$ContTable, nonnormal = "HI_CHOL", printToggle = TRUE)[2,1:2]),
             c(do.call(sprintf, c(list(fmt = "%.2f [%.2f, %.2f]"), outQt1[1:3])),
               do.call(sprintf, c(list(fmt = "%.2f [%.2f, %.2f]"), outQt2[1:3]))))
## median [min,max]
expect_equal(as.vector(print(tab1$ContTable, nonnormal = "HI_CHOL", minMax = TRUE, printToggle = TRUE)[2,1:2]),
             c(do.call(sprintf, c(list(fmt = "%.2f [%.2f, %.2f]"), outQt1[c(1,4,5)])),
               do.call(sprintf, c(list(fmt = "%.2f [%.2f, %.2f]"), outQt2[c(1,4,5)]))))

### p-values
## t test
expect_equal(print(tab1$ContTable, pDigits = 7, printToggle = TRUE)[2,3],
             ## One space for <
             pTTest)
## KW
expect_equal(print(tab1$ContTable, pDigits = 7, nonnormal = "HI_CHOL", printToggle = TRUE)[2,3],
             ## One space for <
             pRankTest)
})


### Gold standard for categorical
outFreq <- svytable( ~ race + RIAGENDR, nhanesSvy)
outPerc <- prop.table(outFreq, margin = 2)
## Take first three rows only as space pading is tedious to reproduce
outFP <- cbind(sprintf("%.3f (%.3f) ", outFreq[,1], (outPerc[,1] * 100)),
               sprintf(" %.3f ( %.3f) ", outFreq[,2], (outPerc[,2] * 100)))[1:3,]
outFP2 <- cbind(sprintf(" %.3f (%.3f) ", outFreq[,1], (outPerc[,1] * 100)),
                sprintf(" %.3f ( %.3f) ", outFreq[,2], (outPerc[,2] * 100)))[1:3,]
## Test
pChisq <- sprintf(" %.7f", as.vector(svychisq( ~ race + RIAGENDR, nhanesSvy)$p.value))



test_that("categorical data object and final print outs are numerically correct", {

### Categorical part

### Sample sizes
expect_equal(as.vector(print(tab1$CatTable, digits = 3, printToggle = TRUE)[1,1:2]),
             sprintf("%.3f", outTotals[,2]))
### Frequencies and percentages
matFP <- print(tab1$CatTable, digits = 3, printToggle = TRUE)[3:5,1:2]
dimnames(matFP) <- NULL
expect_equal(matFP, outFP)

### p-values
expect_equal(print(tab1$CatTable, pDigits = 7, printToggle = TRUE)[2,3],
             pChisq)
})


test_that("mixed data object print outs are numerically correct", {

### Mixed object
## Mean and SD
expect_equal(as.vector(gsub("^ *", "", print(tab1, pDigits = 7)[2,1:2])),
             sprintf("%.2f (%.2f)", outMeans[,2], sqrt(outVar[,2])))
## normal test
expect_equal(print(tab1, pDigits = 7)[2,3], pTTest)
## nonnormal test
expect_equal(print(tab1, pDigits = 7, nonnormal = "HI_CHOL")[2,3], pTTest)

## Table
matFP <- print(tab1, catDigits = 3, printToggle = TRUE)[4:6,1:2]
dimnames(matFP) <- NULL
## column by column deleting spaces
expect_equal(matFP[,1], outFP2[,1])
expect_equal(gsub(" ", "", matFP[,1]), gsub(" ", "", outFP[,1]))
## chisq test
expect_equal(print(tab1, pDigits = 7, nonnormal = "HI_CHOL")[3,3],
             pChisq)

})


test_that("summary method works without errors", {

    ## Expectations
    summary(mwOverall)
    expect_output(summary(mwOverall), "### Summary of categorical variables ### ")
    summary(mwInclNa)
    expect_output(summary(mwInclNa), "<NA>")
    summary(mwByE)
    expect_output(summary(mwByE), "2 vs 3")
    summary(mwByEC1)
    expect_output(summary(mwByEC1),
                  "Standardize mean differences")
    summary(mwContOnlyByEC1)
    expect_output(summary(mwContOnlyByEC1),
                  "Standardize mean differences")
    summary(mwCatOnlyByEC1)
    expect_output(summary(mwCatOnlyByEC1),
                  "Standardize mean differences")

})
