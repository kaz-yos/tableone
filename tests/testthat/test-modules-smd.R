################################################################################
### Unit tests for the standardized mean difference modules
## Reference: http://adv-r.had.co.nz/Testing.html
## Created on:
## Author: Kazuki Yoshida
################################################################################

### Structure
## expectations within tests within context


###
### References
################################################################################
## A unified approach to measuring the effect size between two groups using SAS
## http://support.sas.com/resources/papers/proceedings12/335-2012.pdf


###
### Prepare environment
################################################################################
library(testthat)
library(survey)
library(Matrix)
library(dummies)


###
### Actual tests
################################################################################
context("Tests for functions for standardized mean differences")


### Provide data
data(nhanes)
nhanesSvy <- svydesign(ids = ~ SDMVPSU, strata = ~ SDMVSTRA, weights = ~ WTMEC2YR,
                       nest = TRUE, data = nhanes)
## ‘SDMVPSU’ Primary sampling units
## ‘SDMVSTRA’ Sampling strata
## ‘WTMEC2YR’ Sampling weights
## ‘HI_CHOL’ Numeric vector: 1 for total cholesterol over 240mg/dl, 0
##      under 240mg/dl
## ‘race’ 1=Hispanic, 2=non-Hispanic white, 3=non-Hispanic black,
##      4=other
## ‘agecat’ Age group ‘(0,19]’ ‘(19,39]’ ‘(39,59]’ ‘(59,Inf]’
## ‘RIAGENDR’ Gender: 1=male, 2=female

## Use HI_CHOL for continuous, RIAGENDR for binary, race and agecat as categorical



### Old functions explicitly for 3 groups (Keep; used for testing)
## Standardize differences
StdDiffOld <- function(variable, group) {
    ## For each group
    means <- tapply(variable, group, mean)
    vars  <- tapply(variable, group, var)

    ## Calculate for all three possible pairs
    g1g2 <- abs(means[1] - means[2]) / sqrt((vars[1] + vars[2]) / 2)
    g2g3 <- abs(means[2] - means[3]) / sqrt((vars[2] + vars[3]) / 2)
    g3g1 <- abs(means[3] - means[1]) / sqrt((vars[3] + vars[1]) / 2)

    ## 2vs1, 3vs1, then 3vs2 to be consistent with regression
    out <- c(g1g2, g3g1, g2g3)
    names(out) <- c("3vs2","3vs1","2vs1")
    out
}

svyStdDiffOld <- function(varName, groupName, design) {

    varFormula   <- as.formula(paste("~", varName))
    groupFormula <- as.formula(paste("~", groupName))

    means <- svyby(formula = varFormula, by = groupFormula, FUN = svymean, design = design)[,2]
    vars  <- svyby(formula = varFormula, by = groupFormula, FUN = svyvar,  design = design)[,2]

    ## Calculate for all three possible pairs
    g1g2 <- abs(means[1] - means[2]) / sqrt((vars[1] + vars[2]) / 2)
    g2g3 <- abs(means[2] - means[3]) / sqrt((vars[2] + vars[3]) / 2)
    g3g1 <- abs(means[3] - means[1]) / sqrt((vars[3] + vars[1]) / 2)

    ## 2vs1, 3vs1, then 3vs2 to be consistent with regression
    out <- c(g1g2, g3g1, g2g3)
    names(out) <- c("3vs2","3vs1","2vs1")
    out
}


test_that("continuous standardized difference is correct (nhanes unweighted)", {

    ## Two group with only one contrast 1 vs 2
    means1 <- tapply(nhanes$HI_CHOL, nhanes$RIAGENDR, mean, na.rm = TRUE)
    vars1  <- tapply(nhanes$HI_CHOL, nhanes$RIAGENDR, var, na.rm = TRUE)
    meanDiffs1 <- (means1[1] - means1[2]) / sqrt((vars1[1] + vars1[2]) / 2)
    names(meanDiffs1) <- NULL

    expect_equal(StdDiff(nhanes$HI_CHOL, nhanes$RIAGENDR), abs(meanDiffs1))


    ## Four groups with 6 contrasts
    means2 <- tapply(nhanes$HI_CHOL, nhanes$race, mean, na.rm = TRUE)
    vars2  <- tapply(nhanes$HI_CHOL, nhanes$race, var, na.rm = TRUE)
    meanDiffs2 <-
    c((means2[1] - means2[2]) / sqrt((vars2[1] + vars2[2]) / 2),
      (means2[1] - means2[3]) / sqrt((vars2[1] + vars2[3]) / 2),
      (means2[1] - means2[4]) / sqrt((vars2[1] + vars2[4]) / 2),
      (means2[2] - means2[3]) / sqrt((vars2[2] + vars2[3]) / 2),
      (means2[2] - means2[4]) / sqrt((vars2[2] + vars2[4]) / 2),
      (means2[3] - means2[4]) / sqrt((vars2[3] + vars2[4]) / 2))
    names(meanDiffs2) <- NULL

    ## Individual numbers
    expect_equal(StdDiff(nhanes$HI_CHOL, nhanes$race), abs(meanDiffs2))
    ## Average across
    expect_equal(mean(StdDiff(nhanes$HI_CHOL, nhanes$race)),
                 mean(abs(meanDiffs2)))

})


test_that("continuous standardized difference is correct (nhanes weighted)", {

    ## Two group
    means1 <- svyby( ~ HI_CHOL, by = ~ RIAGENDR, design = nhanesSvy, FUN = svymean, na.rm = TRUE)[,2]
    vars1  <- svyby( ~ HI_CHOL, by = ~ RIAGENDR, design = nhanesSvy, FUN = svyvar, na.rm = TRUE)[,2]
    meanDiffs1 <- (means1[1] - means1[2]) / sqrt((vars1[1] + vars1[2]) / 2)

    expect_equal(svyStdDiff("HI_CHOL", "RIAGENDR", design = nhanesSvy),
                 abs(meanDiffs1))


    ## Four groups
    means2 <- svyby( ~ HI_CHOL, by = ~ race, design = nhanesSvy, FUN = svymean, na.rm = TRUE)[,2]
    vars2  <- svyby( ~ HI_CHOL, by = ~ race, design = nhanesSvy, FUN = svyvar, na.rm = TRUE)[,2]
    meanDiffs2 <-
    c((means2[1] - means2[2]) / sqrt((vars2[1] + vars2[2]) / 2),
      (means2[1] - means2[3]) / sqrt((vars2[1] + vars2[3]) / 2),
      (means2[1] - means2[4]) / sqrt((vars2[1] + vars2[4]) / 2),
      (means2[2] - means2[3]) / sqrt((vars2[2] + vars2[3]) / 2),
      (means2[2] - means2[4]) / sqrt((vars2[2] + vars2[4]) / 2),
      (means2[3] - means2[4]) / sqrt((vars2[3] + vars2[4]) / 2))
    names(meanDiffs2) <- NULL

    ## Individual numbers
    expect_equal(svyStdDiff("HI_CHOL", "race", design = nhanesSvy),
                 abs(meanDiffs2))
    ## Average across
    expect_equal(mean(svyStdDiff("HI_CHOL", "race", design = nhanesSvy)),
                 mean(abs(meanDiffs2)))

})


test_that("binary standardized difference is correct (nhanes unweighted)", {

    ## Two group with only one contrast 1 vs 2
    means1 <- tapply(nhanes$HI_CHOL, nhanes$RIAGENDR, mean, na.rm = TRUE)
    vars1  <- means1 * (1 - means1)
    meanDiffs1 <- (means1[1] - means1[2]) / sqrt((vars1[1] + vars1[2]) / 2)
    names(meanDiffs1) <- NULL

    expect_equal(StdDiff(nhanes$HI_CHOL, nhanes$RIAGENDR, binary = TRUE),
                 abs(meanDiffs1))


    ## Four groups with 6 contrasts
    means2 <- tapply(nhanes$HI_CHOL, nhanes$race, mean, na.rm = TRUE)
    vars2  <- means2 * (1 - means2)
    meanDiffs2 <-
    c((means2[1] - means2[2]) / sqrt((vars2[1] + vars2[2]) / 2),
      (means2[1] - means2[3]) / sqrt((vars2[1] + vars2[3]) / 2),
      (means2[1] - means2[4]) / sqrt((vars2[1] + vars2[4]) / 2),
      (means2[2] - means2[3]) / sqrt((vars2[2] + vars2[3]) / 2),
      (means2[2] - means2[4]) / sqrt((vars2[2] + vars2[4]) / 2),
      (means2[3] - means2[4]) / sqrt((vars2[3] + vars2[4]) / 2))
    names(meanDiffs2) <- NULL

    ## Individual numbers
    expect_equal(StdDiff(nhanes$HI_CHOL, nhanes$race, binary = TRUE),
                 abs(meanDiffs2))
    ## Average across
    expect_equal(mean(StdDiff(nhanes$HI_CHOL, nhanes$race, binary = TRUE)),
                 mean(abs(meanDiffs2)))

})


test_that("binary standardized difference is correct (nhanes weighted)", {

    ## Two group
    means1 <- svyby( ~ HI_CHOL, by = ~ RIAGENDR, design = nhanesSvy, FUN = svymean, na.rm = TRUE)[,2]
    vars1  <- means1 * (1 - means1)
    meanDiffs1 <- (means1[1] - means1[2]) / sqrt((vars1[1] + vars1[2]) / 2)

    expect_equal(svyStdDiff("HI_CHOL", "RIAGENDR", design = nhanesSvy, binary = TRUE),
                 abs(meanDiffs1))


    ## Four groups
    means2 <- svyby( ~ HI_CHOL, by = ~ race, design = nhanesSvy, FUN = svymean, na.rm = TRUE)[,2]
    vars2  <- means2 * (1 - means2)
    meanDiffs2 <-
    c((means2[1] - means2[2]) / sqrt((vars2[1] + vars2[2]) / 2),
      (means2[1] - means2[3]) / sqrt((vars2[1] + vars2[3]) / 2),
      (means2[1] - means2[4]) / sqrt((vars2[1] + vars2[4]) / 2),
      (means2[2] - means2[3]) / sqrt((vars2[2] + vars2[3]) / 2),
      (means2[2] - means2[4]) / sqrt((vars2[2] + vars2[4]) / 2),
      (means2[3] - means2[4]) / sqrt((vars2[3] + vars2[4]) / 2))
    names(meanDiffs2) <- NULL

    ## Individual numbers
    expect_equal(svyStdDiff("HI_CHOL", "race", design = nhanesSvy, binary = TRUE),
                 abs(meanDiffs2))
    ## Average across
    expect_equal(mean(svyStdDiff("HI_CHOL", "race", design = nhanesSvy, binary = TRUE)),
                 mean(abs(meanDiffs2)))

})


test_that("multinomal helpers are correct", {

    means <- c(0.310336708264657, 0.224393689663292, 0.234283023310572, 0.230986578761479)
    covs  <- MultinomialVar(means)

    ## Expectations
    ## Diagonal p(1-p)
    expect_equal(diag(covs), means * (1 - means))
    ## Off diagonal -1 * p_i * p_j
    expect_equal(covs[1,2], -1 * means[1] * means[2])
    expect_equal(covs[1,3], -1 * means[1] * means[3])
    expect_equal(covs[1,4], -1 * means[1] * means[4])
    expect_equal(covs[2,3], -1 * means[2] * means[3])
    expect_equal(covs[2,4], -1 * means[2] * means[4])
    expect_equal(covs[3,4], -1 * means[3] * means[4])

})


test_that("categorical standardized difference is correct (nhanes unweighted)", {

    ## Four groups with 6 contrasts
    means2 <- tapply(nhanes$HI_CHOL, nhanes$race, mean, na.rm = TRUE)
    vars2  <- means2 * (1 - means2)
    meanDiffs2 <-
    c((means2[1] - means2[2]) / sqrt((vars2[1] + vars2[2]) / 2),
      (means2[1] - means2[3]) / sqrt((vars2[1] + vars2[3]) / 2),
      (means2[1] - means2[4]) / sqrt((vars2[1] + vars2[4]) / 2),
      (means2[2] - means2[3]) / sqrt((vars2[2] + vars2[3]) / 2),
      (means2[2] - means2[4]) / sqrt((vars2[2] + vars2[4]) / 2),
      (means2[3] - means2[4]) / sqrt((vars2[3] + vars2[4]) / 2))
    names(meanDiffs2) <- NULL

    ## Individual numbers
    expect_equal(StdDiffMulti(nhanes$HI_CHOL, nhanes$race),
                 abs(meanDiffs2))
    ## Average across
    expect_equal(mean(StdDiffMulti(nhanes$HI_CHOL, nhanes$race)),
                 mean(abs(meanDiffs2)))


    ## Two group with only one contrast 1 vs 2
    strataByLevels1 <- xtabs( ~ RIAGENDR + agecat, data = nhanes)
    ## drop first column after calculating proportion
    propTable1 <- prop.table(strataByLevels1, margin = 1)[, -1, drop = FALSE]
    meanDiffs1 <- propTable1[1,] - propTable1[2,]
    covMean1 <- (MultinomialVar(propTable1[1,]) + MultinomialVar(propTable1[2,])) / 2

    ## R is not strict about transposition
    expect_equal(meanDiffs1 %*% MASS::ginv(covMean1) %*% meanDiffs1,
                 t(meanDiffs1) %*% MASS::ginv(covMean1) %*% t(t(meanDiffs1)))

    ## Test actual functions
    expect_equal(StdDiffMulti(nhanes$agecat, nhanes$RIAGENDR),
                 sqrt(drop(t(meanDiffs1) %*% MASS::ginv(covMean1) %*% t(t(meanDiffs1)))))


    ## Four groups with 6 contrasts
    strataByLevels2 <- xtabs( ~ race + agecat, data = nhanes)
    propTable2 <- prop.table(strataByLevels2, margin = 1)[, -1, drop = FALSE]
    meanDiffs2 <- list(propTable2[1,] - propTable2[2,],
                       propTable2[1,] - propTable2[3,],
                       propTable2[1,] - propTable2[4,],
                       propTable2[2,] - propTable2[3,],
                       propTable2[2,] - propTable2[4,],
                       propTable2[3,] - propTable2[4,])

    covMean2 <-
    list((MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[3,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[4,])) / 2,
         (MultinomialVar(propTable2[2,]) + MultinomialVar(propTable2[3,])) / 2,
         (MultinomialVar(propTable2[2,]) + MultinomialVar(propTable2[4,])) / 2,
         (MultinomialVar(propTable2[3,]) + MultinomialVar(propTable2[4,])) / 2)

    ## These should match in length.
    expect_equal(length(meanDiffs2), length(covMean2))

    smds <- unlist(lapply(seq_along(meanDiffs2), function(i) {

        sqrt(drop(t(meanDiffs2[[i]]) %*% MASS::ginv(covMean2[[i]]) %*% t(t(meanDiffs2[[i]]))))
    }))


    ## Individual numbers
    expect_equal(StdDiffMulti(nhanes$agecat, nhanes$race),
                 smds)
    ## Average across
    expect_equal(mean(StdDiffMulti(nhanes$agecat, nhanes$race)),
                 mean(smds))

})


test_that("categorical standardized difference is correct (nhanes weighted)", {

    ## Binary four groups
    means2 <- svyby( ~ HI_CHOL, by = ~ race, design = nhanesSvy, FUN = svymean, na.rm = TRUE)[,2]
    vars2  <- means2 * (1 - means2)
    meanDiffs2 <-
    c((means2[1] - means2[2]) / sqrt((vars2[1] + vars2[2]) / 2),
      (means2[1] - means2[3]) / sqrt((vars2[1] + vars2[3]) / 2),
      (means2[1] - means2[4]) / sqrt((vars2[1] + vars2[4]) / 2),
      (means2[2] - means2[3]) / sqrt((vars2[2] + vars2[3]) / 2),
      (means2[2] - means2[4]) / sqrt((vars2[2] + vars2[4]) / 2),
      (means2[3] - means2[4]) / sqrt((vars2[3] + vars2[4]) / 2))
    names(meanDiffs2) <- NULL

    ## Individual numbers
    expect_equal(svyStdDiffMulti("HI_CHOL", "race", design = nhanesSvy),
                 abs(meanDiffs2))
    ## Average across
    expect_equal(mean(svyStdDiffMulti("HI_CHOL", "race", design = nhanesSvy)),
                 mean(abs(meanDiffs2)))


    ## Two group with only one contrast 1 vs 2
    strataByLevels1 <- svytable( ~ RIAGENDR + agecat, design = nhanesSvy)
    propTable1 <- prop.table(strataByLevels1, margin = 1)[, -1, drop = FALSE]
    meanDiffs1 <- propTable1[1,] - propTable1[2,]
    covMean1 <- (MultinomialVar(propTable1[1,]) + MultinomialVar(propTable1[2,])) / 2

    ## Test actual functions
    expect_equal(svyStdDiffMulti("agecat", "RIAGENDR", design = nhanesSvy),
                 sqrt(drop(t(meanDiffs1) %*% MASS::ginv(covMean1) %*% t(t(meanDiffs1)))))


    ## Four groups with 6 contrasts
    strataByLevels2 <- svytable( ~ race + agecat, design = nhanesSvy)
    propTable2 <- prop.table(strataByLevels2, margin = 1)[, -1, drop = FALSE]
    meanDiffs2 <- list(propTable2[1,] - propTable2[2,],
                       propTable2[1,] - propTable2[3,],
                       propTable2[1,] - propTable2[4,],
                       propTable2[2,] - propTable2[3,],
                       propTable2[2,] - propTable2[4,],
                       propTable2[3,] - propTable2[4,])

    covMean2 <-
    list((MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[3,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[4,])) / 2,
         (MultinomialVar(propTable2[2,]) + MultinomialVar(propTable2[3,])) / 2,
         (MultinomialVar(propTable2[2,]) + MultinomialVar(propTable2[4,])) / 2,
         (MultinomialVar(propTable2[3,]) + MultinomialVar(propTable2[4,])) / 2)

    ## These should match in length.
    expect_equal(length(meanDiffs2), length(covMean2))

    smds <- unlist(lapply(seq_along(meanDiffs2), function(i) {

        sqrt(drop(t(meanDiffs2[[i]]) %*% MASS::ginv(covMean2[[i]]) %*% t(t(meanDiffs2[[i]]))))
    }))

    ## Individual numbers
    expect_equal(svyStdDiffMulti("agecat", "race", design = nhanesSvy),
                 smds)
    ## Average across
    expect_equal(mean(svyStdDiffMulti("agecat", "race", design = nhanesSvy)),
                 mean(smds))

})


### Test on anomalous data
################################################################################

test_that("decent results are returned for anomalous/difficult data", {

    data(nhanes)
    nhanes$onlyOne <- 1
    nhanes$onlyNa  <- as.numeric(NA)
    nhanes$logi    <- nhanes$RIAGENDR == 1
    nhanes$race2   <- nhanes$race
    nhanesSvy <- svydesign(ids = ~ SDMVPSU, strata = ~ SDMVSTRA,
                           weights = ~ WTMEC2YR, nest = TRUE,
                           data = nhanes)

    ## Note: 2015-08-01
    ## Categorical SMD: When both of two group have at one or more zero
    ## categories, both contribute singular matrices to the pooled
    ## variance covariance matrix S in Yang and Dalton 2012.
    ##
    ## When both groups have a single category that is probability 1 and
    ## other cells that are all zeros, both contribute zero matrices to
    ## the pooled variance covariance matrix S. solve() dies on this
    ## singular matrix. MASS::ginv() returns generalized inverse matrix,
    ## which is the zero matrix of the same dimension, instead.
    ##
    ## This results in SMD of zero regardless of the mean vector difference
    ## (T-C in Yang and Dalton 2012).
    ##
    ## This behavior looks odd, thus, categorical SMD functions were
    ## modified to return NaN when S is a zero matrix.

    ## Logical
    ## 0 due to [0]^- = 0
    table(nhanes$logi, nhanes$RIAGENDR)
    expect_equal(StdDiffMulti(nhanes$logi, group = nhanes$RIAGENDR), NaN)
    ## Matches with result from original variable
    expect_equal(StdDiffMulti(nhanes$logi,     group = nhanes$race),
                 StdDiffMulti(nhanes$RIAGENDR, group = nhanes$race))

    ## Multiple group where T-C is not a zero vector.
    ## All groups have single group that is probability 1, thus,
    ## all contrasts have zero matrix S.
    table(nhanes$race2, nhanes$race)
    expect_equal(StdDiffMulti(nhanes$race2, group = nhanes$race), rep(NaN, 6))
    expect_equal(svyStdDiffMulti("race2", group = "race", design = nhanesSvy), rep(NaN, 6))


    ## Only one value
    ## NaN due to division by zero variance
    by(nhanes$onlyOne, nhanes$RIAGENDR, summary)
    expect_equal(StdDiff(nhanes$onlyOne, group = nhanes$RIAGENDR), NaN)
    ## 0 because [0]^-  = 0, and [1]^T [0]^-1 [1] = 0; defined NaN in (svy)StdDiffMulti
    table(nhanes$onlyOne, nhanes$RIAGENDR)
    expect_equal(StdDiffMulti(nhanes$onlyOne, group = nhanes$RIAGENDR), NaN)
    ## When weighted problematic
    means1 <- svyby(~ onlyOne, by = ~ RIAGENDR, nhanesSvy, FUN = svymean)[,2]
    vars1  <- svyby(~ onlyOne, by = ~ RIAGENDR, nhanesSvy, FUN = svyvar)[,2]
    ## Very small difference is inflated by even smaller variance
    ## on sparc-sun-solaris sign was opposite; abs() solves this issue
    ## as svyStdDiff() uses abs() internally
    expect_equal(svyStdDiff("onlyOne", "RIAGENDR", nhanesSvy),
                 abs((means1[1] - means1[2]) / sqrt(sum(vars1)  / 2)))
    ## NaN should be the case, but it's not, but it's consistent with survey
    ## expect_equal(svyStdDiff("onlyOne", "RIAGENDR", nhanesSvy), NaN)

    ## 0 because [0]^-  = 0, and [1]^T [0]^-1 [1] = 0; defined NaN in (svy)StdDiffMulti
    ## No error even with a single level variable (constant) as redundant
    ## level drop from table occurs only when 2+ levels are present.
    ## If any group has more than 2 levels, then strata-by-level table
    ## is correctly created, which is not the case here. Redefined NaN.
    expect_equal(svyStdDiffMulti("onlyOne", "RIAGENDR", nhanesSvy), NaN)

    ## Four groups (six contrasts)
    ## NaN due to division by zero variance
    by(nhanes$onlyOne, nhanes$race, summary)
    expect_equal(StdDiff(nhanes$onlyOne, group = nhanes$race), rep(NaN, 6))
    ## 0 because [0]^-  = 0, and [1]^T [0]^-1 [1] = 0; defined NaN in (svy)StdDiffMulti
    expect_equal(StdDiffMulti(nhanes$onlyOne, group = nhanes$race), rep(NaN, 6))
    ## When weighted problematic; not in this case??
    means2 <- svyby(~ onlyOne, by = ~ race, nhanesSvy, FUN = svymean)[,2]
    vars2  <- svyby(~ onlyOne, by = ~ race, nhanesSvy, FUN = svyvar)[,2]
    meanDiffs2 <- c((means2[1] - means2[2]) / sqrt((vars2[1] + vars2[2]) / 2),
                    (means2[1] - means2[3]) / sqrt((vars2[1] + vars2[3]) / 2),
                    (means2[1] - means2[4]) / sqrt((vars2[1] + vars2[4]) / 2),
                    (means2[2] - means2[3]) / sqrt((vars2[2] + vars2[3]) / 2),
                    (means2[2] - means2[4]) / sqrt((vars2[2] + vars2[4]) / 2),
                    (means2[3] - means2[4]) / sqrt((vars2[3] + vars2[4]) / 2))
    ## on sparc-sun-solaris sign was opposite; abs() solves this issue
    ## as svyStdDiff() uses abs() internally
    expect_equal(svyStdDiff("onlyOne", "race", nhanesSvy), abs(meanDiffs2))
    ## This one is rep(NaN,6) for most platforms except for sparc-sun-solaris
    ## where ./configure --disable-long-double is used.
    ## capabilities()["long.double"] was added in R 3.1.3.
    ## As of 2015-08-11, only r-oldrel-windows-ix86+x86_64 is R 3.1.3.
    ## https://cran.r-project.org/web/checks/check_results_tableone.html
    if (capabilities()["long.double"]) {
        ## Cannot run on sparc-sun-solaris due to lack of extended precision arithmetic
        expect_equal(svyStdDiff("onlyOne", "race", nhanesSvy), rep(NaN, 6))
    }
    ## 0 because [0]^-  = 0, and [1]^T [0]^-1 [1] = 0; defined NaN in (svy)StdDiffMulti
    expect_equal(svyStdDiffMulti("onlyOne", "race", nhanesSvy), rep(NaN, 6))


    ## onlyNa
    ## NA as na.rm is turned off
    expect_warning(expect_equal(StdDiff(nhanes$onlyNa, group = nhanes$RIAGENDR),
                                as.numeric(NA)),
                   "Variable has only NA's in at least one stratum. na.rm turned off.")
    ## 0 only one level
    expect_warning(expect_equal(StdDiffMulti(nhanes$onlyNa, group = nhanes$RIAGENDR), NaN),
                   "Variable has only NA's in all strata. Regarding NA as a level")
    ## When weighted problematic
    means1 <- svyby(~ onlyNa, by = ~ RIAGENDR, nhanesSvy, FUN = svymean)[,2]
    vars1  <- svyby(~ onlyNa, by = ~ RIAGENDR, nhanesSvy, FUN = svyvar)[,2]
    ## Very small difference is inflated by even smaller variance
    expect_warning(svyStdDiff("onlyNa", "RIAGENDR", nhanesSvy),
                   "onlyNa has only NA's in at least one stratum. na.rm turned off.")
    expect_warning(expect_equal(svyStdDiff("onlyNa", "RIAGENDR", nhanesSvy),
                                as.numeric(NA)),
                   "onlyNa has only NA's in at least one stratum. na.rm turned off.")
    ## 0 because [0]^-  = 0, and [1]^T [0]^-1 [1] = 0; defined NaN in (svy)StdDiffMulti
    expect_warning(expect_equal(svyStdDiffMulti("onlyNa", "RIAGENDR", nhanesSvy), NaN),
                   "onlyNa has only NA's in all strata. Regarding NA as a level.")

    ## Four groups (six contrasts)
    ## NaN due to division by zero variance
    expect_warning(expect_equal(StdDiff(nhanes$onlyNa, group = nhanes$race), rep(NaN, 6)),
                   "Variable has only NA's in at least one stratum. na.rm turned off.")
    ## 0 because [0]^-  = 0, and [1]^T [0]^-1 [1] = 0; defined NaN in (svy)StdDiffMulti
    expect_warning(expect_equal(StdDiffMulti(nhanes$onlyNa, group = nhanes$race), rep(NaN, 6)),
                   "Variable has only NA's in all strata. Regarding NA as a level.")
    ## When weighted problematic; not in this case??
    means2 <- svyby(~ onlyNa, by = ~ race, nhanesSvy, FUN = svymean)[,2]
    vars2  <- svyby(~ onlyNa, by = ~ race, nhanesSvy, FUN = svyvar)[,2]
    meanDiffs2 <- c((means2[1] - means2[2]) / sqrt((vars2[1] + vars2[2]) / 2),
                    (means2[1] - means2[3]) / sqrt((vars2[1] + vars2[3]) / 2),
                    (means2[1] - means2[4]) / sqrt((vars2[1] + vars2[4]) / 2),
                    (means2[2] - means2[3]) / sqrt((vars2[2] + vars2[3]) / 2),
                    (means2[2] - means2[4]) / sqrt((vars2[2] + vars2[4]) / 2),
                    (means2[3] - means2[4]) / sqrt((vars2[3] + vars2[4]) / 2))
    expect_warning(expect_equal(svyStdDiff("onlyNa", "race", nhanesSvy),  meanDiffs2),
                   "onlyNa has only NA's in at least one stratum. na.rm turned off.")
    expect_warning(expect_equal(svyStdDiff("onlyNa", "race", nhanesSvy), rep(NaN, 6)),
                   "onlyNa has only NA's in at least one stratum. na.rm turned off.")
    ## 0 because [0]^-  = 0, and [1]^T [0]^-1 [1] = 0; defined NaN in (svy)StdDiffMulti
    expect_warning(expect_equal(svyStdDiffMulti("onlyNa", "race", nhanesSvy), rep(NaN, 6)),
                   "onlyNa has only NA's in all strata. Regarding NA as a level.")

})



### Define a function for pooled S matrix (used in two tests)
pooledSMat <- function(strataByLevels) {
    lstMeans <- LstMeansFromFullTable(strataByLevels)
    S1 <- MultinomialVar(lstMeans[[1]])
    S2 <- MultinomialVar(lstMeans[[2]])
    S <- (S1 + S2) / 2
    ## first group element, second group element, pooled
    list(S1 = S1, S2 = S2, S = S)
}


test_that("zero-cell vectors are decently handled by categorical SMD (artificial data)", {

    set.seed(2015080200)
    ## Simple artificial data
    wt <- runif(n = 100)
    dat <- data.frame(group = rep(c(1,2), each = 100),
                      var1  = factor(c(rep(1:4, c(0,10,20,70)), rep(1:4, c( 0,10,20,70))), level = 1:4),
                      var2  = factor(c(rep(1:4, c(0,10,20,70)), rep(1:4, c( 0,20,30,50))), level = 1:4),
                      var3  = factor(c(rep(1:4, c(0,10,20,70)), rep(1:4, c(20, 0,30,50))), level = 1:4),
                      var4  = factor(c(rep(1:4, c(0,10,20,70)), rep(1:4, c(10,20,30,40))), level = 1:4),
                      wt    = rep(wt, 2))
    datSvy <- svydesign(ids = ~ 1, data = dat, weights = ~wt)


    ## Expectations
    ## First case
    var1S <- pooledSMat(table(dat$group, dat$var1))
    expect_true(rankMatrix(var1S[[1]]) < 3)
    expect_true(rankMatrix(var1S[[2]]) < 3)
    expect_true(rankMatrix(var1S[[3]]) < 3)
    ## Using ginv zero due to numerator (OK)
    expect_equal(StdDiffMulti(dat$var1, dat$group), 0)

    ## Second case
    var2S <- pooledSMat(table(dat$group, dat$var2))
    expect_true(rankMatrix(var2S[[1]]) < 3)
    expect_true(rankMatrix(var2S[[2]]) < 3)
    expect_true(rankMatrix(var2S[[3]]) < 3)
    ## Using ginv() non-zero
    expect_true(StdDiffMulti(dat$var2, dat$group) > 0)

    ## Third case
    var3S <- pooledSMat(table(dat$group, dat$var3))
    expect_true(rankMatrix(var3S[[1]]) < 3)
    expect_true(rankMatrix(var3S[[2]]) < 3)
    ## pooled can be full rank if zero cell positions are different
    expect_true(rankMatrix(var3S[[3]]) == 3)
    ## Using true inverse from ginv, non-zero
    expect_true(StdDiffMulti(dat$var3, dat$group) > 0)

    ## Third case
    var4S <- pooledSMat(table(dat$group, dat$var4))
    expect_true(rankMatrix(var4S[[1]]) < 3)
    ## All cells populated and full rank
    expect_true(rankMatrix(var4S[[2]]) == 3)
    ## pooled can be full rank if one group is full rank
    expect_true(rankMatrix(var4S[[3]]) == 3)
    expect_true(StdDiffMulti(dat$var4, dat$group) > 0)


    ## Weighted
    ## First case
    var1S <- pooledSMat(svytable( ~ group + var1, datSvy))
    expect_true(rankMatrix(var1S[[1]]) < 3)
    expect_true(rankMatrix(var1S[[2]]) < 3)
    expect_true(rankMatrix(var1S[[3]]) < 3)
    ## Using ginv zero due to numerator (OK)
    expect_equal(svyStdDiffMulti("var1", "group", datSvy), 0)

    ## Second case
    var2S <- pooledSMat(svytable( ~ group + var2, datSvy))
    expect_true(rankMatrix(var2S[[1]]) < 3)
    expect_true(rankMatrix(var2S[[2]]) < 3)
    expect_true(rankMatrix(var2S[[3]]) < 3)
    ## Using ginv() non-zero
    expect_true(svyStdDiffMulti("var2", "group", datSvy) > 0)

    ## Third case
    var3S <- pooledSMat(svytable( ~ group + var3, datSvy))
    expect_true(rankMatrix(var3S[[1]]) < 3)
    expect_true(rankMatrix(var3S[[2]]) < 3)
    ## pooled can be full rank if zero cell positions are different
    expect_true(rankMatrix(var3S[[3]]) == 3)
    ## Using true inverse from ginv, non-zero
    expect_true(svyStdDiffMulti("var3", "group", datSvy) > 0)

    ## Third case
    var4S <- pooledSMat(svytable( ~ group + var4, datSvy))
    expect_true(rankMatrix(var4S[[1]]) < 3)
    ## All cells populated and full rank
    expect_true(rankMatrix(var4S[[2]]) == 3)
    ## pooled can be full rank if one group is full rank
    expect_true(rankMatrix(var4S[[3]]) == 3)
    expect_true(svyStdDiffMulti("var4", "group", datSvy) > 0)

})


test_that("zero-cell vectors are decently handled by categorical SMD (real data)", {

    data(nhanes)
    set.seed(2015080200)

    ## grouping is by a binary nhanes$RIAGENDR

    ## (0,0.1,0.2,0.7)^T for both groups: same distribution
    nhanes$var1 <- NA
    nhanes$var1[nhanes$RIAGENDR == 1] <-
    as.vector(t(1:4) %*% rmultinom(n = nrow(subset(nhanes, RIAGENDR == 1)), size = 1, prob = c(0,0.1,0.2,0.7)))
    nhanes$var1[nhanes$RIAGENDR == 2] <-
    as.vector(t(1:4) %*% rmultinom(n = nrow(subset(nhanes, RIAGENDR == 2)), size = 1, prob = c(0,0.1,0.2,0.7)))
    nhanes$var1 <- factor(nhanes$var1, levels = 1:4)

    ## (0,0.1,0.2,0.7)^T vs (0,0.2,0.3,0.5)^T: different distribution, but same zero cell
    nhanes$var2 <- NA
    nhanes$var2[nhanes$RIAGENDR == 1] <-
    as.vector(t(1:4) %*% rmultinom(n = nrow(subset(nhanes, RIAGENDR == 1)), size = 1, prob = c(0,0.1,0.2,0.7)))
    nhanes$var2[nhanes$RIAGENDR == 2] <-
    as.vector(t(1:4) %*% rmultinom(n = nrow(subset(nhanes, RIAGENDR == 2)), size = 1, prob = c(0,0.2,0.3,0.5)))
    nhanes$var2 <- factor(nhanes$var2, levels = 1:4)

    ## (0,0.1,0.2,0.7)^T vs (0.2,0,0.3,0.5)^T: different distribution and zero cell
    nhanes$var3 <- NA
    nhanes$var3[nhanes$RIAGENDR == 1] <-
    as.vector(t(1:4) %*% rmultinom(n = nrow(subset(nhanes, RIAGENDR == 1)), size = 1, prob = c(0,0.1,0.2,0.7)))
    nhanes$var3[nhanes$RIAGENDR == 2] <-
    as.vector(t(1:4) %*% rmultinom(n = nrow(subset(nhanes, RIAGENDR == 2)), size = 1, prob = c(0.2,0,0.3,0.5)))
    nhanes$var3 <- factor(nhanes$var3, levels = 1:4)

    ## (0,0.1,0.2,0.7)^T vs (0.1,0.2,0.3,0.5)^T: different distribution and one is ok
    nhanes$var4 <- NA
    nhanes$var4[nhanes$RIAGENDR == 1] <-
    as.vector(t(1:4) %*% rmultinom(n = nrow(subset(nhanes, RIAGENDR == 1)), size = 1, prob = c(0,0.1,0.2,0.7)))
    nhanes$var4[nhanes$RIAGENDR == 2] <-
    as.vector(t(1:4) %*% rmultinom(n = nrow(subset(nhanes, RIAGENDR == 2)), size = 1, prob = c(0.1,0.2,0.3,0.4)))
    nhanes$var4 <- factor(nhanes$var4, levels = 1:4)

    ## svydesign
    nhanesSvy <- svydesign(ids = ~ SDMVPSU, strata = ~ SDMVSTRA,
                           weights = ~ WTMEC2YR, nest = TRUE,
                           data = nhanes)


    ## Expectations
    ## First case
    var1S <- pooledSMat(table(nhanes$RIAGENDR, nhanes$var1))
    expect_true(rankMatrix(var1S[[1]]) < 3)
    expect_true(rankMatrix(var1S[[2]]) < 3)
    expect_true(rankMatrix(var1S[[3]]) < 3)
    ## Using ginv, near zero due to near-zero numerator
    expect_true(StdDiffMulti(nhanes$var1, nhanes$RIAGENDR) < 0.1)

    ## Second case
    var2S <- pooledSMat(table(nhanes$RIAGENDR, nhanes$var2))
    expect_true(rankMatrix(var2S[[1]]) < 3)
    expect_true(rankMatrix(var2S[[2]]) < 3)
    expect_true(rankMatrix(var2S[[3]]) < 3)
    ## Using ginv() non-zero
    expect_true(StdDiffMulti(nhanes$var2, nhanes$RIAGENDR) > 0)

    ## Third case
    var3S <- pooledSMat(table(nhanes$RIAGENDR, nhanes$var3))
    expect_true(rankMatrix(var3S[[1]]) < 3)
    expect_true(rankMatrix(var3S[[2]]) < 3)
    ## pooled can be full rank if zero cell positions are different
    expect_true(rankMatrix(var3S[[3]]) == 3)
    ## Using true inverse from ginv, non-zero
    expect_true(StdDiffMulti(nhanes$var3, nhanes$RIAGENDR) > 0)

    ## Fourth case
    var4S <- pooledSMat(table(nhanes$RIAGENDR, nhanes$var4))
    expect_true(rankMatrix(var4S[[1]]) < 3)
    ## All cells populated and full rank
    expect_true(rankMatrix(var4S[[2]]) == 3)
    ## pooled can be full rank if one group is full rank
    expect_true(rankMatrix(var4S[[3]]) == 3)
    expect_true(StdDiffMulti(nhanes$var4, nhanes$RIAGENDR) > 0)


    ## Weighted
    ## First case
    var1S <- pooledSMat(svytable( ~ RIAGENDR + var1, nhanesSvy))
    expect_true(rankMatrix(var1S[[1]]) < 3)
    expect_true(rankMatrix(var1S[[2]]) < 3)
    expect_true(rankMatrix(var1S[[3]]) < 3)
    ## Using ginv, near zero due to small numerator vector
    expect_true(svyStdDiffMulti("var1", "RIAGENDR", nhanesSvy) < 0.1)

    ## Second case
    var2S <- pooledSMat(svytable( ~ RIAGENDR + var2, nhanesSvy))
    expect_true(rankMatrix(var2S[[1]]) < 3)
    expect_true(rankMatrix(var2S[[2]]) < 3)
    expect_true(rankMatrix(var2S[[3]]) < 3)
    ## Using ginv() non-zero
    expect_true(svyStdDiffMulti("var2", "RIAGENDR", nhanesSvy) > 0)

    ## Third case
    var3S <- pooledSMat(svytable( ~ RIAGENDR + var3, nhanesSvy))
    expect_true(rankMatrix(var3S[[1]]) < 3)
    expect_true(rankMatrix(var3S[[2]]) < 3)
    ## pooled can be full rank if zero cell positions are different
    expect_true(rankMatrix(var3S[[3]]) == 3)
    ## Using true inverse from ginv, non-zero
    expect_true(svyStdDiffMulti("var3", "RIAGENDR", nhanesSvy) > 0)

    ## Third case
    var4S <- pooledSMat(svytable( ~ RIAGENDR + var4, nhanesSvy))
    expect_true(rankMatrix(var4S[[1]]) < 3)
    ## All cells populated and full rank
    expect_true(rankMatrix(var4S[[2]]) == 3)
    ## pooled can be full rank if one RIAGENDR is full rank
    expect_true(rankMatrix(var4S[[3]]) == 3)
    expect_true(svyStdDiffMulti("var4", "RIAGENDR", nhanesSvy) > 0)

})


### Tests working on multiple variables
################################################################################

test_that("multiple variables can be looped", {


    contVars  <- c("WTMEC2YR", "HI_CHOL", "race", "RIAGENDR")
    catVars <-   c("HI_CHOL", "race", "agecat", "RIAGENDR")


    ## Two groups
    expect_equal(as.numeric(sapply(contVars, function(var) {
        StdDiff(variable = nhanes[,var], group = nhanes[,"RIAGENDR"])
    })),
    c(StdDiff(variable = nhanes[,contVars[1]], group = nhanes[,"RIAGENDR"]),
      StdDiff(variable = nhanes[,contVars[2]], group = nhanes[,"RIAGENDR"]),
      StdDiff(variable = nhanes[,contVars[3]], group = nhanes[,"RIAGENDR"]),
      StdDiff(variable = nhanes[,contVars[4]], group = nhanes[,"RIAGENDR"])))

    expect_equal(as.numeric(sapply(catVars, function(var) {
        StdDiffMulti(variable = nhanes[,var], group = nhanes[,"RIAGENDR"])
    })),
    c(StdDiffMulti(variable = nhanes[,catVars[1]], group = nhanes[,"RIAGENDR"]),
      StdDiffMulti(variable = nhanes[,catVars[2]], group = nhanes[,"RIAGENDR"]),
      StdDiffMulti(variable = nhanes[,catVars[3]], group = nhanes[,"RIAGENDR"]),
      StdDiffMulti(variable = nhanes[,catVars[4]], group = nhanes[,"RIAGENDR"])))

    ## Four groups
    expect_equal(lapply(contVars, function(var) {
        StdDiff(variable = nhanes[,var], group = nhanes[,"race"])
    }),
    list(StdDiff(variable = nhanes[,contVars[1]], group = nhanes[,"race"]),
         StdDiff(variable = nhanes[,contVars[2]], group = nhanes[,"race"]),
         StdDiff(variable = nhanes[,contVars[3]], group = nhanes[,"race"]),
         StdDiff(variable = nhanes[,contVars[4]], group = nhanes[,"race"])))

    expect_equal(lapply(catVars, function(var) {
        StdDiffMulti(variable = nhanes[,var], group = nhanes[,"race"])
    }),
    list(StdDiffMulti(variable = nhanes[,catVars[1]], group = nhanes[,"race"]),
      StdDiffMulti(variable = nhanes[,catVars[2]], group = nhanes[,"race"]),
      StdDiffMulti(variable = nhanes[,catVars[3]], group = nhanes[,"race"]),
      StdDiffMulti(variable = nhanes[,catVars[4]], group = nhanes[,"race"])))


    ## Two groups
    expect_equal(as.numeric(sapply(contVars, function(var) {
        svyStdDiff(var, "RIAGENDR", nhanesSvy)
    })),
    c(svyStdDiff(contVars[1], "RIAGENDR", nhanesSvy),
      svyStdDiff(contVars[2], "RIAGENDR", nhanesSvy),
      svyStdDiff(contVars[3], "RIAGENDR", nhanesSvy),
      svyStdDiff(contVars[4], "RIAGENDR", nhanesSvy)))

    expect_equal(as.numeric(sapply(catVars, function(var) {
        svyStdDiffMulti(var, "RIAGENDR", nhanesSvy)
    })),
    c(svyStdDiffMulti(catVars[1], "RIAGENDR", nhanesSvy),
      svyStdDiffMulti(catVars[2], "RIAGENDR", nhanesSvy),
      svyStdDiffMulti(catVars[3], "RIAGENDR", nhanesSvy),
      svyStdDiffMulti(catVars[4], "RIAGENDR", nhanesSvy)))

    ## Four groups
    expect_equal(lapply(contVars, function(var) {
        svyStdDiff(var, "race", nhanesSvy)
    }),
    list(svyStdDiff(contVars[1], "race", nhanesSvy),
         svyStdDiff(contVars[2], "race", nhanesSvy),
         svyStdDiff(contVars[3], "race", nhanesSvy),
         svyStdDiff(contVars[4], "race", nhanesSvy)))

    expect_equal(lapply(catVars, function(var) {
        svyStdDiffMulti(var, "race", nhanesSvy)
    }),
    list(svyStdDiffMulti(catVars[1], "race", nhanesSvy),
      svyStdDiffMulti(catVars[2], "race", nhanesSvy),
      svyStdDiffMulti(catVars[3], "race", nhanesSvy),
      svyStdDiffMulti(catVars[4], "race", nhanesSvy)))

})


### Older tests with simpler data
################################################################################

test_that("binary standardized difference is correct", {

    ## Prepare data
    variable <- c(1,0,0,0,0, 1,1,0,0,0, 1,1,1,0,0)
    group    <- rep(1:3, each = 5)

    data1 <- data.frame(variable = variable, group = group,
                        weight = rep(0.5, 15))
    data1Svy <- svydesign(ids = ~ 1, data = data1, weights = ~ weight)

    ## Gold standard
    props <- c(0.2, 0.4, 0.6)
    vars  <- c(0.2*0.8, 0.4*0.6, 0.6*0.4)

    g1g2 <- abs(props[1] - props[2]) / sqrt((vars[1] + vars[2]) / 2)
    g2g3 <- abs(props[2] - props[3]) / sqrt((vars[2] + vars[3]) / 2)
    g3g1 <- abs(props[3] - props[1]) / sqrt((vars[3] + vars[1]) / 2)


    ## Expectations
    ## Binary case
    expect_equal(StdDiff(variable, group, binary = TRUE),
                 c(g1g2, g3g1, g2g3))
    ## Continuous case
    expect_equal(StdDiff(variable, group, binary = FALSE),
                 as.vector(StdDiffOld(variable, group)))

    ## Weighted
    ## Binary case
    expect_equal(svyStdDiff("variable","group", design = data1Svy, binary = TRUE),
                 c(g1g2, g3g1, g2g3))
    ## Continuous case
    expect_equal(svyStdDiff("variable","group", design = data1Svy, binary = FALSE),
                 as.vector(svyStdDiffOld("variable","group", design = data1Svy)))


    ## Multiple variables
    expect_equal(StdDiffs(data = data1, vars = c("variable","variable"),
                          groupVar = "group", binaryVars = "variable"),
                 list(c(g1g2, g3g1, g2g3), c(g1g2, g3g1, g2g3)))

    expect_equal(StdDiffs(data = data1, vars = c("variable","variable"),
                          groupVar = "group", binaryVars = NA),
                 list(as.vector(StdDiffOld(variable, group)),
                      as.vector(StdDiffOld(variable, group))))

    expect_equal(svyStdDiffs(data = data1Svy, vars = c("variable","variable"),
                          groupVar = "group", binaryVars = "variable"),
                 list(c(g1g2, g3g1, g2g3), c(g1g2, g3g1, g2g3)))

    expect_equal(svyStdDiffs(data = data1Svy, vars = c("variable","variable"),
                          groupVar = "group", binaryVars = NA),
                 list(as.vector(svyStdDiffOld("variable","group", design = data1Svy)),
                      as.vector(svyStdDiffOld("variable","group", design = data1Svy))))
})

test_that("Multinomial SMD is correct", {

    set.seed(102)
    ## 4-category variable
    probs <- c(0.1,0.3,0.5,0.2)
    X <- rmultinom(n = 100, size = 1, prob = probs)
    X <- as.vector(seq_along(probs) %*% X)
    ## Three groups
    group <- c(rep(1,20), rep(2,30), rep(3,50))

    ## Drop first column to avoid dependent column
    ## The result does not change if generalized inverse is used
    dummyX <- dummies::dummy(X)[, -1, drop = FALSE]

    lstDummyX <- split(x = as.data.frame(dummyX), f = group, drop = FALSE)

    ## Means for each indicator
    means <- lapply(lstDummyX, MultinomialMeans)
    ## cov mat for each indicators
    covs  <- lapply(means, MultinomialVar)

    meanDiff1 <- means[[1]] - means[[2]]
    meanDiff2 <- means[[1]] - means[[3]]
    meanDiff3 <- means[[2]] - means[[3]]

    covMean1 <- (covs[[1]] + covs[[2]]) / 2
    covMean2 <- (covs[[1]] + covs[[3]]) / 2
    covMean3 <- (covs[[2]] + covs[[3]]) / 2

    smd1 <- drop(sqrt(t(meanDiff1) %*% MASS::ginv(covMean1) %*% t(t(meanDiff1))))
    smd2 <- drop(sqrt(t(meanDiff2) %*% MASS::ginv(covMean2) %*% t(t(meanDiff2))))
    smd3 <- drop(sqrt(t(meanDiff3) %*% MASS::ginv(covMean3) %*% t(t(meanDiff3))))

    ## Calculate using multi-category to SMD function
    outSmd <- StdDiffMulti(variable = X, group = group)

    expect_equal(length(outSmd), 3)
    expect_equal(outSmd, c(smd1,smd2,smd3))


    ##
    ## Repeat for dummy variable matrix with all columns (k-1 independent)
    dummyX <- dummies::dummy(X)
    lstDummyX <- split(x = as.data.frame(dummyX), f = group, drop = FALSE)
    ## Means for each indicator
    means <- lapply(lstDummyX, MultinomialMeans)
    ## cov mat for each indicators
    covs  <- lapply(means, MultinomialVar)
    meanDiff1 <- means[[1]] - means[[2]]
    meanDiff2 <- means[[1]] - means[[3]]
    meanDiff3 <- means[[2]] - means[[3]]
    covMean1 <- (covs[[1]] + covs[[2]]) / 2
    covMean2 <- (covs[[1]] + covs[[3]]) / 2
    covMean3 <- (covs[[2]] + covs[[3]]) / 2
    smd1 <- drop(sqrt(t(meanDiff1) %*% MASS::ginv(covMean1) %*% t(t(meanDiff1))))
    smd2 <- drop(sqrt(t(meanDiff2) %*% MASS::ginv(covMean2) %*% t(t(meanDiff2))))
    smd3 <- drop(sqrt(t(meanDiff3) %*% MASS::ginv(covMean3) %*% t(t(meanDiff3))))

    expect_equal(outSmd, c(smd1,smd2,smd3))


    ## Check behavior with a binary variable
    set.seed(102)
    ## Binary variable
    X <- rbinom(n = 100, size = 1, prob = 0.2)
    group <- rep(c(0,1), c(20,80))

    dummyX <- dummies::dummy(X)[, -1, drop = FALSE]

    lstDummyX <- split(x = as.data.frame(dummyX), f = group, drop = FALSE)

    ## Means for each indicator
    means <- lapply(lstDummyX, MultinomialMeans)
    ## cov mat for each indicators
    covs  <- lapply(means, MultinomialVar)
    ##
    meanDiff1 <- means[[1]] - means[[2]]
    covMean1 <- (covs[[1]] + covs[[2]]) / 2

    smd1 <- drop(sqrt(t(meanDiff1) %*% MASS::ginv(covMean1) %*% t(t(meanDiff1))))

    ## Comparison to manual calculation
    expect_equal(StdDiffMulti(variable = X, group = group), smd1)
    ## Comparison to continuous version with binary support
    expect_equal(StdDiffMulti(variable = X, group = group),
                 StdDiff(variable = X, group = group, binary = TRUE))

    ## Check missing value handling (binary)
    X[13] <- NA
    ## within group values
    means <- tapply(X, group, mean, na.rm = TRUE)
    covs  <- means * (1 - means)
    ## Diff and cov mean
    meanDiff1 <- means[[1]] - means[[2]]
    covMean1  <- (covs[[1]] + covs[[2]]) / 2
    smd1 <- drop(sqrt(t(meanDiff1) %*% MASS::ginv(covMean1) %*% t(t(meanDiff1))))
    smd2 <- meanDiff1 / sqrt(covMean1)

    expect_equal(smd1, smd2)

    expect_equal(StdDiff(variable = X, group = group, binary = TRUE), smd1)
    expect_equal(StdDiff(variable = X, group = group, binary = TRUE), smd2)

    expect_equal(StdDiffMulti(variable = X, group = group),
                 StdDiff(variable = X, group = group, binary = TRUE))

    expect_equal(StdDiffMulti(variable = X, group = group),
                 StdDiff(variable = X, group = group, binary = TRUE))

})


test_that("mutlinomial SMD for survey data is correct", {

    set.seed(102)
    ## 4-category variable
    probs <- c(0.1,0.3,0.5,0.2)
    X <- rmultinom(n = 100, size = 1, prob = probs)
    X <- as.vector(seq_along(probs) %*% X)
    ## Three groups
    group <- c(rep(1,20), rep(2,30), rep(3,50))

    dat <- data.frame(X = X, group = group, weight = 1)
    datSvy <- svydesign(ids = ~ 1, data = dat, weights = ~ weight)

    ## Check tables match with weight of 1 for everyone
    tab1svytable <- svytable(~ group + X, design = datSvy)
    tab1xtabs <- xtabs(~ group + X, data = dat)
    expect_true(all(tab1svytable == tab1xtabs))

    ## Check equality for equal weight case
    expect_equal(svyStdDiffMulti("X", "group", datSvy),
                 StdDiffMulti(dat$X, group = dat$group))

    ## Missing value is introduced
    dat$X[13] <- NA
    ## Recreate
    datSvy <- svydesign(ids = ~ 1, data = dat, weights = ~ weight)
    ## Tables again
    tab1svytable <- svytable(~ group + X, design = datSvy)
    tab1xtabs <- xtabs(~ group + X, data = dat)

    expect_true(all(tab1svytable == tab1xtabs))


    expect_equal(svyStdDiffMulti("X", "group", datSvy),
                 StdDiffMulti(dat$X, group = dat$group))

})



###
### User level functionality check
################################################################################

test_that("SMDs are correctly shown in print()", {

    vars   <- c("race","agecat")
    strata <- c("HI_CHOL","RIAGENDR")

    ## Create an interaction variable
    nhanes$strataVar <- interaction(nhanes$HI_CHOL, nhanes$RIAGENDR, sep = ":")
    ## Create an weighted data
    nhanesSvy <- svydesign(ids = ~ SDMVPSU, strata = ~ SDMVSTRA, weights = ~ WTMEC2YR,
                           nest = TRUE, data = nhanes)


    ## Unweighed version
    ## Create a table
    tab1 <- CreateTableOne(vars = vars, strata = strata, data = nhanes)

    ## Show summary including
    summary(tab1)
    expect_output(summary(tab1), "Standardize mean differences")

    ## First variable changes fastest, and its consistent with by()'s order
    expect_equal(levels(nhanes$strataVar),
                 c("0:1", "1:1", "0:2", "1:2"))
    expect_equal(levels(nhanes$strataVar),
                 colnames(print(tab1$ContTable))[1:4])

    ## Check ordering is correct (continuous)
    expect_equal(as.vector(attr(tab1$ContTable, "smd"))[-1],
                 StdDiff(nhanes$race, nhanes$strataVar))

   ## Check ordering is correct (categorical)
    expect_equal(as.vector(attr(tab1$CatTable, "smd"))[-1],
                 StdDiffMulti(nhanes$agecat, nhanes$strataVar))

    out1 <- print(tab1, smd = TRUE)
    expect_equal(as.vector(out1[,"SMD"][2:3]),
                 c(sprintf(" %.3f", attr(tab1$ContTable, "smd")[1,1]),
                   sprintf(" %.3f", attr(tab1$CatTable, "smd")[1,1])))

})
