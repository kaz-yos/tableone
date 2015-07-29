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
## A unified approach to measuring the effect size between two groups using SAS®
## http://support.sas.com/resources/papers/proceedings12/335-2012.pdf


###
### Prepare environment
################################################################################
library(testthat)
library(survey)


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



### Old functions explicitly for 3 groups
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
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2)

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
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2,
         (MultinomialVar(propTable2[1,]) + MultinomialVar(propTable2[2,])) / 2)

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
