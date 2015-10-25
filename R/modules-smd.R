################################################################################
### Modules for standardized mean difference support
##
## Created on: 2015-07-29
## Author: Kazuki Yoshida
################################################################################

###
### References
################################################################################
## A unified approach to measuring the effect size between two groups using SAS
## http://support.sas.com/resources/papers/proceedings12/335-2012.pdf
## R multinomial distribution variance
## http://stackoverflow.com/questions/19960605/r-multinomial-distribution-variance


###
### Helpers common to both unweighted and weighed functions
################################################################################

## Dummy matrix to means (proportions for each dummy)
MultinomialMeans <- function(dummyMat, na.rm = TRUE) {
    colMeans(dummyMat, na.rm = na.rm)
}

## Proportions to variance-covariance matrix
## http://stackoverflow.com/questions/19960605/r-multinomial-distribution-variance
## diagonal p_k(1-p_k); off-diagonal -1 * p_k * p_l
MultinomialVar <- function(multinomialMeans) {
    ## Mean for each
    p <- multinomialMeans
    ## Diagonal elements are p_i(1-p_i)
    vars <- p * (1 - p)
    ## Off-diagonal elements are - p_i p_j where i != j
    covs <- - outer(p, p)
    ## Put diagonal elements
    diag(covs) <- vars
    ## Reduce to a scalar if 1x1
    drop(covs)
}

## List of mean vector of a multinomial variable to categorical SMD (Yang & Dalton 2012)
StdDiffFromLstMeans <- function(lstMeans) {

    ## list of variance-covariance matrices
    ## http://stackoverflow.com/questions/19960605/r-multinomial-distribution-variance
    ## diagonal p_k(1-p_k); off-diagonal -1 * p_k * p_l
    lstCovs <- lapply(lstMeans, MultinomialVar)

    ## All possible mean vector differences
    lstMeanDiffs <- lapply(lstMeans, function(x) {
        lapply(lstMeans, function(y) {
            x - y
        })
    })

    ## All possible covariance matrix means
    lstCovMeans <- lapply(lstCovs, function(x) {
        lapply(lstCovs, function(y) {
            (x + y) / 2
        })
    })

    ## Initialize a numeric vector object for capturing values
    sqSmds <- vector(mode = "numeric")

    ## Add upper triangle elements (i < j) to output list
    for (i in seq_along(lstMeans)) {
        for (j in seq_along(lstMeans)) {
            if (i < j) {
                ## For upper triangle elements only
                ## Squared Mahalanobis distance
                ## meanDiffs^T (pooled vcov)^-1 meanDiffs
                ## Generalized inverse for protection against singularity
                ## Reduces to true inverse if non-singular

                ## Column mean difference vector
                T_C <- t(t(lstMeanDiffs[[i]][[j]]))
                ## Pooled vcov
                S   <- lstCovMeans[[i]][[j]]

                if (all(S[!is.na(S)] %in% 0)) {
                    ## If S is a zero matrix, ginv is a zero matrix
                    ## which gives a zero SMD regardless of mean
                    ## difference. Such a case should be NaN.
                    ## NOTE: NA's are dropped first. Non-NA elements
                    ## are assessed for 0's. If all remaining
                    ## are zeros or no element remained (all NA),
                    ## all() returns TRUE, and sqMD is forced to NaN.
                    sqMD <- NaN
                } else {
                    ## Squared Mahalanobis distance
                    sqMD <- t(T_C) %*% MASS::ginv(S) %*% T_C
                }

                ## Add sqrt of MD to output
                ## Not efficient; room for improvement
                sqSmds <- c(sqSmds, sqMD)
            }
        }
    }
    ## We want it on the original scale
    sqrt(sqSmds)
}


## Given strata x levels of variable table
## Return list of proportions of levels dropping redundant first level
LstMeansFromFullTable <- function(strataByLevels) {
    ## Proportion within each stratum
    ## Equivalent to mean of dummy variables
    propTables <- prop.table(strataByLevels, margin = 1)

    ## Drop first level to eliminate dependence if more than two levels
    ## Avoids errors with constant variable
    if (ncol(propTables) > 1) {
        propTables <- propTables[, -1, drop = FALSE]
    }

    ## list of mean vectors (Missing values are discarded)
    lstMeans <- lapply(seq_len(nrow(propTables)), function(i) {

        propTables[i,]
    })
    lstMeans
}


## Create a matrix form
FormatLstSmds <- function(lstSmds, nStrata) {

    if (nStrata < 2) {
        warning("nStrata has to be >= 2.")
    }

    ## matrix form
    matSmds <- do.call(rbind, lstSmds)

    ## Create contrast names
    allCombns <- t(combn(nStrata, 2))
    combnNames <- apply(allCombns, MARGIN = 1, FUN = paste, collapse = " vs ")
    colnames(matSmds) <- combnNames

    ## Add a mean column if more than two columns
    if (ncol(matSmds) > 1) {
        matSmds <- cbind(average = rowMeans(matSmds),
                         matSmds)
    }

    matSmds
}


###
### Functions for unweighted data only
################################################################################

### Check strata for NA-only strata
CheckNaOnlyStrata <- function(variable, group) {

    unlist(lapply(split(variable, group),
                  function(var) {
                      ## TRUE if only NA's within stratum
                      all(is.na(var))
                  }))
}


### Continuous/binary standardized mean differences
## Expects continuous or 0,1 binary variable
StdDiff <- function(variable, group, binary = FALSE, na.rm = TRUE) {

    ## Check strata for all NA strata
    logiAllNaStrata <- CheckNaOnlyStrata(variable, group)
    ## If ANY strata have only NA's do not remove NA's
    if (any(logiAllNaStrata)) {
        warning("Variable has only NA's in at least one stratum. na.rm turned off.")
        na.rm = FALSE
    }

    ## Proportion of 1 is the mean of variable
    means <- tapply(variable, group, mean, na.rm = na.rm)

    ## Variance is p(1-p)
    if (binary) {
        vars  <- means * (1 - means)
    } else {
        vars  <- tapply(variable, group, var, na.rm = na.rm)
    }

    ## Outer to obtain all pairwise differences
    meanDiffs  <- outer(X = means, Y = means, FUN = "-")
    ## Outer to obtain all pairwise variance mean
    varMeans   <- outer(X = vars, Y = vars, FUN = "+") / 2

    out <- meanDiffs / sqrt(varMeans)

    ## This lower.tri() approach is actually giving 2vs1, 3vs1, etc
    ## opposite of stated 1vs2, 1vs3. Only correct if abs() is used.
    abs(out[lower.tri(out)])
}

### Categorical (including binary) standardizzed mean difference
StdDiffMulti <- function(variable, group) {

    ## Check strata for all NA strata
    logiAllNaStrata <- CheckNaOnlyStrata(variable, group)

    ## If ALL strata have only NA's convert to a level
    ## If any strata have valid levels, table is ok
    ## Empty table breaks the function
    if (all(logiAllNaStrata)) {

        warning("Variable has only NA's in all strata. Regarding NA as a level.")

        variable <- factor(variable, exclude = NULL)
    }

    ## strata x variable table
    strataByLevels <- table(group, variable)
    lstMeans       <- LstMeansFromFullTable(strataByLevels)
    ## Return vector of SMDs
    StdDiffFromLstMeans(lstMeans)
}


### Standardized mean differences for multiple variables
## Continuous or binary only
StdDiffs <- function(data, vars, groupVar, binaryVars) {

    lapply(vars, function(var) {

        StdDiff(variable = data[,var], group = data[,groupVar],
                binary = (var %in% binaryVars))
    })
}


###
### Functions for weighted data only
################################################################################

### Check strata for NA-only strata
svyCheckNaOnlyStrata <- function(varName, groupName, design) {

    unlist(lapply(split(design$variables[,varName],
                        design$variables[,groupName]),
                  function(var) {
                      ## TRUE if only NA's within stratum
                      all(is.na(var))
                  }))
}

### Continuous/binary standardized mean differences
## Expects continuous or 0,1 binary variable
svyStdDiff <- function(varName, groupName, design, binary = FALSE, na.rm = TRUE) {

    ## Check strata for all NA strata
    logiAllNaStrata <- svyCheckNaOnlyStrata(varName, groupName, design)
    ## If ANY strata have only NA's do not remove NA's
    if (any(logiAllNaStrata)) {
        warning(varName, " has only NA's in at least one stratum. na.rm turned off.")
        na.rm = FALSE
    }

    varFormula   <- as.formula(paste("~", varName))
    groupFormula <- as.formula(paste("~", groupName))

    means <- svyby(formula = varFormula, by = groupFormula,
                   FUN = svymean, design = design, na.rm = na.rm)[,2]

    if (binary) {
        vars  <- means * (1 - means)
    } else {
        vars  <- svyby(formula = varFormula, by = groupFormula,
                       FUN = svyvar,  design = design, na.rm = na.rm)[,2]
    }

    ## Outer to obtain all pairwise differences
    meanDiffs <- outer(X = means, Y = means, FUN = "-")
    ## Outer to obtain all pairwise variance mean
    varMeans  <- outer(X = vars, Y = vars, FUN = "+") / 2

    out <- meanDiffs / sqrt(varMeans)

    ## This lower.tri() approach is actually giving 2vs1, 3vs1, etc
    ## opposite of stated 1vs2, 1vs3. Only correct if abs() is used.
    abs(out[lower.tri(out)])
}


### Categorical (including binary) standardizzed mean difference
svyStdDiffMulti <- function(varName, groupName, design) {

    ## Check strata for all NA strata
    logiAllNaStrata <- svyCheckNaOnlyStrata(varName, groupName, design)

    ## If ALL strata have only NA's convert to a level
    ## If any strata have valid levels, table is ok
    ## Empty table breaks the function
    if (all(logiAllNaStrata)) {

        warning(varName, " has only NA's in all strata. Regarding NA as a level.")

        design$variables[,varName] <- factor(design$variables[,varName],
                                             exclude = NULL)
    }

    ## If same variable is used as a group, factor() to allow tabling
    if (varName == groupName) {
        groupName <- sprintf("factor(%s)", groupName)
    }

    tabFormula   <- as.formula(sprintf("~ %s + %s", groupName, varName))

    ## strata x variable table
    strataByLevels <- svytable(formula = tabFormula, design = design)
    lstMeans       <- LstMeansFromFullTable(strataByLevels)
    ## Return vector of SMDs
    StdDiffFromLstMeans(lstMeans)
}


### Standardized mean differences for multiple variables
## Continuous or binary only
svyStdDiffs <- function(data, vars, groupVar, binaryVars) {

    lapply(vars, function(var) {

        svyStdDiff(varName = var, groupName = groupVar, design = data,
                   binary = (var %in% binaryVars))
    })
}
