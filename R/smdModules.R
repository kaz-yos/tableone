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

##
StdDiffFromLstMeans <- function(lstMeans) {

    ## list of covariance matrices
    lstCovs  <- lapply(lstMeans,   MultinomialVar)

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
                ## meanDiffs^T covMean^-1 meanDiffs
                ## Generalized inverse for protection against singularity
                ## Reduces to true inverse if non-singular
                sqMahaDist <-
                t(lstMeanDiffs[[i]][[j]]) %*%
                MASS::ginv(lstCovMeans[[i]][[j]]) %*%
                t(t(lstMeanDiffs[[i]][[j]]))
                ## Add sqrt of MD to output
                ## Not efficient; room for improvement
                sqSmds <- c(sqSmds, sqMahaDist)
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





###
### Functions for unweighted data only
################################################################################

### Continuous/binary standardized mean differences
## Expects continuous or 0,1 binary variable
StdDiff <- function(variable, group, binary = FALSE, na.rm = TRUE) {

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

    abs(out[lower.tri(out)])
}

### Categorical (including binary) standardizzed mean difference
StdDiffMulti <- function(variable, group) {

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

### Continuous/binary standardized mean differences
## Expects continuous or 0,1 binary variable
svyStdDiff <- function(varName, groupName, design, binary = FALSE, na.rm = TRUE) {

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

    abs(out[lower.tri(out)])
}


### Categorical (including binary) standardizzed mean difference
svyStdDiffMulti <- function(varName, groupName, design) {

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
