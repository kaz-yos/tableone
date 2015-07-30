################################################################################
### Modules for survey data support
##
## Created on: 2015-07-23
## Author: Kazuki Yoshida
################################################################################


###
### Generic helpers
################################################################################

## Function to return 1 as many as x elements
## Used for counting weighted sample size
one <- function(x) {
    rep(1, length(x))
}


## 0,1 indicator variable for missingness
miss <- function(x) {
    as.numeric(is.na(x))
}


## Return formula string having vars on RHS
FormulaString <- function(vars) {
    paste0(" ~ ", paste0(vars, collapse = " + "))
}


###
### Helpers for both types of data
################################################################################

## Check for survey data; fail if not
StopIfNotSurveyDesign <- function(data) {

    if (!("survey.design2" %in% class(data))) {
        stop("The data argument needs to be a survey design object.")
    }

}


###
### Helpers for continuous variable summary
################################################################################

svyN <- function(vars, design) {
    ## Same for all variables, just use first
    form <- sprintf(" ~ one(%s)", vars[1])
    ## No missing handling as all values are 1
    res <- svytotal(x = as.formula(form), design = design)
    ## Repeat for all variables
    out <- rep(res[1], length(vars))
    names(out) <- vars
    out
}


svyMiss <- function(vars, design) {

    ## Rewrite variables locally to missing 0,1 indicators
    design$variables[vars] <- lapply(design$variables[vars], miss)

    ## Same for all variables, just use first
    form <- FormulaString(vars)

    res <- svytotal(x = as.formula(form), design = design)
    out <- as.vector(res)
    names(out) <- vars
    out
}


svyPMiss <- function(vars, design) {
    svyMiss(vars, design) / svyN(vars, design)
}


svyMean <- function(vars, design) {
    ## Same for all variables, just use first
    form <- FormulaString(vars)
    ## Remove missingness and mean
    ## Bad behavior, but consistent with the unweighted version
    res <- svymean(x = as.formula(form), design = design, na.rm = TRUE)
    out <- as.vector(res)
    names(out) <- vars
    out
}


svySd <- function(vars, design) {
    ## Same for all variables, just use first
    form <- FormulaString(vars)
    ## Remove missingness and var
    ## Bad behavior, but consistent with the unweighted version
    res <- svyvar(x = as.formula(form), design = design, na.rm = TRUE)


    ## Diagnonal elements are variances given 2+ variables
    if (length(vars) == 1) {

        out <- as.vector(sqrt(res))

    } else if (length(vars) > 1) {

        ## Matrix if vars is of length 2+
        out <- sqrt(diag(res))
    }

    names(out) <- vars
    out
}


svyQuant <- function(vars, design, q = 0.5) {
    ## Same for all variables, just use first
    form <- FormulaString(vars)
    ## Remove missingness and mean
    ## Bad behavior, but consistent with the unweighted version
    ## Use only one quantile
    res <- svyquantile(x = as.formula(form), quantiles = q[1], design = design, na.rm = TRUE)
    out <- as.vector(res)
    names(out) <- vars
    out
}


svyContSummary <- function(vars, design) {

    ## Save for reuse
    nVec    <- svyN(vars, design)
    missVec <- svyMiss(vars, design)

    cbind(n      = nVec,
          miss   = missVec,
          p.miss = missVec / nVec * 100,
          mean   = svyMean(vars, design),
          sd     = svySd(vars, design),
          median = svyQuant(vars, design, q = 0.5),
          p25    = svyQuant(vars, design, q = 0.25),
          p75    = svyQuant(vars, design, q = 0.75),
          min    = svyQuant(vars, design, q = 0),
          max    = svyQuant(vars, design, q = 1))
}


###
### Helpers for categorical variable summary
################################################################################
## These work on one variable at a time

svyTable <- function(var, design) {
    form <- FormulaString(var)
    out <- svytable(formula = as.formula(form), design = design)
    out
}


svyLevel <- function(var, design) {
    names(svyTable(var, design))
}


svyPropTable <- function(var, design) {
    prop.table(svyTable(var, design))
}


## This one works at a single variable level within a stratum
svyCatSummaryForOneVar <- function(var, design) {

    ## Tables
    freqTab <- svyTable(var, design)
    propTab <- prop.table(freqTab)
    nLevels <- length(freqTab)
    ## Repeat as many as the levels
    nVec    <- rep(svyN(var, design), nLevels)
    missVec <- rep(svyMiss(var, design), nLevels)

    data.frame(n           = nVec,
               miss        = missVec,
               p.miss      = missVec / nVec * 100,
               level       = names(freqTab),
               freq        = as.vector(freqTab),
               percent     = as.vector(propTab) * 100,
               cum.percent = cumsum(propTab) * 100,
               ## To protect against, level having <NA>
               row.names   = NULL)
}


## This one can take multiple variables and return a list
svyCatSummary <- function(vars, design) {

    sapply(vars, function(var) {

        svyCatSummaryForOneVar(var, design)

    }, simplify = FALSE)
}


###
### Helpers for testing for p values
################################################################################

## Function to do Wald test on a multi-degree variable after linear regression
svyGlmTermTest <- function(formula, design, test.terms, method = "Wald") {

    ## Perform linear regression and perform
    regTermTest(svyglm(formula, design), test.terms = test.terms, method = method)
}


## Given a formula string as its first argument, calls svyGlmTermTest correctly
svyTestNormal <- function(formulaString, design, test.terms, method = "Wald") {

    out <- svyGlmTermTest(formula = as.formula(formulaString), design = design,
                          test.terms = test.terms, method = method)
    ## Give an appropriate name for consistent extraction
    list(p.value = out$p[1,1])
}


## Kruskal-Wallis test
svyTestNonNormal <- function(formulaString, design) {

    ## This returns an htest object that has a scalar $p.value element
    svyranktest(formula = as.formula(formulaString), design = design)
}


svyTestChisq <- function(formulaString, design) {

    ## This returns an htest object that has a scalar $p.value element
    svychisq(formula = as.formula(formulaString), design = design)
}


svyModuleApproxExactTests <- function(data, vars, strataVarName,
                                      testApprox, argsApprox) {

    ## Loop over variables and create a list of xtabs
    ## Empty strata are kept in the corss tables. Different behavior than the cont counterpart!
    listXtabs <- sapply(X = vars,
                        FUN = function(var) {
                            ## Create a formula
                            formula <- as.formula(paste0("~ ", var, " + ", "..strataVar.."))

                            ## Create a 2-dimensional crosstable
                            svytable(formula = formula, design = data)
                        },
                        simplify = FALSE)

    ## Rename the second dimension of the xtabs with the newly create name.
    for (i in seq_along(listXtabs)) {

        names(dimnames(listXtabs[[i]]))[2] <- strataVarName
    }

    ## Loop over variables, and create p-values
    pValues <-
    sapply(X = vars,
           FUN = function(var) {
               formulaString <- paste0(" ~ ", var, " + ..strataVar..")
               ## Perform tests and return the result as 1x2 DF
               data.frame(pApprox = ModuleTestSafe(formulaString, testApprox,
                                                   c(list(design = data), argsApprox)),
                          pExact  = NA) # Not available for survey data. Just fill in with NA
           },
           simplify = FALSE)

    ## Create a single data frame (n x 2 (normal,nonormal))
    pValues <- do.call(rbind, pValues)

    ## Return both xtabs and p value df
    list(pValues = pValues, xtabs = listXtabs)
}
