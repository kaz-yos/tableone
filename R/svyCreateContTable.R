##' Create an object summarizing continous variables for weighted dataset
##'
##' Create an object summarizing continous variables optionally stratifying by one or more startifying variables and performing statistical tests. Usually, \code{\link{svyCreateTableOne}} should be used as the universal frontend for both continuous and categorical data.
##'
##' @param vars Variable(s) to be summarized given as a character vector.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A survey design object in which these variables exist. All variables (both vars and strata) must be in this survey design object. It is created with the \code{svydesign} function in the \code{survey} package.
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed. Both tests that assume normality and tests that do not are performed. Either one of the result can be obtained from the print method.
##' @param testNormal A function used to perform the normal assumption based tests. The default is multiple degrees of freedom test using \code{svyglm} and \code{regTermTest}. This is equivalent of the \code{svyttest} when there are only two groups.
##' @param argsNormal A named list of arguments passed to the function specified in \code{testNormal}.
##' @param testNonNormal A function used to perform the nonparametric tests. The default is \code{svyranktest}.
##' @param argsNonNormal A named list of arguments passed to the function specified in \code{testNonNormal}.
##' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated.
##' @return An object of class \code{svyContTable}.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{svyCreateTableOne}}, \code{\link{print.svyContTable}}, \code{\link{summary.svyContTable}},
##' @examples
##'
##' ## See the examples for svyCreateTableOne()
##'
##' @export
svyCreateContTable <-
function(vars,                                  # character vector of variable names
         strata,                                # character vector of variable names
         data,                                  # survey design data
         test          = TRUE,                  # Whether to include p-values
         testNormal    = svyTestNormal,         # test for normally distributed variables
         argsNormal    = list(method = "Wald"), # arguments passed to testNormal
         testNonNormal = svyTestNonNormal,      # test for nonnormally distributed variables
         argsNonNormal = NULL,                  # arguments passed to testNonNormal
         smd           = TRUE                   # whether to include standardize mean differences
         ) {

### Data check
    ## Check if the data given is a survey design object
    StopIfNotSurveyDesign(data)

    ## Check if variables exist. Drop them if not.
    ## survey.design$variables holds original data frame
    vars <- ModuleReturnVarsExist(vars, data$variables)

    ## Abort if no variables exist at this point
    ModuleStopIfNoVarsLeft(vars)

    ## Toggle test FALSE if no strata
    test <- ModuleReturnFalseIfNoStrata(strata, test)
    smd  <- ModuleReturnFalseIfNoStrata(strata, smd)

    ## Create strata data frame (data frame with only strata variables)
    ## FIXME: This changes type of strata; not a good practice
    strata <- ModuleReturnStrata(strata, data$variables)

    ## Create a single stratification variable
    ## Keeps non-existing levels
    data$variables$..strataVar.. <- interaction(strata, sep = ":")
    strataVarLevels <- levels(data$variables$..strataVar..)
    ## Dummy and dumb object to avoid CRAN check "no visible binding for global variable"
    ..strataVar.. <- NULL

    ## Handle non-numeric elements (intergers give TRUE, and pass)
    if(any(!sapply(data$variables[vars], is.numeric))){

        ## If there is any non-numeric variables
        vars <- vars[sapply(data$variables[vars], is.numeric)]
        warning("Non-numeric variables dropped")
    }

    ## Check if all the variables are continuous, and stop if not
    if(!all(sapply(data$variables[vars], is.numeric))) {
        stop("Can only be run on numeric variables")
    }


### Actual descriptive statistics are calculated here.

    ## To implement
    ## Create a single grouping variable from strata variables
    ## Create a list of subgroup data by the grouping variable
    ## Loop over each stratum with matrix forming function

    result <- sapply(strataVarLevels, function(level) {

        ## Create a matrix including vars X c(n,miss,...) matrix
        svyContSummary(vars, subset(data, ..strataVar.. %in% level))

    }, simplify = FALSE)

    ## Make it a by object
    class(result) <- "by"


    ## Add stratification variable information as an attribute
    if (length(result) > 1) {
        ## strataVarName from dimension headers
        strataVarName <- paste0(names(strata), collapse = ":")
        ## Add an attribute for the stratifying variable name
        attributes(result) <- c(attributes(result),
                                list(strataVarName = strataVarName))
    }


### Perform tests when necessary
    ## Initialize to avoid error when it does not exist at the attribute assignment
    pValues <- NULL


    ## Only when test is asked FOR
    if (test) {

        ## Loop over variables in dat, and obtain p values for two tests
        ## DF = 6 when there are 8 levels (one empty), i.e., empty strata dropped by oneway.test/kruskal.test
        pValues <-
        sapply(X = vars,
               FUN = function(var) {

                   ## Create a formula as a string
                   formulaString <- paste0(var, " ~ ..strataVar..")

                   ## Perform tests and return the result as 1x2 DF
                   ## The test functions should take a formula string as their first argument.
                   data.frame(pNormal    = ModuleTestSafe(formulaString, testNormal,
                                                          c(list(design = data, test.terms = "..strataVar.."),
                                                            argsNormal)),
                              pNonNormal = ModuleTestSafe(formulaString, testNonNormal,
                                                          c(list(design = data), argsNonNormal)))
               },
               simplify = FALSE)

        ## Create a single data frame (n x 2 (normal,nonormal))
        pValues <- do.call(rbind, pValues)
    } # Conditional for test == TRUE ends here.


### Perform SMD when requested
    smds <- NULL

    ## Only when SMD is asked for
    if (smd) {
        ## list of smds
        smds <- sapply(vars, function(var) {
            svyStdDiff(varName = var, groupName = "..strataVar..", design = data)
        }, simplify = FALSE)
        ## Give name and add mean column
        smds <- FormatLstSmds(smds, nStrata = length(result))
    }


    ## Return object
    ## Give an S3 class
    class(result) <- c("svyContTable", "ContTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues = pValues),
                            list(smd     = smds))

    ## Return
    return(result)
}
