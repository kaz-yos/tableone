##' Create an object summarizing categorical variables for weighted data
##'
##' Create an object summarizing categorical variables optionally stratifying by one or more startifying variables and performing statistical tests. Usually, \code{\link{svyCreateTableOne}} should be used as the universal frontend for both continuous and categorical data.
##'
##' @param vars Variable(s) to be summarized given as a character vector.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A survey design object in which these variables exist. All variables (both vars and strata) must be in this survey design object. It is created with the \code{svydesign} function in the \code{survey} package.
##' @param includeNA If TRUE, NA is handled as a regular factor level rather than missing. NA is shown as the last factor level in the table. Only effective for categorical variables.
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed. Both tests that require the large sample approximation and exact tests are performed. Either one of the result can be obtained from the print method.
##' @param testApprox A function used to perform the large sample approximation based tests. The default is \code{svychisq}.
##' @param argsApprox A named list of arguments passed to the function specified in testApprox.
##' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated.
##' @return An object of class \code{svyCatTable}.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{svyCreateTableOne}}, \code{\link{print.svyCatTable}}, \code{\link{summary.svyCatTable}},
##' @examples
##'
##' ## See the examples for svyCreateTableOne()
##'
##' @export
svyCreateCatTable <-
function(vars,                      # character vector of variable names
         strata,                    # character vector of variable names
         data,                      # survey design object
         includeNA  = FALSE,        # include NA as a category
         test       = TRUE,         # whether to include p-values
         testApprox = svyTestChisq, # function for approximation test (only choice)
         argsApprox = NULL,         # arguments passed to testApprox
         smd        = TRUE          # whether to include standardize mean differences
         ) {

### Data check
    ## Check if the data given is a dataframe
    StopIfNotSurveyDesign(data)

    ## Check if variables exist. Drop them if not.
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

    ## Convert to a factor if it is not a factor already. (categorical version only)
    ## Not done on factors, to avoid dropping zero levels.
    logiNotFactor <- sapply(data$variables, function(v) {
        ## Return TRUE if classes for a vector does NOT contain class "factor"
        ## v is a vector of a variable in the data$variables data frame, use class
        ## Ordered factor has c("ordered", "factor"), thus, %in% is necessary
        !("factor" %in% class(v))
    })

    data$variables[logiNotFactor] <- lapply(data$variables[logiNotFactor], factor)

    ## If including NA as a level, include NA as a factor level before subsetting
    if (includeNA) {
        data$variables <- ModuleIncludeNaAsLevel(data$variables)
    }

### Actual descriptive statistics are calculated here.

    ## Return a list of summary matrices
    result <- sapply(strataVarLevels, function(level) {

        ## Create a matrix including vars X c(n,miss,...) matrix
        svyCatSummary(vars, subset(data, ..strataVar.. %in% level))

    }, simplify = FALSE)

    ## Make it a by object
    class(result) <- "by"

    ## strataVarName from dimension headers
    strataVarName <- paste0(names(strata), collapse = ":")
    ## Add stratification variable information as an attribute
    if (length(result) > 1) {
        ## Add an attribute for the stratifying variable name
        attributes(result) <- c(attributes(result),
                                list(strataVarName = strataVarName))
    }


### Perform tests when necessary
    ## Initialize
    pValues   <- NULL
    listXtabs <- list()

    ## Only when test is asked for
    if (test) {

        lstXtabsPVals <-
        svyModuleApproxExactTests(data          = data,
                                  vars          = vars,
                                  strataVarName = strataVarName,
                                  testApprox    = testApprox,
                                  argsApprox    = argsApprox)
        pValues   <- lstXtabsPVals$pValues
        listXtabs <- lstXtabsPVals$xtabs
    }

### Perform SMD when requested
    smds <- NULL

    ## Only when SMD is asked for
    if (smd) {
        ## list of smds
        smds <- sapply(vars, function(var) {
            svyStdDiffMulti(varName = var, groupName = "..strataVar..", design = data)
        }, simplify = FALSE)
        ## Give name and add mean column
        smds <- FormatLstSmds(smds, nStrata = length(result))
    }

    ## Return object
    ## Give an S3 class
    class(result) <- c("svyCatTable", "CatTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues = pValues),
                            list(xtabs   = listXtabs),
                            list(smd     = smds))

    ## Return
    return(result)
}
