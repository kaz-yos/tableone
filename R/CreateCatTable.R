##' Create an object summarizing categorical variables
##'
##' Create an object summarizing categorical variables optionally stratifying by one or more startifying variables and performing statistical tests. Usually, \code{\link{CreateTableOne}} should be used as the universal frontend for both continuous and categorical data.
##'
##' @param vars Variable(s) to be summarized given as a character vector.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
##' @param includeNA If TRUE, NA is handled as a regular factor level rather than missing. NA is shown as the last factor level in the table. Only effective for categorical variables.
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed. Both tests that require the large sample approximation and exact tests are performed. Either one of the result can be obtained from the print method.
##' @param testApprox A function used to perform the large sample approximation based tests. The default is \code{chisq.test}. This is not recommended when some of the cell have small counts like fewer than 5.
##' @param argsApprox A named list of arguments passed to the function specified in testApprox. The default is \code{list(correct = TRUE)}, which turns on the continuity correction for \code{chisq.test}.
##' @param testExact A function used to perform the exact tests. The default is \code{fisher.test}. If the cells have large numbers, it will fail because of memory limitation. In this situation, the large sample approximation based should suffice.
##' @param argsExact A named list of arguments passed to the function specified in testExact. The default is \code{list(workspace = 2*10^5)}, which specifies the memory space allocated for \code{fisher.test}.
##' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated.
##' @return An object of class \code{CatTable}.
##' @author Kazuki Yoshida (based on \code{Deducer::frequencies()})
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{print.CatTable}},  \code{\link{summary.CatTable}}
##' @examples
##'
##' ## Load
##' library(tableone)
##'
##' ## Load Mayo Clinic Primary Biliary Cirrhosis Data
##' library(survival)
##' data(pbc)
##' ## Check variables
##' head(pbc)
##'
##' ## Create an overall table for categorical variables
##' catVars <- c("status","ascites","hepato","spiders","edema","stage")
##' catTableOverall <- CreateCatTable(vars = catVars, data = pbc)
##'
##' ## Simply typing the object name will invoke the print.CatTable method,
##' ## which will show the sample size, frequencies and percentages.
##' ## For 2-level variables, only the higher level is shown for simplicity
##' ## unless the variables are specified in the cramVars argument.
##' catTableOverall
##'
##' ## If you need to show both levels for some 2-level factors, use cramVars
##' print(catTableOverall, cramVars = "hepato")
##'
##' ## Use the showAllLevels argument to see all levels for all variables.
##' print(catTableOverall, showAllLevels = TRUE)
##'
##' ## You can choose form frequencies ("f") and/or percentages ("p") or both.
##' ## "fp" frequency (percentage) is the default. Row names change accordingly.
##' print(catTableOverall, format = "f")
##' print(catTableOverall, format = "p")
##'
##' ## To further examine the variables, use the summary.CatTable method,
##' ## which will show more details.
##' summary(catTableOverall)
##'
##' ## The table can be stratified by one or more variables
##' catTableBySexTrt <- CreateCatTable(vars = catVars,
##'                                    strata = c("sex","trt"), data = pbc)
##'
##' ## print now includes p-values which are by default calculated by chisq.test.
##' ## It is formatted at the decimal place specified by the pDigits argument
##' ## (3 by default). It is formatted like <0.001 if very small.
##' catTableBySexTrt
##'
##' ## The exact argument toggles the p-values to the exact test result from
##' ## fisher.test. It will show which ones are from exact tests.
##' print(catTableBySexTrt, exact = "ascites")
##'
##' ## summary now includes both types of p-values
##' summary(catTableBySexTrt)
##'
##' ## If your work flow includes copying to Excel and Word when writing manuscripts,
##' ## you may benefit from the quote argument. This will quote everything so that
##' ## Excel does not mess up the cells.
##' print(catTableBySexTrt, exact = "ascites", quote = TRUE)
##'
##' ## If you want to center-align values in Word, use noSpaces option.
##' print(catTableBySexTrt, exact = "ascites", quote = TRUE, noSpaces = TRUE)
##'
##' @export
CreateCatTable <-
function(vars,                                  # character vector of variable names
         strata,                                # character vector of variable names
         data,                                  # data frame
         includeNA  = FALSE,                    # include NA as a category
         test       = TRUE,                     # whether to include p-values
         testApprox = chisq.test,               # function for approximation test
         argsApprox = list(correct = TRUE),     # arguments passed to testApprox
         testExact  = fisher.test,              # function for exact test
         argsExact  = list(workspace = 2*10^5), # arguments passed to testExact
         smd        = TRUE                      # whether to include standardize mean differences
         ) {

### Data check
    ## Check if the data given is a dataframe
    ModuleStopIfNotDataFrame(data)

    ## Check if variables exist. Drop them if not.
    vars <- ModuleReturnVarsExist(vars, data)

    ## Abort if no variables exist at this point
    ModuleStopIfNoVarsLeft(vars)

    ## Extract necessary variables (unused variables are not included in dat)
    dat <- data[c(vars)]

    ## Toggle test FALSE if no strata
    test <- ModuleReturnFalseIfNoStrata(strata, test)
    smd  <- ModuleReturnFalseIfNoStrata(strata, smd)

    ## Create strata data frame (data frame with only strata variables)
    strata <- ModuleReturnStrata(strata, data)

    ## Convert to a factor if it is not a factor already. (categorical version only)
    ## Not done on factors, to avoid dropping zero levels.
    ## Probably this cannot handle Surv object??
    logiNotFactor <- sapply(dat, function(VEC) {
        ## Return TRUE if classes for a vector does NOT contain class "factor"
        ## VEC is a vector of a variable in the dat data frame, use class
        !any(class(VEC) %in% c("factor"))
    })

    dat[logiNotFactor] <- lapply(dat[logiNotFactor], factor)

    ## If including NA as a level, include NA as a factor level before subsetting
    if (includeNA) {
        dat <- ModuleIncludeNaAsLevel(dat)
    }

### Actual descriptive statistics are calculated here.
    ## strata--variable-CreateTableForOneVar structure
    ## Devide by strata
    result <- by(data = dat, INDICES = strata, # INDICES can be a multi-column data frame

                 ## Work on each stratum
                 FUN = function(dfStrataDat) { # dfStrataDat should be a data frame

                     ## Loop for variables
                     sapply(dfStrataDat,
                            FUN = ModuleCreateTableForOneVar,
                            simplify = FALSE)

                 }, simplify = FALSE)


    ## Add stratification variable information as an attribute
    if (length(result) > 1 ) {
        ## strataVarName from dimension headers
        strataVarName <- ModuleCreateStrataVarName(result)
        ## Add an attribute for the stratifying variable name
        attributes(result) <- c(attributes(result),
                                list(strataVarName = strataVarName))
    }


### Perform tests when necessary
    ## Initialize
    pValues   <- NULL
    listXtabs <- list()

    ## Create a single variable representation of multivariable stratification
    ## Respect ordering of levels in by()
    strataVar <- ModuleCreateStrataVarAsFactor(result, strata)

    ## Only when test is asked for
    if (test) {
        lstXtabsPVals <-
        ModuleApproxExactTests(result        = result,
                               strata        = strata,
                               dat           = dat,
                               strataVarName = strataVarName,
                               testApprox    = testApprox,
                               argsApprox    = argsApprox,
                               testExact     = testExact,
                               argsExact     = argsExact)
        pValues   <- lstXtabsPVals$pValues
        listXtabs <- lstXtabsPVals$xtabs
    }


### Perform SMD when requested
    smds <- NULL

    ## Only when SMD is asked for
    if (smd) {
        ## list of smds
        smds <- sapply(dat, function(var) {
            StdDiffMulti(variable = var, group = strataVar)
        }, simplify = FALSE)
        ## Give name and add mean column
        smds <- FormatLstSmds(smds, nStrata = length(result))
    }


    ## Return object
    ## Give an S3 class
    class(result) <- c("CatTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues = pValues),
                            list(xtabs   = listXtabs),
                            list(smd     = smds))

    ## Return
    return(result)
}
