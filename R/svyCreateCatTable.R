##' Create an object summarizing categorical variables for weighted data
##'
##' Create an object summarizing categorical variables optionally stratifying
##' by one or more startifying variables and performing statistical tests. The
##' object gives a table that is easy to use in medical research papers. See
##' also \code{\link{print.svyCatTable}} and \code{\link{summary.svyCatTable}}.
##'
##' @param vars Variable(s) to be summarized given as a character vector.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A survey design object in which these variables exist. All variables (both vars and strata) must be in this survey design object. It is created with the \code{svydesign} function in the \code{survey} package.
##' @param includeNA If TRUE, NA is handled as a regular factor level rather than missing. NA is shown as the last factor level in the table. Only effective for categorical variables.
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed. Both tests that require the large sample approximation and exact tests are performed. Either one of the result can be obtained from the print method.
##' @param testApprox A function used to perform the large sample approximation based tests. The default is \code{svychisq}.
##' @param argsApprox A named list of arguments passed to the function specified in testApprox.
##' @return An object of class \code{svyCatTable}.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{svyCreateCatTable}},  \code{\link{print.svyCatTable}},  \code{\link{summary.svyCatTable}},
##' \code{\link{svyCreateContTable}}, \code{\link{print.svyContTable}}, \code{\link{summary.svyContTable}},
##' \code{\link{svyCreateTableOne}},  \code{\link{print.TableOne}},     \code{\link{summary.TableOne}}
##' @examples
##'
##' ## See the examples for svyCreateTableOne()
##'
##' @export
svyCreateCatTable <-
function(vars,                      # character vector of variable names
         strata,                    # character vector of variable names
         data,                      # data frame
         includeNA  = FALSE,        # include NA as a category
         test       = TRUE,         # whether to put p-values
         testApprox = svyTestChisq, # function for approximation test (only choice)
         argsApprox = NULL          # arguments passed to testApprox
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

    ## Create strata data frame (data frame with only strata variables)
    ## FIXME: This changes type of strata; not a good practice
    strata <- ModuleReturnStrata(strata, data$variables)

    ## Create a single stratification variable
    ## Keeps non-existing levels
    data$variables$..strataVar.. <- interaction(strata, sep = ":")
    strataVarLevels <- levels(data$variables$..strataVar..)

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
        ## Logical vector for variables that have any NA
        logiAnyNA <- (colSums(is.na(data$variables)) > 0)

        ## Add NA as a new level unless already present
        data$variables[logiAnyNA] <-
                                lapply(data$variables[logiAnyNA],
                                       function(var) {
                                           if (all(!is.na(levels(var)))) {
                                               var <- factor(var, c(levels(var), NA),
                                                             exclude = NULL)
                                           }
                                           var
                                       })
    }

### Actual descriptive statistics are calculated here.

    ## To implement
    ## Create a single grouping variable from strata variables
    ## Create a list of subgroup data by the grouping variable
    ## Loop over each stratum with matrix forming function

    result <- sapply(strataVarLevels, function(level) {

        ## Create a matrix including vars X c(n,miss,...) matrix
        svyCatSummary(vars, subset(data, ..strataVar.. %in% level))

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
    ## Initialize
    pValues   <- NULL
    listXtabs <- list()

    ## Only when test is asked for
    if (test) {

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
                              ## Not available for survey data. Just fill in with NA
                              pExact  = NA)
                          },
                          simplify = FALSE)

        ## Create a single data frame (n x 2 (normal,nonormal))
        pValues <- do.call(rbind, pValues)
    } # Conditional for test == TRUE ends here.


    ## Return object
    ## Give an S3 class
    class(result) <- c("svyCatTable", "CatTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues = pValues),
                            list(xtabs   = listXtabs))

    ## Return
    return(result)
}
