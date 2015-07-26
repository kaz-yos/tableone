##' Create an object summarizing categorical variables for weighted data
##'
##' Create an object summarizing categorical variables optionally stratifying
##' by one or more startifying variables and performing statistical tests. The
##' object gives a table that is easy to use in medical research papers. See
##' also \code{\link{print.CatTable}} and \code{\link{summary.CatTable}}.
##'
##' @param vars Variable(s) to be summarized given as a character vector.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed. Both tests that require the large sample approximation and exact tests are performed. Either one of the result can be obtained from the print method.
##' @param testApprox A function used to perform the large sample approximation based tests. The default is \code{\link{chisq.test}}. This is not recommended when some of the cell have small counts like fewer than 5.
##' @param argsApprox A named list of arguments passed to the function specified in testApprox. The default is \code{list(correct = TRUE)}, which turns on the continuity correction for \code{\link{chisq.test}}.
##' @param testExact A function used to perform the exact tests. The default is fisher.test. If the cells have large numbers, it will fail because of memory limitation. In this situation, the large sample approximation based should suffice.
##' @param argsExact A named list of arguments passed to the function specified in testExact. The default is \code{list(workspace = 2*10^5)}, which specifies the memory space allocated for \code{\link{fisher.test}}.
##' @return An object of class \code{CatTable}, which really is a \code{\link{by}} object with additional attributes. Each element of the \code{\link{by}} part is a matrix with rows representing variables, and columns representing summary statistics.
##' @author Kazuki Yoshida (based on \code{Deducer::frequencies()})
##' @seealso
##' \code{\link{CreateCatTable}}, \code{\link{print.CatTable}}, \code{\link{summary.CatTable}},
##' \code{\link{CreateContTable}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}},
##' \code{\link{CreateTableOne}}, \code{\link{print.TableOne}}, \code{\link{summary.TableOne}}
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
##' ## (3 by default). It does <0.001 for you.
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
