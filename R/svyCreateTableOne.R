##' Create an object summarizing both continuous and categorical variables for weighted data
##'
##' Create an object summarizing all baseline variables (both continuous and categorical) optionally stratifying by one or more startifying variables and performing statistical tests. The object gives a table that is easy to use in medical research papers.
##'
##' @param vars Variables to be summarized given as a character vector. Factors are handled as categorical variables, whereas numeric variables are handled as continuous variables. If empty, all variables in the survey design object specified in the data argument are used.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A survey design object in which these variables exist. All variables (both vars and strata) must be in this survey design object. It is created with the \code{svydesign} function in the \code{survey} package.
##' @param factorVars Numerically coded variables that should be handled as categorical variables given as a character vector. If omitted, only factors are considered categorical variables. If all categorical variables in the dataset are already factors, this option is not necessary. The variables specified here must also be specified in the \code{vars} argument.
##' @param includeNA If TRUE, NA is handled as a regular factor level rather than missing. NA is shown as the last factor level in the table. Only effective for categorical variables.
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed.
##' @param testNormal A function used to perform the normal assumption based tests. The default is multiple degrees of freedom test using \code{svyglm} and \code{regTermTest}. This is equivalent of the \code{svyttest} when there are only two groups.
##' @param argsNormal A named list of arguments passed to the function specified in \code{testNormal}.
##' @param testNonNormal A function used to perform the nonparametric tests. The default is \code{svyranktest}.
##' @param argsNonNormal A named list of arguments passed to the function specified in \code{testNonNormal}.
##' @param testApprox A function used to perform the large sample approximation based tests. The default is \code{svychisq}.
##' @param argsApprox A named list of arguments passed to the function specified in testApprox.
##' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated.
##'
##' @details See the details for \code{\link{CreateTableOne}}.
##'
##' @return An object of class \code{svyTableOne}, which is a list of three objects.
##' @return \item{ContTable}{an object of class \code{svyContTable}, containing continuous variables only}
##' @return \item{CatTable}{ an object of class \code{svyCatTable}, containing categorical variables only}
##' @return \item{MetaData}{list of metadata regarding variables}
##'
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{print.TableOne}}, \code{\link{summary.TableOne}}
##' @examples
##'
##' ## Load packages
##' library(tableone)
##' library(survey)
##'
##' ## Create a weighted survey design object
##' data(nhanes)
##' nhanesSvy <- svydesign(ids = ~ SDMVPSU, strata = ~ SDMVSTRA, weights = ~ WTMEC2YR,
##'                        nest = TRUE, data = nhanes)
##'
##' ## Create a table object
##' ## factorVars are converted to factors; no need for variables already factors
##' ## strata will stratify summaries; leave it unspecified for overall summaries
##' tab1 <- svyCreateTableOne(vars = c("HI_CHOL","race","agecat","RIAGENDR"),
##'                           strata = "RIAGENDR", data = nhanesSvy,
##'                           factorVars = c("race","RIAGENDR"))
##'
##' ## Detailed output
##' summary(tab1)
##'
##' ## Default formatted printing
##' tab1
##'
##' ## nonnormal specifies variables to be shown as median [IQR]
##' print(tab1, nonnormal = "HI_CHOL", contDigits = 3, catDigits = 2,
##'       pDigits = 4, smd = TRUE)
##'
##' ## minMax changes it to median [min, max]
##' print(tab1, nonnormal = "HI_CHOL", minMax = TRUE, contDigits = 3,
##'       catDigits = 2, pDigits = 4, smd = TRUE)
##'
##' ## showAllLevels can be used tow show levels for all categorical variables
##' print(tab1, showAllLevels = TRUE, smd = TRUE)
##'
##' ## To see all printing options
##' ?print.TableOne
##'
##' ## To examine categorical variables only
##' tab1$CatTable
##'
##' ## To examine continuous variables only
##' tab1$ContTable
##'
##' ## If SMDs are needed as numericals, use ExtractSmd()
##' ExtractSmd(tab1)
##'
##' @export
svyCreateTableOne <-
function(vars,                                   # character vector of variable names
         strata,                                 # character vector of variable names
         data,                                   # data frame
         factorVars,                             # variables to be transformed to factors
         includeNA     = FALSE,                  # include NA as a category (categoricals only)
         test          = TRUE,                   # whether to include p-values
         ## Test configuration for categorical data
         testApprox    = svyTestChisq,           # function for approximation test (only choice)
         argsApprox    = NULL,                   # arguments passed to testApprox
         ## Test configuration for continuous data
         testNormal    = svyTestNormal,          # test for normally distributed variables
         argsNormal    = list(method = "Wald"),  # arguments passed to testNormal
         testNonNormal = svyTestNonNormal,       # test for nonnormally distributed variables
         argsNonNormal = NULL,                   # arguments passed to testNonNormal
         smd           = TRUE                    # whether to include standardize mean differences
         ) {

### Data check
    ## Check if the data given is a dataframe
    StopIfNotSurveyDesign(data)

    ## Check if vars argument is missing. If so, add all names in data.
    if (missing(vars)) {
        vars <- names(data$variables)
    }

    ## Check if variables exist. Drop them if not.
    vars <- ModuleReturnVarsExist(vars, data$variables)

    ## Abort if no variables exist at this point
    ModuleStopIfNoVarsLeft(vars)

    ## Factor conversions if the factorVars argument exist
    if (!missing(factorVars)) {
        ## Check if variables exist. Drop them if not.
        factorVars <- ModuleReturnVarsExist(factorVars, data$variables)
        ## Convert to factor
        data$variables[factorVars] <- lapply(data$variables[factorVars], factor)
    }

    ## Toggle test FALSE if no strata is given
    test <- ModuleReturnFalseIfNoStrata(strata, test)
    smd  <- ModuleReturnFalseIfNoStrata(strata, smd)

    ## Get the classes of the variables
    varClasses  <- lapply(data$variables[vars], class)

    ## Classify as varFactors if any one of these classes are contained
    varFactors <- sapply(varClasses, function(VEC) {
        any(VEC %in% c("factor", "ordered", "logical", "character"))
    })
    varFactors <- names(varFactors)[varFactors]

    ## Classify as varNumerics if any one of these classes are contained
    varNumerics <- sapply(varClasses, function(VEC) {
        any(VEC %in% c("numeric", "integer"))
    })
    varNumerics <- names(varNumerics)[varNumerics]

    ## Drop variables that do not meet either because it is unsupported
    varDrop <- setdiff(vars, c(varFactors, varNumerics))
    if (length(varDrop) > 0) {
        warning("Dropping variable(s) ", paste0(varDrop, sep = " "),
                " due to unsupported class.\n")
        vars <- setdiff(vars, varDrop)
    }

    ## Create a logical vector indicator for factors (vars in varFactors = TRUE)
    logiFactors <- vars %in% varFactors

    ## Create lists of arguments
    argsCreateContTable <- list(data          = data,
                                test          = test,
                                testNormal    = testNormal,
                                argsNormal    = argsNormal,
                                testNonNormal = testNonNormal,
                                argsNonNormal = argsNonNormal,
                                smd           = smd)
    argsCreateCatTable  <- list(data          = data,
                                includeNA     = includeNA,
                                test          = test,
                                testApprox    = testApprox,
                                argsApprox    = argsApprox,
                                smd           = smd)

    ## Add strata = strata for argument only if strata is given
    if (!missing(strata)) {

        ## Check strata. This returns a DF. Returns a "Overall" DF if strata is missing.
        ## Must not be place outside if (!missing(strata)) {  }.
        dfStrata <- ModuleReturnStrata(strata, data$variable)
        ## Return variable names. Code inefficient in exchange for code simplicity.
        strata   <- names(dfStrata)

        ## Create lists of arguments including strata
        argsCreateContTable <- c(list(strata = strata), argsCreateContTable)
        argsCreateCatTable  <- c(list(strata = strata), argsCreateCatTable)
    }


### If only varFactors/varNumerics are present, just call one constructor
    if (length(varNumerics) == 0) {
        ## No numerics
        ContTable <- NULL
        CatTable  <- do.call(svyCreateCatTable,
                             args = c(list(vars = varFactors), argsCreateCatTable))

    } else if (length(varFactors) == 0) {
        ## No factors
        ContTable <- do.call(svyCreateContTable,
                             args = c(list(vars = varNumerics), argsCreateContTable))
        CatTable  <- NULL

### Both types of variables are present, call both constructors
    } else if ((length(varFactors) > 0) & (length(varNumerics) > 0)) {

        ## ContTable
        ContTable <- do.call(svyCreateContTable,
                             args = c(list(vars = varNumerics), argsCreateContTable))
        ## CatTable
        CatTable  <- do.call(svyCreateCatTable,
                             args = c(list(vars = varFactors),  argsCreateCatTable))
    } else {

        ## vars never empty by data check with ModuleStopIfNoVarsLeft()
        ## Just to make sure
        warning("No variables left to analyzed in vars.")
    }


    ## Create a list for output
    ## Either one of the two tables may be NULL
    TableOneObject <- list(ContTable = ContTable,
                           CatTable  = CatTable,
                           MetaData  = list(vars        = vars,
                                            ## describes which pos is vars is factor
                                            logiFactors = logiFactors,
                                            ## names of vars of each type
                                            varFactors  = varFactors,
                                            varNumerics = varNumerics))

    ## Give a class
    class(TableOneObject) <- c("svyTableOne", "TableOne")

    ## Return the object
    return(TableOneObject)
}
