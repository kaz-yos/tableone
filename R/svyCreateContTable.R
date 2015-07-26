##' Create an object summarizing continous variables for weighted dataset
##'
##' Create an object summarizing continous variables optionally stratifying by
##' one or more startifying variables and performing statistical tests. The
##' object gives a table that is easy to use in medical research papers. See
##' also \code{\link{print.ContTable}} and \code{\link{summary.ContTable}}.
##'
##' @param vars Variable(s) to be summarized given as a character vector.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
##' @param funcNames Not available for weighted dataset
##' @param funcAdditional Not available for weighted dataset
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed. Both tests that assume normality and tests that do not are performed. Either one of the result can be obtained from the print method.
##' @param testNormal A function used to perform the normal assumption based tests. The default is \code{\link{oneway.test}}. This is equivalent of the t-test when there are only two groups.
##' @param argsNormal A named list of arguments passed to the function specified in \code{testNormal}. The default is \code{list(var.equal = TRUE)}, which makes it the ordinary ANOVA that assumes equal variance across groups.
##' @param testNonNormal A function used to perform the nonparametric tests. The default is \code{kruskal.test} (Kruskal-Wallis rank sum test). This is equivalent of the wilcox.test (Man-Whitney U test) when there are only two groups.
##' @param argsNonNormal A named list of arguments passed to the function specified in \code{testNonNormal}. The default is \code{list(NULL)}, which is just a placeholder.
##' @return An object of class \code{ContTable}, which really is a \code{\link{by}} object with additional attributes. Each element of the \code{\link{by}} part is a matrix with rows representing variables, and columns representing summary statistics.
##' @author Kazuki Yoshida (based on \code{Deducer::descriptive.table()})
##' @seealso
##' \code{\link{CreateContTable}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}},
##' \code{\link{CreateCatTable}}, \code{\link{print.CatTable}}, \code{\link{summary.CatTable}},
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
##' ## Create an overall table for continuous variables
##' contVars <- c("time","age","bili","chol","albumin","copper",
##'               "alk.phos","ast","trig","platelet","protime")
##' contTableOverall <- CreateContTable(vars = contVars, data = pbc)
##'
##' ## Simply typing the object name will invoke the print.ContTable method,
##' ## which will show the sample size, means and standard deviations.
##' contTableOverall
##'
##' ## To further examine the variables, use the summary.ContTable method,
##' ## which will show more details.
##' summary(contTableOverall)
##'
##' ## c("age","chol","copper","alk.phos","trig","protime") appear highly skewed.
##' ## Specify them in the nonnormal argument, and the display changes to the median,
##' ## and the [25th, 75th] percentile.
##' nonNormalVars <- c("age","chol","copper","alk.phos","trig","protime")
##' print(contTableOverall, nonnormal = nonNormalVars)
##'
##' ## To show median [min,max] for nonnormal variables, use minMax = TRUE
##' print(contTableOverall, nonnormal = nonNormalVars, minMax = TRUE)
##'
##' ## The table can be stratified by one or more variables
##' contTableBySexTrt <- CreateContTable(vars = contVars,
##'                                      strata = c("sex","trt"), data = pbc)
##'
##' ## print now includes p-values which are by default calculated by oneway.test (t-test
##' ## equivalent in the two group case). It is formatted at the decimal place specified
##' ## by the pDigits argument (3 by default). It does <0.001 for you.
##' contTableBySexTrt
##'
##' ## The nonnormal argument toggles the p-values to the nonparametric result from
##' ## kruskal.test (wilcox.test equivalent for the two group case).
##' print(contTableBySexTrt, nonnormal = nonNormalVars)
##'
##' ## summary now includes both types of p-values
##' summary(contTableBySexTrt)
##'
##' ## If your work flow includes copying to Excel and Word when writing manuscripts,
##' ## you may benefit from the quote argument. This will quote everything so that
##' ## Excel does not mess up the cells.
##' print(contTableBySexTrt, nonnormal = nonNormalVars, quote = TRUE)
##'
##' ## If you want to center-align values in Word, use noSpaces option.
##' print(contTableBySexTrt, nonnormal = nonNormalVars, quote = TRUE, noSpaces = TRUE)
##'
##' @export
svyCreateContTable <-
function(vars,                                  # character vector of variable names
         strata,                                # character vector of variable names
         data,                                  # survey design data
         test          = TRUE,                  # Whether to put p-values
         testNormal    = svyTestNormal,         # test for normally distributed variables
         argsNormal    = list(method = "Wald"), # arguments passed to testNormal
         testNonNormal = svyTestNonNormal,      # test for nonnormally distributed variables
         argsNonNormal = list(NULL)             # arguments passed to testNonNormal
         ) {

    ## Require dependencies (DELETE before CRAN release. Use Depends in DESCRIPTION)
    ## require(e1071)      # for skewness and kurtosis

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

    ## Create strata data frame (data frame with only strata variables)
    ## FIXME: This changes type of strata; not a good practice
    strata <- ModuleReturnStrata(strata, data$variables)

    ## Create a single stratification variable
    ## Keeps non-existing levels
    data$variables$..strataVar..       <- interaction(strata, sep = ":")
    strataVarLevels <- levels(data$variables$..strataVar..)

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


    ## Return object
    ## Give an S3 class
    class(result) <- c("svyContTable", "ContTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues = pValues))

    ## Return
    return(result)
}
