##' Create an object summarizing continous variables
##'
##' Create an object summarizing continous variables optionally stratifying by
##' one or more startifying variables and performing statistical tests. The
##' object gives a table that is easy to use in medical research papers. See
##' also \code{\link{print.ContTable}} and \code{\link{summary.ContTable}}.
##'
##' @param vars Variable(s) to be summarized given as a character vector.
##' @param strata Stratifying (grouping) variable name(s) given as a character
##' vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables
##' (both vars and strata) must be in this data frame.
##' @param func.names The functions to give the group size, number with missing
##' values, mean, standard deviations, median, 25th percentile, 75th
##' percentile, minimum, maximum, skewness (same definition as in SAS),
##' kurtosis (same definition as in SAS). All of them can be seen in the
##' summary method output. The print method uses subset of these. You can
##' choose subset of them or reorder them. They are all configure to omit NA
##' values (na.rm = TRUE).
##' @param func.additional Additional functions can be given as a named list.
##' For example, list(sum = sum).
##' @param test If TRUE, as in the default and there are more than two groups,
##' groupwise comparisons are performed. Both tests that assume normality and
##' tests that do not are performed. Either one of the result can be obtained
##' from the print method.
##' @param testNormal A function used to perform the normal assumption based
##' tests. The default is \code{\link{oneway.test}}. This is equivalent of the t-test when there are only two groups.
##' @param argsNormal A named list of arguments passed to the function specified in \code{testNormal}. The default is \code{list(var.equal = TRUE)}, which makes it the ordinary ANOVA that assumes equal variance across groups.
##' @param testNonNormal A function used to perform the nonparametric tests.
##' The default is kruskal.test (Kruskal-Wallis Rank Sum Test). This is
##' equivalent of the wilcox.test (Man-Whitney U test) when there are only two
##' groups.
##' @param argsNonNormal A named list of arguments passed to the function specified in \code{testNonNormal}. The default is \code{list(NULL)}, which is just a placeholder. 
##' @return An object of class \code{ContTable}, which really is a \code{\link{by}} object with
##' additional attributes. Each element of the \code{\link{by}} part is a matrix with rows
##' representing variables, and columns representing summary statistics.
##' @author Kazuki Yoshida
##' @seealso \code{\link{print.ContTable}}, \code{\link{summary.ContTable}}, \code{\link{CreateCatTable}},
##' \code{\link{print.CatTable}}, \code{\link{summary.CatTable}}
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
##'
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
##' ## The table can be stratified by one or more variables
##' contTableBySexTrt <- CreateContTable(vars = contVars,
##'                                      strata = c("sex","trt"), data = pbc)
##'
##' ## print now includes p-values which are by default calculated by oneway.test (t-test
##' ## equivalent in the two group case). It is formatted at the decimal place specified
##' ## by the pDigits argument (3 by default). It does <0.001 for you.
##' contTableBySexTrt
##'
##' ## The nonnormal argument will toggle the p-values to the nonparametric result from
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
##' @export
CreateContTable <-
    function(vars,                                # character vector of variable names
             strata,                              # character vector of variable names
             data,                                # data frame
             func.names = c(                      # can pick a subset of them
                 "n","miss",
                 "mean","sd",
                 "median","p25","p75","min","max",
                 "skew","kurt"
                 ),
             func.additional,                     # named list of additional functions
             test = TRUE,                         # Whether to put p-values
             testNormal = oneway.test,            # test for normally distributed variables
             argsNormal = list(var.equal = TRUE), # arguments passed to testNormal
             testNonNormal = kruskal.test,        # test for nonnormally distributed variables
             argsNonNormal = list(NULL)           # arguments passed to testNonNormal
             ) {

    ## Require dependencies (DELETE before CRAN release. Use Depends in DESCRIPTION)
    ## require(e1071)      # for skewness and kurtosis

### Data check
    ## Check if the data given is a dataframe
    ModuleStopIfNotDataFrame(data)

    ## Check if variables exist. Drop them if not.
    vars <- ModuleReturnVarsExist(vars, data)

    ## Abort if no variables exist at this point
    ModuleStopIfNoVarsLeft(vars)

    ## Extract necessary variables
    dat <- data[c(vars)]

    ## Toggle test FALSE if no strata
    test <- ModuleReturnFalseIfNoStrata(strata, test)

    ## Create strata data frame (data frame with only strata variables)
    strata <- ModuleReturnStrata(strata, data, dat)


    ## Handle non-numeric elements
    if(any(!sapply(dat, is.numeric))){
        ## If there is any non-numeric variables
        dat <- dat[sapply(dat, is.numeric)]
        warning("Non-numeric variables dropped")
    }

    ## Check if all the variables are continuous, and stop if not
    if(!all(sapply(dat, is.numeric))) {stop("Can only be run on numeric variables")}


    ## Create indexes for default functions by partial string matching with the func.names argument
    func.indexes <- pmatch(func.names, c("n","miss",
                                         "mean","sd",
                                         "median","p25","p75","min","max",
                                         "skew","kurt"))
    ## Remove NA
    func.indexes <- func.indexes[!is.na(func.indexes)]

    ## Create a list of default functions
    functions <- c("n"      = function(x) length(x),
                   "miss"   = function(x) sum(is.na(x)),
                   "mean"   = function(x) mean(x, na.rm = TRUE),
                   "sd"     = function(x) sd(x, na.rm = TRUE),
                   "median" = function(x) median(x, na.rm = TRUE),
                   "p25"    = function(x) quantile(x, probs = 0.25, na.rm = TRUE),
                   "p75"    = function(x) quantile(x, probs = 0.75, na.rm = TRUE),
                   "min"    = function(x) min(x, na.rm = TRUE),
                   "max"    = function(x) max(x, na.rm = TRUE),
                   "skew"   = function(x) sasSkewness(x),
                   "kurt"   = function(x) sasKurtosis(x)
                   )

    ## Keep only functions in use
    functions <- functions[func.indexes]

    ## Check for additional functions
    if(!missing(func.additional)){
        ## When additional functions are given
        if(!is.list(func.additional) || is.null(names(func.additional))) {
            ## Stop if not a named list
            stop("func.additional must be a named list of functions")
        }

        ## If a named list is given, add to the vector of functions and their names
        functions  <- c(functions, unlist(func.additional))
        func.names <- c(func.names, names(func.additional))
    }


### Actual descriptive statistics are calculated here.
    ## strata-functions-variable structure alternative 2014-01-22
    ## Devide by strata
    result <- by(data = dat, INDICES = strata,  # INDICES can be a multi-column data frame
                 FUN = function(strataDat) { # Work on each stratum through by()

                     ## Loop for functions
                     out <- sapply(X = functions,
                                   FUN = function(fun) {

                                       ## Loop for variables
                                       sapply(X = strataDat, FUN = fun, simplify = TRUE)

                                   }, simplify = FALSE)

                     ## The 2nd-level loop does not simplify to avoid oversimplification
                     ## when there is only one variable.
                     do.call(cbind, out)
                 })

    ## Add stratification information to the column header
    if (length(result) > 1 ) {
        ## strataVarName from dimension headers
        strataVarName <- ModuleCreateStrataVarName(result)
        ## Add an attribute for the stratifying variable name
        attributes(result) <- c(attributes(result),
                                list(strataVarName = strataVarName))
    }


### Perform tests when necessary
    ## Initialize to avoid error when it does not exist at the attribute assignment
    pValues <- NULL

### This part performs between group tests
    ## Only when test is asked for
    if (test == TRUE) {

        ## Create a single variable representing all strata
        strataVec                   <- apply(X = strata, MARGIN = 1, FUN = paste0, collapse = ":")
        ## Give NA if any of the variables are missing
        strataVecAnyMiss            <- apply(X = is.na(strata), MARGIN = 1, FUN = sum) > 0
        strataVec[strataVecAnyMiss] <- NA
        ## Make it a factor (kruskal.test requires it)
        strataVec                   <- factor(strataVec)


        ## Loop over variables in dat, and obtain p values for two tests
        pValues <-
            sapply(X = dat,
                   FUN = function(var) {
                       ## Perform tests and return the result as 1x2 DF
                       data.frame(
                           pNormal    = ModuleTestSafe(var ~ strataVec, testNormal, argsNormal),
                           pNonNormal = ModuleTestSafe(var ~ strataVec, testNonNormal, argsNonNormal)
                           )
                   },
                   simplify = FALSE)

        ## Create a single data frame (n x 2 (normal,nonormal))
        pValues <- do.call(rbind, pValues)
    }


    ## Return object
    ## Give an S3 class
    class(result) <- c("ContTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues = pValues))

    ## Return
    return(result)
}
