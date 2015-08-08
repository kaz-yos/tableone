##' Create an object summarizing continous variables
##'
##' Create an object summarizing continous variables optionally stratifying by one or more startifying variables and performing statistical tests. Usually, \code{\link{CreateTableOne}} should be used as the universal frontend for both continuous and categorical data.
##'
##' @param vars Variable(s) to be summarized given as a character vector.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
##' @param funcNames The functions to give the group size, number with missing values, proportion with missing values, mean, standard deviations, median, 25th percentile, 75th percentile, minimum, maximum, skewness (same definition as in SAS), kurtosis (same definition as in SAS). All of them can be seen in the summary method output. The print method uses subset of these. You can choose subset of them or reorder them. They are all configure to omit NA values (\code{na.rm = TRUE}).
##' @param funcAdditional Additional functions can be given as a named list. For example, \code{list(sum = sum)}.
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed. Both tests that assume normality and tests that do not are performed. Either one of the result can be obtained from the print method.
##' @param testNormal A function used to perform the normal assumption based tests. The default is \code{oneway.test}. This is equivalent of the t-test when there are only two groups.
##' @param argsNormal A named list of arguments passed to the function specified in \code{testNormal}. The default is \code{list(var.equal = TRUE)}, which makes it the ordinary ANOVA that assumes equal variance across groups.
##' @param testNonNormal A function used to perform the nonparametric tests. The default is \code{kruskal.test} (Kruskal-Wallis rank sum test). This is equivalent of the wilcox.test (Man-Whitney U test) when there are only two groups.
##' @param argsNonNormal A named list of arguments passed to the function specified in \code{testNonNormal}. The default is \code{list(NULL)}, which is just a placeholder.
##' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated.
##' @return An object of class \code{ContTable}.
##' @author Kazuki Yoshida (based on \code{Deducer::descriptive.table()})
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}}
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
CreateContTable <-
function(vars,                                   # character vector of variable names
         strata,                                 # character vector of variable names
         data,                                   # data frame
         funcNames    = c(                       # can pick a subset of them
                 "n","miss","p.miss",
                 "mean","sd",
                 "median","p25","p75","min","max",
                 "skew","kurt"),
         funcAdditional,                         # named list of additional functions
         test          = TRUE,                   # Whether to include p-values
         testNormal    = oneway.test,            # test for normally distributed variables
         argsNormal    = list(var.equal = TRUE), # arguments passed to testNormal
         testNonNormal = kruskal.test,           # test for nonnormally distributed variables
         argsNonNormal = list(NULL),             # arguments passed to testNonNormal
         smd           = TRUE                    # whether to include standardize mean differences
         ) {

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
    smd  <- ModuleReturnFalseIfNoStrata(strata, smd)

    ## Create strata data frame (data frame with only strata variables)
    strata <- ModuleReturnStrata(strata, data)


    ## Handle non-numeric elements (intergers give TRUE, and pass)
    if(any(!sapply(dat, is.numeric))){
        ## If there is any non-numeric variables
        dat <- dat[sapply(dat, is.numeric)]
        warning("Non-numeric variables dropped")
    }

    ## Check if all the variables are continuous, and stop if not
    if(!all(sapply(dat, is.numeric))) {stop("Can only be run on numeric variables")}


    ## Create indexes for default functions by partial string matching with the funcNames argument
    funcIndexes <- pmatch(funcNames, c("n","miss","p.miss",
                                       "mean","sd",
                                       "median","p25","p75","min","max",
                                       "skew","kurt"))
    ## Remove NA
    funcIndexes <- funcIndexes[!is.na(funcIndexes)]

    ## Create a list of default functions
    functions <- c("n"      = function(x) {length(x)},
                   "miss"   = function(x) {sum(is.na(x))},
                   "p.miss" = function(x) {(sum(is.na(x)) / length(x)) * 100},
                   "mean"   = function(x) {mean(x, na.rm = TRUE)},
                   "sd"     = function(x) {sd(x, na.rm = TRUE)},
                   "median" = function(x) {median(x, na.rm = TRUE)},
                   "p25"    = function(x) {quantile(x, probs = 0.25, na.rm = TRUE)},
                   "p75"    = function(x) {quantile(x, probs = 0.75, na.rm = TRUE)},
                   "min"    = function(x) {min(x, na.rm = TRUE)},
                   "max"    = function(x) {max(x, na.rm = TRUE)},
                   "skew"   = function(x) {ModuleSasSkewness(x)},
                   "kurt"   = function(x) {ModuleSasKurtosis(x)}
                   )

    ## Keep only functions in use
    functions <- functions[funcIndexes]

    ## Check for additional functions
    if(!missing(funcAdditional)) {

        ## When additional functions are given
        if(!is.list(funcAdditional) || is.null(names(funcAdditional))) {
            ## Stop if not a named list
            stop("funcAdditional must be a named list of functions")
        }

        ## If a named list is given, add to the vector of functions and their names
        functions  <- c(functions, unlist(funcAdditional))
        funcNames  <- c(funcNames, names(funcAdditional))
    }


### Actual descriptive statistics are calculated here.
    ## strata-functions-variable structure alternative 2014-01-22
    ## Devide by strata
    result <- by(data = dat, INDICES = strata, # INDICES can be a multi-column data frame

                 ## Work on each stratum
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

    ## Add stratification variable information as an attribute
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

    ## Create a single variable representation of multivariable stratification
    ## Respect ordering of levels in by()
    strataVar <- ModuleCreateStrataVarAsFactor(result, strata)

    ## Only when test is asked for
    if (test) {

        ## Loop over variables in dat, and obtain p values for two tests
        ## DF = 6 when there are 8 levels (one empty),
        ## i.e., empty strata dropped by oneway.test/kruskal.test
        pValues <-
            sapply(X = dat,
                   FUN = function(var) {
                       ## Perform tests and return the result as 1x2 DF
                       data.frame(
                           pNormal    = ModuleTestSafe(var ~ strataVar, testNormal,    argsNormal),
                           pNonNormal = ModuleTestSafe(var ~ strataVar, testNonNormal, argsNonNormal)
                           )
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
        smds <- sapply(dat, function(var) {
            StdDiff(variable = var, group = strataVar)
        }, simplify = FALSE)
        ## Give name and add mean column
        smds <- FormatLstSmds(smds, nStrata = length(result))
    }


    ## Return object
    ## Give an S3 class
    class(result) <- c("ContTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues = pValues),
                            list(smd     = smds))

    ## Return
    return(result)
}
