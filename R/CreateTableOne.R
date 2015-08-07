##' Create an object summarizing both continuous and categorical variables
##'
##' Create an object summarizing all baseline variables (both continuous and categorical) optionally stratifying by one or more startifying variables and performing statistical tests. The object gives a table that is easy to use in medical research papers.
##'
##' @param vars Variables to be summarized given as a character vector. Factors are handled as categorical variables, whereas numeric variables are handled as continuous variables. If empty, all variables in the data frame specified in the data argument are used.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
##' @param factorVars Numerically coded variables that should be handled as categorical variables given as a character vector. If omitted, only factors are considered categorical variables. If all categorical variables in the dataset are already factors, this option is not necessary. The variables specified here must also be specified in the \code{vars} argument.
##' @param includeNA If TRUE, NA is handled as a regular factor level rather than missing. NA is shown as the last factor level in the table. Only effective for categorical variables.
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed.
##' @param testNormal A function used to perform the normal assumption based tests. The default is \code{oneway.test}. This is equivalent of the t-test when there are only two groups.
##' @param argsNormal A named list of arguments passed to the function specified in \code{testNormal}. The default is \code{list(var.equal = TRUE)}, which makes it the ordinary ANOVA that assumes equal variance across groups.
##' @param testNonNormal A function used to perform the nonparametric tests. The default is \code{kruskal.test} (Kruskal-Wallis Rank Sum Test). This is equivalent of the wilcox.test (Man-Whitney U test) when there are only two groups.
##' @param argsNonNormal A named list of arguments passed to the function specified in \code{testNonNormal}. The default is \code{list(NULL)}, which is just a placeholder.
##' @param testApprox A function used to perform the large sample approximation based tests. The default is \code{chisq.test}. This is not recommended when some of the cell have small counts like fewer than 5.
##' @param argsApprox A named list of arguments passed to the function specified in testApprox. The default is \code{list(correct = TRUE)}, which turns on the continuity correction for \code{chisq.test}.
##' @param testExact A function used to perform the exact tests. The default is \code{fisher.test}. If the cells have large numbers, it will fail because of memory limitation. In this situation, the large sample approximation based should suffice.
##' @param argsExact A named list of arguments passed to the function specified in testExact. The default is \code{list(workspace = 2*10^5)}, which specifies the memory space allocated for \code{fisher.test}.
##' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated.
##'
##' @details The definitions of the standardized mean difference (SMD) are available in \href{http://www.tandfonline.com/doi/abs/10.1080/00031305.1986.10475403}{Flury \emph{et al} 1986} for the univariate case and the multivariate case (essentially the square root of the Mahalanobis distance). Extension to binary variables is discussed in \href{http://www.tandfonline.com/doi/abs/10.1080/03610910902859574}{Austin 2009} and extension to multinomival variables is suggested in \href{http://support.sas.com/resources/papers/proceedings12/335-2012.pdf}{Yang \emph{et al} 2012}. This multinomial extesion treats a single multinomial variable as multiple non-redundant dichotomous variables and use the Mahalanobis distance. The off diagonal elements of the covariance matrix on page 3 have an error, and need negation. In weighted data, the same definitions can be used except that the mean and standard deviation estimates are weighted estimates (\href{http://www.ncbi.nlm.nih.gov/pubmed/23902694}{Li \emph{et al} 2013} and \href{http://onlinelibrary.wiley.com/doi/10.1002/sim.6607/full}{Austin \emph{et al} 2015}). In tableone, all weighted estimates are calculated by weighted estimation functions in the \code{survey} package.
##'
##' @return An object of class \code{TableOne}, which is a list of three objects.
##' @return \item{ContTable}{object of class \code{ContTable}, containing continuous variables only}
##' @return \item{CatTable}{object of class \code{CatTable}, containing categorical variables only}
##' @return \item{MetaData}{list of metadata regarding variables}
##'
##' @references Flury, BK. and Riedwyl, H. (1986). Standard distance in univariate and multivariate analysis. \emph{The American Statistician}, \bold{40}, 249-251.
##' @references Austin, PC. (2009). Using the Standardized Difference to Compare the Prevalence of a Binary Variable Between Two Groups in Observational Research. \emph{Communications in Statistics - Simulation and Computation}, \bold{38}, 1228-1234.
##' @references Yang, D. and Dalton, JE. (2012). A unified approach to measuring the effect size between two groups using SAS. SAS Global Forum 2012, Paper 335-2012.
##' @references Li, L. and Greene, T. (2013). A weighting analogue to pair matching in propensity score analysis. \emph{International Journal of Biostatistics}, \bold{9}, 215-234.
##' @references Austin, PC. and Stuart, EA. (2015). Moving towards best practice when using inverse probability of treatment weighting (IPTW) using the propensity score to estimate causal treatment effects in observational studies. \emph{Statistics in Medicine}, Online on August 3, 2015.
##'
##' @author Kazuki Yoshida, Justin Bohn
##' @seealso
##' \code{\link{print.TableOne}}, \code{\link{summary.TableOne}}
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
##' ## Make categorical variables factors
##' varsToFactor <- c("status","trt","ascites","hepato","spiders","edema","stage")
##' pbc[varsToFactor] <- lapply(pbc[varsToFactor], factor)
##'
##' ## Create a variable list
##' dput(names(pbc))
##' vars <- c("time","status","age","sex","ascites","hepato",
##'           "spiders","edema","bili","chol","albumin",
##'           "copper","alk.phos","ast","trig","platelet",
##'           "protime","stage")
##'
##' ## Create Table 1 stratified by trt
##' tableOne <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc)
##'
##' ## Just typing the object name will invoke the print.TableOne method
##' tableOne
##'
##' ## Specifying nonnormal variables will show the variables appropriately,
##' ## and show nonparametric test p-values. Specify variables in the exact
##' ## argument to obtain the exact test p-values. cramVars can be used to
##' ## show both levels for a 2-level categorical variables.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), cramVars = "hepato", smd = TRUE)
##'
##' ## Use the summary.TableOne method for detailed summary
##' summary(tableOne)
##'
##' ## See the categorical part only using $ operator
##' tableOne$CatTable
##' summary(tableOne$CatTable)
##'
##' ## See the continuous part only using $ operator
##' tableOne$ContTable
##' summary(tableOne$ContTable)
##'
##' ## If your work flow includes copying to Excel and Word when writing manuscripts,
##' ## you may benefit from the quote argument. This will quote everything so that
##' ## Excel does not mess up the cells.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), quote = TRUE)
##'
##' ## If you want to center-align values in Word, use noSpaces option.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), quote = TRUE, noSpaces = TRUE)
##'
##' ## If SMDs are needed as numericals, use ExtractSmd()
##' ExtractSmd(tableOne)
##'
##' @export
CreateTableOne <-
function(vars,                                      # character vector of variable names
         strata,                                    # character vector of variable names
         data,                                      # data frame
         factorVars,                                # variables to be transformed to factors
         includeNA     = FALSE,                     # include NA as a category (categoricals only)
         test          = TRUE,                      # whether to include p-values
         ## Test configuration for categorical data
         testApprox    = chisq.test,                # function for approximation test
         argsApprox    = list(correct = TRUE),      # arguments passed to testApprox
         testExact     = fisher.test,               # function for exact test
         argsExact     = list(workspace = 2*10^5),  # arguments passed to testExact
         ## Test configuration for continuous data
         testNormal    = oneway.test,               # test for normally distributed variables
         argsNormal    = list(var.equal = TRUE),    # arguments passed to testNormal
         testNonNormal = kruskal.test,              # test for nonnormally distributed variables
         argsNonNormal = list(NULL),                # arguments passed to testNonNormal
         smd           = TRUE                       # whether to include standardize mean differences
         ) {

### Data check
    ## Check if the data given is a dataframe
    ModuleStopIfNotDataFrame(data)

    ## Check if vars argument is missing. If so, add all names in data.
    if (missing(vars)) {
        vars <- names(data)
    }

    ## Check if variables exist. Drop them if not.
    vars <- ModuleReturnVarsExist(vars, data)

    ## Abort if no variables exist at this point
    ModuleStopIfNoVarsLeft(vars)

    ## Factor conversions if the factorVars argument exist
    if (!missing(factorVars)) {
        ## Check if variables exist. Drop them if not.
        factorVars <- ModuleReturnVarsExist(factorVars, data)
        ## Convert to factor
        data[factorVars] <- lapply(data[factorVars], factor)
    }

    ## Toggle test FALSE if no strata is given
    test <- ModuleReturnFalseIfNoStrata(strata, test)
    smd  <- ModuleReturnFalseIfNoStrata(strata, smd)

    ## Get the classes of the variables
    varClasses  <- lapply(data[vars], class)

    ## Classify as varFactors if any one of these classes are contained
    varFactors <-sapply(varClasses, function(VEC) {
        any(VEC %in% c("factor", "ordered", "logical", "character"))
    })
    varFactors <- names(varFactors)[varFactors]

    ## Classify as varNumerics if any one of these classes are contained
    varNumerics <-sapply(varClasses, function(VEC) {
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
                                testExact     = testExact,
                                argsExact     = argsExact,
                                smd           = smd)

    ## Add strata = strata for argument only if strata is given
    if (!missing(strata)) {

        ## Check strata. This returns a DF. Returns a "Overall" DF if strata is missing.
        ## Must not be place outside if (!missing(strata)) {  }.
        dfStrata <- ModuleReturnStrata(strata, data)
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
        CatTable  <- do.call(CreateCatTable,
                             args = c(list(vars = varFactors), argsCreateCatTable))

    } else if (length(varFactors) == 0) {
        ## No factors
        ContTable <- do.call(CreateContTable,
                             args = c(list(vars = varNumerics), argsCreateContTable))
        CatTable  <- NULL

### Both types of variables are present, call both constructors
    } else if ((length(varFactors) > 0) & (length(varNumerics) > 0)) {

        ## ContTable
        ContTable <- do.call(CreateContTable,
                             args = c(list(vars = varNumerics), argsCreateContTable))
        ## CatTable
        CatTable  <- do.call(CreateCatTable,
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
    class(TableOneObject) <- "TableOne"

    ## Return the object
    return(TableOneObject)
}
