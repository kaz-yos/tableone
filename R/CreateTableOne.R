##' Create an object summarizing both categorical and continuous variables
##'
##' Create an object summarizing all baseline variables optionally stratifying by one or more startifying variables and performing statistical tests. The object gives a table that is easy to use in medical research papers. See also \code{\link{print.TableOne}} and \code{\link{summary.TableOne}}.
##'
##' @param vars Variables to be summarized given as a character vector. Factors are handled as categorical variables, whereas numeric variables are handled as continuous variables.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
##' @param factorVars Numerically coded variables that should be handled as categorical variables given as a character vector. If omitted, only factors are considered categorical variables. If all categorical variables in the dataset are already factors, this option is not necessary. The variables specified here must also be specified in the \code{vars} argument.
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed.
##' @param testNormal A function used to perform the normal assumption based tests. The default is \code{\link{oneway.test}}. This is equivalent of the t-test when there are only two groups.
##' @param argsNormal A named list of arguments passed to the function specified in \code{testNormal}. The default is \code{list(var.equal = TRUE)}, which makes it the ordinary ANOVA that assumes equal variance across groups.
##' @param testNonNormal A function used to perform the nonparametric tests. The default is \code{kruskal.test} (Kruskal-Wallis Rank Sum Test). This is equivalent of the wilcox.test (Man-Whitney U test) when there are only two groups.
##' @param argsNonNormal A named list of arguments passed to the function specified in \code{testNonNormal}. The default is \code{list(NULL)}, which is just a placeholder.
##' @param testApprox A function used to perform the large sample approximation based tests. The default is \code{\link{chisq.test}}. This is not recommended when some of the cell have small counts like fewer than 5.
##' @param argsApprox A named list of arguments passed to the function specified in testApprox. The default is \code{list(correct = TRUE)}, which turns on the continuity correction for \code{\link{chisq.test}}.
##' @param testExact A function used to perform the exact tests. The default is fisher.test. If the cells have large numbers, it will fail because of memory limitation. In this situation, the large sample approximation based should suffice.
##' @param argsExact A named list of arguments passed to the function specified in testExact. The default is \code{list(workspace = 2*10^5)}, which specifies the memory space allocated for \code{\link{fisher.test}}.
##' @return An object of class \code{TableOne}, which really is a list of three objects.
##' @return \item{TableOne}{a categorical-continuous mixture data formatted and printed by the \code{\link{print.TableOne}} method}
##' @return \item{ContTable}{an object of class \code{ContTable}, containing continuous variables only}
##' @return \item{CatTable}{ an object of class \code{CatTable}, containing categorical variables only}
##' @return The second and third objects can be then be examined with the \code{print} and \code{summary} method, for example, \code{summary(object$CatTable)} to examine the categorical variables in detail.
##'
##' @author Justin Bohn, Kazuki Yoshida
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{print.TableOne}}, \code{\link{summary.TableOne}},
##' \code{\link{CreateCatTable}}, \code{\link{print.CatTable}}, \code{\link{summary.CatTable}},
##' \code{\link{CreateContTable}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}}
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
##' ## argument to obtain the exact test p-values.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"))
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
##' @export
CreateTableOne <-
    function(vars,                                      # character vector of variable names
             strata,                                    # character vector of variable names
             data,                                      # data frame
             factorVars,                                # variables to be transformed to factors
             test          = TRUE,                      # whether to put p-values
             ## Test configuration for categorical data
             testApprox    = chisq.test,                # function for approximation test
             argsApprox    = list(correct = TRUE),      # arguments passed to testApprox
             testExact     = fisher.test,               # function for exact test
             argsExact     = list(workspace = 2*10^5),  # arguments passed to testExact
             ## Test configuration for continuous data
             testNormal    = oneway.test,               # test for normally distributed variables
             argsNormal    = list(var.equal = TRUE),    # arguments passed to testNormal
             testNonNormal = kruskal.test,              # test for nonnormally distributed variables
             argsNonNormal = list(NULL)                 # arguments passed to testNonNormal
             ) {

### Data check
        ## Check if the data given is a dataframe
        ModuleStopIfNotDataFrame(data)

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
                                    argsNonNormal = argsNonNormal
                                    )
        argsCreateCatTable  <- list(data          = data,
                                    test          = test,
                                    testApprox    = testApprox,
                                    argsApprox    = argsApprox,
                                    testExact     = testExact,
                                    argsExact     = argsExact
                                    )
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


        ## Condition on the absence of factor/numeric
        if (length(varNumerics) == 0) {
            ## No numerics
            cat('NOTE: no numeric/integer variables supplied, using CreateCatTable()\n')
            CatTable <- do.call(CreateCatTable,
                                args = c(list(vars = varFactors), argsCreateCatTable))
            return(CatTable)

        } else if (length(varFactors) == 0) {
            ## No factors
            cat('NOTE: no factor/logical/character variables supplied, using CreateContTable()\n')
            ContTable <- do.call(CreateContTable,
                                 args = c(list(vars = varNumerics), argsCreateContTable))
            return(ContTable)

### Proceed if both types of variables are present (both factors and numerics)
        } else if ((length(varFactors) > 0) & (length(varNumerics) > 0)) {

            ## Create a list of constructors
            listOfConstructors <- list(CreateContTable = CreateContTable,
                                       CreateCatTable  = CreateCatTable)
            ## CreateCatTable for categorical. CreateContTable for continuous.
            listOfConstructors <- listOfConstructors[logiFactors + 1]
            ## Create a list of arguments
            listOfArgs         <- list(argsCreateContTable = argsCreateContTable,
                                       argsCreateCatTable  = argsCreateCatTable)
            ## argsCreateCatTable for categorical. argsCreateContTable for continuous.
            listOfArgs         <- listOfArgs[logiFactors + 1]

            ## Create a list of tables by looping over variables/constructors/arguments
            TableOne <- sapply(seq_along(listOfConstructors),
                               FUN = function(i) {

                                   args <- c(list(vars = vars[i]),  # vector element
                                             listOfArgs[[i]])       # list element

                                   do.call(listOfConstructors[[i]], # list element
                                           args = args)
                               },
                               simplify = FALSE)

            ## Give variable names to the result object
            names(TableOne) <- vars


            ## Create ContTable and CatTable objects (this is redundant, but easy)
            ## Aggregated ContTable
            ContTable <- do.call(CreateContTable,
                                 args = c(list(vars = varNumerics), argsCreateContTable))
            ## Aggregated CatTable
            CatTable  <- do.call(CreateCatTable,
                                 args = c(list(vars = varFactors),  argsCreateCatTable))

            ## Create a list for output
            TableOneObject <- list(TableOne  = TableOne,
                                   ContTable = ContTable,
                                   CatTable  = CatTable
                                   )

            ## Give a class
            class(TableOneObject) <- "TableOne"

            ## Return the object
            return(TableOneObject)
        }
    }
