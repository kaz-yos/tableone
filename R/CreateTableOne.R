### 2014-02-09 Unifying function suggested by Justin Bohn
##'
##' Create an object|output summarizing both categorical and continuous variables
##'
##' Create an object|output summarizing categorical variables optionally stratifying
##' by one or more startifying variables and performing statistical tests. The
##' object gives a table that is easy to use in medical research papers.
##'
##' @param vars Variables to be summarized given as a character vector. Factors are
##' handled as categorical variables, whereas numeric variables are handled as continuous variables.
##' @param strata Stratifying (grouping) variable name(s) given as a character
##' vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables
##' (both vars and strata) must be in this data frame.
##' @param test If TRUE, as in the default and there are more than two groups,
##' groupwise comparisons are performed.
##' @param testNormal A function used to perform the normal assumption based
##' tests. The default is \code{\link{oneway.test}}. This is equivalent of the t-test when there are only two groups.
##' @param argsNormal A named list of arguments passed to the function specified in \code{testNormal}. The default is \code{list(var.equal = TRUE)}, which makes it the ordinary ANOVA that assumes equal variance across groups.
##' @param testNonNormal A function used to perform the nonparametric tests.
##' The default is \code{kruskal.test} (Kruskal-Wallis Rank Sum Test). This is
##' equivalent of the wilcox.test (Man-Whitney U test) when there are only two
##' groups.
##' @param argsNonNormal A named list of arguments passed to the function specified in \code{testNonNormal}. The default is \code{list(NULL)}, which is just a placeholder.
##' @param testApprox A function used to perform the large sample approximation
##' based tests. The default is \code{\link{chisq.test}}. This is not recommended when some
##' of the cell have small counts like fewer than 5.
##' @param argsApprox A named list of arguments passed to the function specified in testApprox. The default is \code{list(correct = TRUE)}, which turns on the continuity correction for \code{\link{chisq.test}}.
##' @param testExact A function used to perform the exact tests. The default is
##' fisher.test. If the cells have large numbers, it will fail because of
##' memory limitation. In this situation, the large sample approximation based
##' should suffice.
##' @param argsExact A named list of arguments passed to the function specified in testExact. The default is \code{list(workspace = 2*10^5)}, which specifies the memory space allocated for \code{\link{fisher.test}}.
##' @return An object of class \code{TableOne}, which really is a list of two \code{\link{by}} objects with
##' additional attributes. These correspond to structures holding results for
##' @author Justin Bohn, Kazuki Yoshida
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{print.TableOne}}, \code{\link{summary.TableOne}},
##' \code{\link{CreateCatTable}}, \code{\link{print.CatTable}}, \code{\link{summary.CatTable}},
##' \code{\link{CreateContTable}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}}
##' @examples
##' library(tableone)
##' library(survival)
##' data(pbc)
##'
##' @export
CreateTableOne <-
    function(vars, strata, data,
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

        ## Get the classes of the variables
        varClasses  <- sapply(data[vars], class)
        varFactors  <- names(varClasses[varClasses == "factor"])
        varNumerics <- names(varClasses[varClasses == "numeric" | varClasses == "integer"])

        ## Create a logical vector indicator for factors
        logiFactors <- sapply(data[vars], is.factor)

        ## Create lists of arguments
        argsCreateContTable <- list(strata = strata, data = data,
                                    test       = test,
                                    testNormal = testNormal,
                                    argsNormal = argsNormal,
                                    testNonNormal = testNonNormal,
                                    argsNonNormal = argsNonNormal
                                    )
        argsCreateCatTable <- list(strata = strata, data = data,
                                   test       = test,
                                   testApprox = testApprox,
                                   argsApprox = argsApprox,
                                   testExact  = testExact,
                                   argsExact  = argsExact
                                   )


        ## Condition on the absence of factor/numeric
        if (length(varNumerics) == 0) {
            ## No numerics
            cat('NOTE: no numeric/integer variables supplied, using CreateCatTable()\n')
            CatTable <- do.call(CreateCatTable,
                                args = c(list(vars = varFactors), argsCreateCatTable))
            return(CatTable)

        } else if (length(varFactors) == 0) {
            ## No factors
            cat('NOTE: no factor variables supplied, using CreateContTable()\n')
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
            listOfArgs <- list(argsCreateContTable = argsCreateContTable,
                               argsCreateCatTable  = argsCreateCatTable)
            ## argsCreateCatTable for categorical. argsCreateContTable for continuous.
            listOfArgs <- listOfArgs[logiFactors + 1]

            ## Create a list of tables
            listOfTables <- sapply(seq_along(listOfConstructors),
                                   FUN = function(i) {

                                       args <- c(list(vars = vars[i]),  # vector element
                                                 listOfArgs[[i]])       # list element

                                       do.call(listOfConstructors[[i]], # list element
                                               args = args)
                                   },
                                   simplify = FALSE)

            ## Give variable names
            names(listOfTables) <- vars
            
            ## Give a class
            class(listOfTables) <- "TableOne"

            ## Return the object
            return(listOfTables)
        }
    }

## ideas/comments (Justin):
##   1. smartly process variables based on their class
##      (i.e. do not make user specify which vars are continuous vs categorical)
##   2. figure out a good print method
##   3. if user specifies additional func.names for CreateContTable,
##      the dimensions will be wrong and rbind will not work. Could consider
##      extra functions creating extra rows instead of extra columns.
##   4. it looks like your print method does some kind of alignment with spaces
##      that might lead to strange formatting after rbind()
