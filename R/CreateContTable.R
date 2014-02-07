## A function to create a table for continuous variables
## Modification of "descriptive.table.R" in Deducer version 0.7-6.1 published on 2013-10-28 by Ian Fellows et al.

CreateContTable <- function(vars,                         # vector of characters
                            strata,                       # single element character vector
                            data,                         # data frame
                            func.names = c(               # can pick a subset of them
                                "n","miss",
                                "mean","sd",
                                "median","p25","p75","min","max",
                                "skew","kurt"
                                ),
                            func.additional,              # named list of additional functions
                            ## nonnormal = FALSE,         # Nonormal flag for testing
                            test = TRUE,                  # Whether to put p-values
                            testNormal = oneway.test,     # test for normally distributed variables
                            testNonNormal = kruskal.test  # test for nonnormally distributed variables
                            ) {
    ## Require dependencies
    require(e1071)      # for skewness and kurtosis

    ## Check if the data given is a dataframe
    if (is.data.frame(data) == FALSE) {
        stop("The data argument needs to be a data frame (no quote).")
    }

    ## Check if variables exist. Drop them if not.
    varsNotInData <- setdiff(vars, names(data))
    if (length(varsNotInData) > 0) {
        warning("The data frame does not have ",
                paste0(varsNotInData, sep = " "), ". Dropping them.")
        ## Only keep variables that exist
        vars <- intersect(vars, names(data))
    }

    ## Abort if no variables exist at this point
    if (length(vars) < 1) {stop("No valid variables.")}

    ## Extract necessary variables
    dat <- data[c(vars)]


    ## Condition on the presence/absence of the strata
    if(missing(strata)){
        ## If there is no strata, give "Overall" to every subject
        strata <- rep("Overall", dim(dat)[1])
        ## test cannot be performed
        test <- FALSE
    } else {

        ## Check presence of strata variables in the data frame (multivariable support)
        presenceOfStrata <- strata %in% names(data)
        strata <- strata[presenceOfStrata]

        if (length(strata) == 0) {
            stop("None of the stratifying variables are present in the data frame")
        }

        ## Extract the stratifying variables as a data frame (by() can use it directly)
        strata <- data[c(strata)]
    }


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

    ## Define special skewness and kurtosis functions that do not fail (SAS definitions)
    tryCatch.W.E <- function(expr) { # Taken from demo(error.catching)
        W <- NULL
        w.handler <- function(w){ # warning handler
            W <<- w
            invokeRestart("muffleWarning")
        }
        list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                 warning = w.handler),
             warning = W)
    }
    sasSkewness <- function(x) {
        ## tryCatch.W.E
        out <- tryCatch.W.E(e1071::skewness(x, na.rm = TRUE, type = 2))
        ## If it returns a numeric value, return it. Otherwise, return NaN.
        ifelse(is.numeric(out$value), out$value, NaN)
    }
    sasKurtosis <- function(x) {
        ## tryCatch.W.E
        out <- tryCatch.W.E(e1071::kurtosis(x, na.rm = TRUE, type = 2))
        ## If it returns a numeric value, return it. Otherwise, return NaN.
        ifelse(is.numeric(out$value), out$value, NaN)
    }

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


    ## Perform tests when necessary
    ## Initialize to avoid error when it does not exist at the attribute assignment
    pValues <- NULL

### This part performs between group tests
    ## Only when test is asked for
    if (test == TRUE) {

        ## Define special test functions that do not fail, and return p-values or NA
        tryTestNormal <- function(formula) {

            out <- tryCatch.W.E(testNormal(formula)$p.value)
            ## If it returns a numeric value, return it. Otherwise, return NA.
            ifelse(is.numeric(out$value), out$value, NA)
        }
        tryTestNonNormal <- function(formula) {

            out <- tryCatch.W.E(testNonNormal(formula)$p.value)
            ## If it returns a numeric value, return it. Otherwise, return NA.
            ifelse(is.numeric(out$value), out$value, NA)
        }


        ## Create a single variable representing all strata
        strataVec                   <- apply(X = strata, MARGIN = 1, FUN = paste0, collapse = ":")
        ## Give NA if any of the variables are missing
        strataVecAnyMiss            <- apply(X = is.na(strata), MARGIN = 1, FUN = sum) > 0
        strataVec[strataVecAnyMiss] <- NA
        ## Make it a factor (kruskal.test requires it)
        strataVec                   <- factor(strataVec)


        ## Loop over variables in dat, and obtain p values for two tests
        pValues <- sapply(X = dat,
                          FUN = function(var) {
                              ## Perform tests and return the result as 1x2 DF
                              data.frame(
                                  pNormal    = tryTestNormal(var ~ strataVec),
                                  pNonNormal = tryTestNonNormal(var ~ strataVec)
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
