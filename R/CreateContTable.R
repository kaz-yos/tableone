## A function to create a table for continuous variables
## Modification of "descriptive.table.R" in Deducer version 0.7-6.1 published on 2013-10-28 by Ian Fellows et al.

CreateContTable <- function(vars,       # vector of characters
                            strata,     # single element character vector
                            data,       # data frame
                            func.names = c(     # can pick a subset of them
                                "n","miss",
                                "mean","sd",
                                "median","q25","q75","min","max",
                                "skew","kurt"
                                ),
                            func.additional,     # named list of additional functions
                            nonnormal = FALSE,  # Nonormal flag for testing
                            test = FALSE,       # Whether to put p-values
                            testNormal = oneway.test,           # 
                            testNonNormal = kruskal.test        # 
                            ) {
    ## Require dependencies
    require(e1071)

    ## Check the dataframe
    if (is.data.frame(data) == FALSE) {
        stop("The data argument needs to be a data frame (no quote).")
    }

    ## Check variables
    varsNotInData <- setdiff(vars, names(data))
    if (length(varsNotInData) > 0) {
        warning("The data frame does not have ",
                paste0(varsNotInData, sep = " "), ". Dropping them.")
        ## Only keep variables that exist
        vars <- intersect(vars, names(data))
    }

    ## Abort if variables exist at this point
    if (length(vars) < 1) {stop("No valid variables.")}

    ## Extract necessary variables
    dat <- data[c(vars)]

    ## Condition on the presence/absence of the strata
    if(missing(strata)){
        ## If there is no strata, name the list "Overall"
        strata <- rep("Overall", dim(dat)[1])
        ## test cannot be performed
        test <- FALSE
    } else {

        ## Check strata variable
        ## if (!strata %in% names(data)) {
        ##     stop("The stratifying variable ", strata, " does not exist in the data.")
        ## }

        ## Check presence of strata
        presenceOfStrata <- strata %in% names(data)
        strata <- strata[presenceOfStrata]

        if (length(strata) == 0) {
            stop("None of the stratifying variables are present in the data frame")
        }

        ## Extract the stratifying variable vector
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

    
    ## This part is duplicated in the print method (better here? 2014-01-26)
    ## Check the nonnormal argument
    if (!is.logical(nonnormal) & !is.character(nonnormal)) {
        stop("nonnormal argument has to be FALSE/TRUE or character.")
    }
    ## Extend if it is a logitcal vector with one element.
    if (is.logical(nonnormal)) {

        if (length(nonnormal) != 1) {
            stop("nonormal has to be a logical vector of length 1")
        }
        
        nonnormal <- rep(nonnormal, ncol(dat))
    }
    ## Convert to a logical vector if it is a character vector
    if (is.character(nonnormal)) {
        nonnormal <- vars %in% nonnormal
    }

    ## Convert to numeric (1 for normal, 2 for nonnormal)
    nonnormal <- as.numeric(nonnormal) + 1

    ## Create a list of these functions
    listOfTests <- list(testNormal, testNonNormal)

    ## Take functions from the 2-element list, and create an n-length list
    listOfTests <- listOfTests[nonnormal]
    


    ## Create indexes for default functions by partial string matching with the func.names argument
    func.indexes <- pmatch(func.names, c("n","miss",
                                         "mean","sd",
                                         "median","q25","q75","min","max",
                                         "skew","kurt"))
    ## Remove NA
    func.indexes <- func.indexes[!is.na(func.indexes)]

    ## Define special skewness and kurtosis that do not fail (SAS definitions)
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
        out <- tryCatch.W.E(skewness(x, na.rm = TRUE, type = 2))
        ## If it returns a numeric value, return it. Otherwise, return NaN.
        ifelse(is.numeric(out$value), out$value, NaN)
    }
    sasKurtosis <- function(x) {
        ## tryCatch.W.E
        out <- tryCatch.W.E(kurtosis(x, na.rm = TRUE, type = 2))
        ## If it returns a numeric value, return it. Otherwise, return NaN.
        ifelse(is.numeric(out$value), out$value, NaN)
    }

    ## Create a list of default functions
    functions <- c("n"      = function(x) length(x),
                   "miss"   = function(x) sum(is.na(x)),
                   "mean"   = function(x) mean(x, na.rm = TRUE),
                   "sd"     = function(x) sd(x, na.rm = TRUE),
                   "median" = function(x) median(x, na.rm = TRUE),
                   "q25"    = function(x) quantile(x, probs = 0.25, na.rm = TRUE),
                   "q75"    = function(x) quantile(x, probs = 0.75, na.rm = TRUE),
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


    ## strata-functions-variable structure alternative 2014-01-22
    ## Devide by strata
    result <- by(data = dat, INDICES = strata,

                 ## Work on each stratum
                 FUN = function(strataDat) { # strataDat should be a data frame

                     ## Loop for functions
                     sapply(functions,
                            FUN = function(fun) {

                                ## Loop for variables
                                sapply(strataDat, fun)
                            })
                 })

    
    ## Perform tests when necessary
    ## Initialize
    pValues <- NULL

    ## Only when test is asked for
    if (test == TRUE) {

        ## Loop for variables
        resTests <- sapply(seq_len(ncol(dat)),
                           FUN = function(i) {

                               ## Test function
                               test <- listOfTests[[i]]
                               ## Outcome variable
                               var <- dat[,i]
                               ## Perform test
                               ## Need fixing when extending to multi-var strata
                               test(var ~ strata[[1]])
                           },
                           simplify = FALSE)

        pValues <- sapply(resTests, getElement, "p.value")
    }
    

    ## Return object
    ## Give an S3 class
    class(result) <- c("ContTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(nonnormal = nonnormal,
                                 pValues = pValues))

    ## Return
    return(result)
}
