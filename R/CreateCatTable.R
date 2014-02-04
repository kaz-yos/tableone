## A function to create a table for categorical variables
## Modification of "descriptive.table.R" in Deducer version 0.7-6.1 published on 2013-10-28 by Ian Fellows et al.

CreateCatTable <- function(vars,          # vector of characters
                           strata,        # single element character vector
                           data,          # data frame
                           exact = FALSE, # Whether to use the exact test
                           test  = FALSE, # Whether to put p-values
                           testApprox = chisq.test,
                           testExact  = fisher.test
                           ) {

### Data check
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

    
    ## This part is duplicated in the print method (better here? 2014-01-26) # just perform both tests
    ## Check the exact argument
    if (!is.logical(exact) & !is.character(exact)) {
        stop("exact argument has to be FALSE/TRUE or character.")
    }
    ## Extend if it is a logitcal vector with one element.
    if (is.logical(exact)) {

        if (length(exact) != 1) {
            stop("exact has to be a logical vector of length 1")
        }
        
        exact <- rep(exact, ncol(dat))
    }
    ## Convert to a logical vector if it is a character vector
    if (is.character(exact)) {
        exact <- vars %in% exact
    }

    ## Convert to numeric (1 for normal, 2 for exact)
    exact <- as.numeric(exact) + 1

    ## Create a list of these functions
    listOfTests <- list(testApprox, testExact)

    ## Take functions from the 2-element list, and create an n-length list
    listOfTests <- listOfTests[exact]
    


    ## ## Create indexes for default functions by partial string matching with the func.names argument
    ## func.indexes <- pmatch(func.names, c("n","miss",
    ##                                      "mean","sd",
    ##                                      "median","q25","q75","min","max",
    ##                                      "skew","kurt"))
    ## ## Remove NA
    ## func.indexes <- func.indexes[!is.na(func.indexes)]

    ## Define special skewness and kurtosis that do not fail (SAS definitions)
    ## tryCatch.W.E <- function(expr) { # Taken from demo(error.catching)
    ##     W <- NULL
    ##     w.handler <- function(w){ # warning handler
    ##         W <<- w
    ##         invokeRestart("muffleWarning")
    ##     }
    ##     list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
    ##              warning = w.handler),
    ##          warning = W)
    ## }

    ## Create a list of default functions
    functions <- c("n"      = function(x) length(x),
                   "miss"   = function(x) sum(is.na(x)),
                   "table"  = function(x) table(x),
                   "summary" = function(x) summary.factor(x)
                   )

    ## ## Keep only functions in use
    ## functions <- functions[func.indexes]

    ## ## Check for additional functions
    ## if(!missing(func.additional)){
    ##     ## When additional functions are given
    ##     if(!is.list(func.additional) || is.null(names(func.additional))) {
    ##         ## Stop if not a named list
    ##         stop("func.additional must be a named list of functions")
    ##     }

    ##     ## If a named list is given, add to the vector of functions and their names
    ##     functions  <- c(functions, unlist(func.additional))
    ##     func.names <- c(func.names, names(func.additional))
    ## }


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
    class(result) <- c("CatTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(exact = exact,
                                 pValues = pValues))

    ## Return
    return(result)
}
