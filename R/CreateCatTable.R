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

    ## Convert to a factor if it is not a factor already.
    ## Not done on factors, to avoid dropping zero levels.
    datNotFactor <- sapply(dat, class) != "factor"
    dat[datNotFactor] <- lapply(dat[datNotFactor], factor)
    

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


    ## Taken from Deducer::frequencies()
    CreateTableForOneVar <- function(x) {

        ## Create a one dimensional table
        freqRaw          <- table(x)

        ## Level names
        freq <- data.frame(level = names(freqRaw))

        ## Total n (duplicated as many times as there are levels)
        freq$n <- length(x)

        ## Total missing n (duplicated as many times as there are levels)
        freq$miss        <- sum(is.na(x))
        
        ## Category frequency
        freq$freq        <- freqRaw

        ## Category percent
        freq$percent     <- freqRaw / sum(freqRaw) * 100
        
        ## 
        freq$cum.percent <- cumsum(freqRaw) / sum(freqRaw) * 100
        
        ## Return result as a data frame
        return(freq)
    }

    ## Example output data frame
    ## CreateTableForOneVar(pbc$status)
    ##     n miss freq   percent cum.percent
    ## 1 418    0  232 55.502392    55.50239
    ## 2 418    0   25  5.980861    61.48325
    ## 3 418    0  161 38.516746   100.00000
        

    ## strata--variable-helperFunction structure
    ## Devide by strata
    result <- by(data = dat, INDICES = strata,

                 ## Work on each stratum
                 FUN = function(strataDat) { # strataDat should be a data frame

                     ## Loop for variables
                     sapply(strataDat,
                            FUN = CreateTableForOneVar,
                            simplify = FALSE)
                     
                 }, simplify = FALSE)

    ## Obtain collpased result
    resultCollapsed <- lapply(result,   # Loop over strata
                              function(LIST) {
                                  ## Collapse DFs within each stratum
                                  do.call(rbind, LIST)
                              })
        
    ## Perform tests when necessary
    ## Initialize
    pValues <- NULL

    ## Only when test is asked for
    if (test == TRUE) {

        ## Loop for variables
        listXtabs <- sapply(seq_len(ncol(dat)),
                           FUN = function(i) {

                               ## Test function
                               test <- listOfTests[[i]]
                               ## Outcome variable
                               var <- dat[,i]
                               ## Perform test


                               ## This part need modification to adjust to xtabs
                               xtabs(~ var + strata[[1]])
                               ## test(var ~ strata[[1]])
                           },
                           simplify = FALSE)

        ## Perform tests                                # Need error handling
        resApprox <- lapply(listXtabs, testApprox)
        resExact  <- lapply(listXtabs, testExact)

        ## Obtain p-values
        pApprox <- sapply(resApprox, getElement, "p.value")
        pExact  <- sapply(resExact, getElement, "p.value")
    }
    

    ## Return object
    ## Give an S3 class
    class(result) <- c("CatTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pApprox = pApprox,
                                 pExact  = pExact))

    ## Return
    return(result)
}
