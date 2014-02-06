## A function to create a table for categorical variables
## Modification of "descriptive.table.R" in Deducer version 0.7-6.1 published on 2013-10-28 by Ian Fellows et al.

CreateCatTable <- function(vars,                    # vector of characters
                           strata,                  # single element character vector
                           data,                    # data frame
                           test  = TRUE,            # Whether to put p-values
                           testApprox = chisq.test, # approximation test
                           testExact  = fisher.test # exact test
                           ) {

### Data check
    ## Check if the data given is a dataframe.
    if (is.data.frame(data) == FALSE) {
        stop("The data argument needs to be a data frame (no quote).")
    }

    ## Check if variables exist in the data frame. If not, drop them.
    varsNotInData <- setdiff(vars, names(data))
    if (length(varsNotInData) > 0) {
        warning("The data frame does not have ",
                paste0(varsNotInData, sep = " "), ". Dropping them.")
        ## Only keep variables that exist
        vars <- intersect(vars, names(data))
    }

    ## Abort if no variables exist at this point
    if (length(vars) < 1) {stop("No valid variables.")}

    ## Extract necessary variables (unused variables are not included in dat)
    dat <- data[c(vars)]

    ## Convert to a factor if it is not a factor already. (categorical version only)
    ## Not done on factors, to avoid dropping zero levels.
    datNotFactor <- sapply(dat, class) != "factor"
    dat[datNotFactor] <- lapply(dat[datNotFactor], factor)


    ## Condition on the presence/absence of the strata
    if(missing(strata)){
        ## If there is no strata, give "Overall" to every subject
        strata <- rep("Overall", dim(dat)[1])                           # Check if dim(dat)[[1]] is correct.
        ## test cannot be performed
        test <- FALSE
    } else {

        ## Check presence of strata variables in the data frame  (multivariable support)
        presenceOfStrata <- strata %in% names(data)
        ## Delete variables that do not exist in the data frame
        strata <- strata[presenceOfStrata]

        if (length(strata) == 0) {
            stop("None of the stratifying variables are present in the data frame")
        }

        ## Extract the stratifying variable vector (strata is a data frame)
        strata <- data[c(strata)]
    }



### Perform descriptive analysis

    ## Used to define non-failing functions, that return NA when there is an error
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

        ## Create a one dimensional table (NA is intentionally dropped)
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

        ## Category percent (cumulative)
        freq$cum.percent <- cumsum(freqRaw) / sum(freqRaw) * 100

        ## Reorder variables
        freq <- freq[c("n","miss","level","freq","percent","cum.percent")]

        ## Return result as a data frame
        return(freq)
    }

    ## strata--variable-CreateTableForOneVar structure
    ## Devide by strata
    result <- by(data = dat, INDICES = strata,

                 ## Work on each stratum
                 FUN = function(dfStrataDat) { # dfStrataDat should be a data frame

                     ## Loop for variables
                     sapply(dfStrataDat,
                            FUN = CreateTableForOneVar,
                            simplify = FALSE)

                 }, simplify = FALSE)

    
### Perform tests when necessary
    ## Initialize
    pValues <- NULL

    ## Only when test is asked for
    if (test == TRUE) {

        ## Define special test functions that do not fail, and return p-values or NA
        tryTestApprox <- function(mat) {

            out <- tryCatch.W.E(testApprox(mat)$p.value)
            ## If it returns a numeric value, return it. Otherwise, return NA.
            ifelse(is.numeric(out$value), out$value, NA)
        }
        tryTestExact <- function(mat) {

            out <- tryCatch.W.E(testExact(mat)$p.value)
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
                                ## Create a 2-dimensional table
                                resXtabs <- xtabs(~ var + strataVec)
                                
                                ## Perform tests and return the result as 1x2 DF
                                data.frame(
                                    pApprox = tryTestApprox(resXtabs),
                                    pExact  = tryTestExact(resXtabs)
                                    )
                            },
                            simplify = FALSE)

        ## Create a single data frame (n x 2 (normal,nonormal))
        pValues <- do.call(rbind, pValues)
    }


    ## Return object
    ## Give an S3 class
    class(result) <- c("CatTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues  = pValues))

    ## Return
    return(result)
}
