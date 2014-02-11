################################################################################
### Modules for tableone
## This file contains common modules used in the tableone package.
## Created on: 2014-02-10
## Author: Kazuki Yoshida
################################################################################

### Modules intended for the constructors
################################################################################
ModuleStopIfNotDataFrame <- function(data) {

    if (is.data.frame(data) == FALSE) {
        stop("The data argument needs to be a data frame (no quote).")
    }
}
## Extract variables that exist in the data frame
ModuleReturnVarsExist <- function(vars, data) {
    
    ## Check if variables exist. Drop them if not.
    varsNotInData <- setdiff(vars, names(data))
    
    if (length(varsNotInData) > 0) {
        warning("The data frame does not have ",
                paste0(varsNotInData, sep = " "), " Dropped")
        ## Only keep variables that exist
        vars <- intersect(vars, names(data))
    }
    ## Return existing variables
    return(vars)
}
## Stop if not vars are left
ModuleStopIfNoVarsLeft <- function(vars) {
    if (length(vars) < 1) {stop("No valid variables.")}
}
##
ModuleReturnFalseIfNoStrata <- function(strata, test) { # Give strata variable names

    if(missing(strata)){
        ## test cannot be performed if no strata
        test <- FALSE
    }
    return(test)
}
## Check statra variables and conditionally create
ModuleReturnStrata <- function(strata, data, dat) {     # Give strata variable names
    
    if(missing(strata)){
        ## If there is no strata, give "Overall" to every subject
        strata <- rep("Overall", dim(dat)[1])                           # Check if dim(dat)[[1]] is correct.
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

    ## return DF with strata variable(s)
    return(strata)
}
## Module to create a table for one categorical variable
## Taken from Deducer::frequencies()
ModuleCreateTableForOneVar <- function(x) { # Give a vector

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



## Create StrataVarName from multiple dimension headers
ModuleCreateStrataVarName <- function(obj) {
    ## Combine variable names with : in between
    paste0(names(attr(obj, "dimnames")), collapse = ":")
}

## Try catch function           # Taken from demo(error.catching)
## Used to define non-failing functions, that return NA when there is an error
tryCatch.W.E <- function(expr) { 
    W <- NULL
    w.handler <- function(w){ # warning handler
        W <<- w
        invokeRestart("muffleWarning")
    }
    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
             warning = w.handler),
         warning = W)
}

## Function to perform non-failing tests (obj should be xtabs or formula)
## Function has to have $p.value element
## Consider additional options by do.call()
ModuleTestSafe <- function(obj, testFunction) {

    out <- tryCatch.W.E(testFunction(obj)$p.value)
    ## If it returns a numeric value, return it. Otherwise, return NA.
    ifelse(is.numeric(out$value), out$value, NA)
}

## Define special skewness and kurtosis functions that do not fail (SAS definitions)
sasSkewness <- function(x) {
    out <- tryCatch.W.E(e1071::skewness(x, na.rm = TRUE, type = 2))
    ## If it returns a numeric value, return it. Otherwise, return NaN.
    ifelse(is.numeric(out$value), out$value, NaN)
}
sasKurtosis <- function(x) {
    out <- tryCatch.W.E(e1071::kurtosis(x, na.rm = TRUE, type = 2))
    ## If it returns a numeric value, return it. Otherwise, return NaN.
    ifelse(is.numeric(out$value), out$value, NaN)
}


### Modules intented for the print methods
################################################################################

### ModuleQuoteAndPrintMat()
## Takes an matrix object format, print, and (invisibly) return it
## Requires quote and printToggle argument in the printToggle method
ModuleQuoteAndPrintMat <- function(matObj, quote = FALSE, printToggle = TRUE) {

    ## Add quotes for names if requested
    if (quote) {
        ## row and col names
        rownames(matObj) <- paste0('"', rownames(matObj), '"')
        colnames(matObj) <- paste0('"', colnames(matObj), '"')
        ## dimension name
        names(dimnames(matObj)) <- paste0('"', names(dimnames(matObj)), '"')
        ## 1st (row) dimension needs a preceding space for best copy and paste
        names(dimnames(matObj))[1] <- paste0(" ", names(dimnames(matObj))[1])
    }


    ## print if required and return
    if (printToggle) {
        
        print(matObj, quote = quote)
        return(matObj)
        
    } else if (!printToggle) {

        return(matObj)
    }
}
