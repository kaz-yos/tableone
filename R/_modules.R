################################################################################
### Modules for tableone
## This file contains common modules used in the tableone package.
## Created on: 2014-02-10
## Author: Kazuki Yoshida
################################################################################

### Modules intended for the constructors
################################################################################
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
