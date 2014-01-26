## Print method for a continuous table
print.ContTable <- function(ContTable, missing = FALSE, digits = 2, nonnormal = FALSE, quote = FALSE) {

    ## Save variable names
    varNames <- rownames(ContTable[[1]])
    ## Check the number of rows
    nRows <- length(varNames)

    ## Check the nonnormal argument
    if (!is.logical(nonnormal) & !is.character(nonnormal)) {
        stop("nonnormal argument has to be FALSE/TRUE or character.")
    }
    ## Extend if it is a logitcal vector with one element.
    if (is.logical(nonnormal)) {

        if (length(nonnormal) != 1) {
            stop("nonormal has to be a logical vector of length 1")
        }
        
        nonnormal <- rep(nonnormal, nRows)
    }
    ## Convert to a logical vector if it is a character vector
    if (is.character(nonnormal)) {
        nonnormal <- vars %in% nonnormal
    }


################################################################################

    ## These should be moved to separate files later.

    ## Define a function for a normal variable
    ConvertNormal <- function(rowMat) {
        fmt <- paste0("%.",digits,"f"," (%.",digits,"f",")")

        out <- sprintf(fmt = fmt,
                       rowMat[, "mean"],
                       rowMat[, "sd"])

        return(out)
    }

    ## Define a function for a nonnormal variable
    ConvertNonNormal <- function(rowMat) {
        fmt <- paste0("%.",digits,"f [%.",digits,"f, %.",digits,"f]")

        out <- sprintf(fmt = fmt,
                       rowMat[, "median"],
                       rowMat[, "q25"],
                       rowMat[, "q75"])

        return(out)
    }

    ## Create a list
    listOfFunctions <- list(normal = ConvertNormal, nonnormal = ConvertNonNormal)

    ## Convert to 1 for normal, 2 for nonnormal
    nonnormal <- as.numeric(nonnormal) + 1

    ## Take functions
    listOfFunctions <- listOfFunctions[nonnormal]

    ## ## Create a function to actually apply these functions
    ## ApplyFunction <- function(fun = ConvertNormal, rowMat) {

    ##     fun(rowMat)
    ## }


    ## Loop over strata (may be just one)
    out1 <- sapply(ContTable,
                   FUN = function(stratum) {

                       ## Apply row by row within each stratum
                       out2 <- sapply(seq_len(nRows),
                                      FUN = function(i) {

                                          fun <- listOfFunctions[[i]]
                                          fun(stratum[i, , drop = FALSE])
                                      },
                                      simplify = TRUE)
                       ## Return
                       out2
                   },
                   simplify = TRUE)


    ## Put the variables names back (looping over rows can solve this)
    rownames(out1) <- varNames

    ## Return
    ## return(out1)

    ## Print
    print(out1, quote = quote)

    
## ################################################################################

##     ## nonnormal: indicator vector for nonnormality.
##     if (nonnormal == FALSE) {

##         ## If the result is stratified
##         if (length(ContTable) > 1) {

##             ## Loop over strata
##             LIST <- lapply(ContTable,
##                            function(MAT) {

##                                ## mean (sd) version
##                                fmt <- paste0("%.",digits,"f"," (%.",digits,"f",")")
##                                VEC <- sprintf(fmt = fmt,
##                                               MAT[, "mean"],
##                                               MAT[, "sd"]
##                                               )

##                                ## Name variables
##                                names(VEC) <- rownames(MAT)

##                                ## Return as a vector
##                                VEC
##                            })

##             ## Combine as columns to form a matrix
##             out <- do.call(cbind, LIST)

##             ## Show with quotes
##             print(out, quote = FALSE)
##             ## return(out)


##             ## If the result is NOT stratified
##         } else if (length(ContTable) == 1) {

##             MAT <- ContTable[[1]]

##             ## mean (sd) version
##             fmt <- paste0("%.",digits,"f"," (%.",digits,"f",")")
##             VEC <- sprintf(fmt = fmt,
##                            MAT[, "mean"],
##                            MAT[, "sd"]
##                            )

##             ## Name variables
##             names(VEC) <- rownames(MAT)

##             out <- data.frame(Overall = VEC)

##             print(out)
##         }

## ################################################################################

##         ## nonormal case
##     } else if (nonnormal == TRUE) {

##         ## If the result is stratified
##         if (length(ContTable) > 1) {

##             ## Loop over elements
##             LIST <- lapply(ContTable,
##                            function(MAT) {
##                                ## Format median [p25, p75]
##                                fmt <- paste0("%.",digits,"f [%.",digits,"f, %.",digits,"f]")
##                                VEC <- sprintf(fmt = fmt,
##                                               MAT[, "median"],
##                                               MAT[, "q25"],
##                                               MAT[, "q75"]
##                                               )

##                                ## Name variables
##                                names(VEC) <- rownames(MAT)

##                                ## Return as a vector
##                                VEC
##                            })

##             ## Combine as columns to form a matrix
##             out <- do.call(cbind, LIST)

##             ## Show with quotes
##             print(out, quote = FALSE)


##             ## If the result is NOT stratified
##         } else if (length(ContTable) == 1) {

##             MAT <- ContTable[[1]]

##             ## Format median [p25, p75]
##             fmt <- paste0("%.",digits,"f [%.",digits,"f, %.",digits,"f]")
##             VEC <- sprintf(fmt = fmt,
##                            MAT[, "median"],
##                            MAT[, "q25"],
##                            MAT[, "q75"]
##                            )

##             ## Name variables
##             names(VEC) <- rownames(MAT)

##             out <- data.frame(Overall = VEC)

##             print(out)
##         } else {stop("Neither stratified or non-stratified!?")}

##     } else {stop("Neither nonnormal or normal!?")} ## nonnormal conditions end here

}
