## Print method for a continuous table
print.ContTable <- function(ContTable, missing = FALSE,
                            digits = 2, nonnormal = NULL, quote = FALSE,
                            test = TRUE, pDigits = 3,
                            explain = TRUE
                            ) {

### Check data structure first

    ## ContTable is by() object
    ## Get the position of the first non-null element
    posFirstNonNullElement <- which(!sapply(ContTable, is.null))[1]
    ## Save variable names using the first non-null element
    varNames <- rownames(ContTable[[posFirstNonNullElement]])
    ## Check the number of rows
    nRows <- length(varNames)

    ## If null, do normal print/test
    if (is.null(nonnormal)) {
        ##  Give one as many as there are rows
        nonnormal <- rep(1, nRows)

    } else {
        ## If not null, it needs checking.

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
        ## Convert to numeric (1 for normal, 2 for nonnormal)
        nonnormal <- as.numeric(nonnormal) + 1
    }

    ## Check the statistics. If necessary statistics are lacking abort
    statNames <- colnames(ContTable[[posFirstNonNullElement]])
    funcDefault <- c("n","miss","mean","sd","median","p25","p75")
    if (any(!funcDefault %in% statNames)) {

        ## summary(ContTable)
        stop("The object does not contain all necessary statistics. Use summary() method.")
    }


### Conversion of data for printing

    ## These may want to be moved to separate files later.
    ## Define a function to print a normal variable
    ConvertNormal <- function(rowMat) {
        fmt <- paste0("%.",digits,"f"," (%.",digits,"f",")")

        out <- sprintf(fmt = fmt,
                       rowMat[, "mean"],
                       rowMat[, "sd"])

        return(out)
    }
    ## Define a function to print a nonnormal variable
    ConvertNonNormal <- function(rowMat) {
        fmt <- paste0("%.",digits,"f [%.",digits,"f, %.",digits,"f]")

        out <- sprintf(fmt = fmt,
                       rowMat[, "median"],
                       rowMat[, "p25"],
                       rowMat[, "p75"])

        return(out)
    }


    ## Create a list of these two functions
    listOfFunctions <- list(normal = ConvertNormal, nonnormal = ConvertNonNormal)

    ## Take functions from the 2-element list, and convert to an nRows-length list
    listOfFunctions <- listOfFunctions[nonnormal]

    ## Loop over strata (There may be just one)
    out <- sapply(ContTable,
                  FUN = function(stratum) {

                      ## In an empty stratum, return empty
                      if (is.null(stratum)) {
                          out2 <- rep("empty", nRows)

                      } else {

                          ## Apply row by row within each non-empty stratum
                          out2 <- sapply(seq_len(nRows),
                                         FUN = function(i) {

                                             fun <- listOfFunctions[[i]]
                                             fun(stratum[i, , drop = FALSE])
                                         },
                                         simplify = TRUE)
                      }

                      ## Return
                      out2
                  },
                  simplify = FALSE)
    ## The outer sapply should not simplify to avoid a vector
    out <- do.call(cbind, out)

    ## Put the variables names back (looping over rows can solve this)
    rownames(out) <- varNames

    ## Add column names if multivariable stratification is used.
    if (length(attr(ContTable, "dimnames")) > 1) {

        colnames(out) <-
            ## Create all combinations and collapse as strings
            apply(expand.grid(attr(ContTable, "dimnames")),
                  MARGIN = 1,
                  paste0, collapse = ":")
    }

    
    ## Add p-values when requested and available
    if (test == TRUE & !is.null(attr(ContTable, "pValues"))) {

        ## nVariables x 2 (pNormal,pNonNormal) data frame
        pValues <- attr(ContTable, "pValues")

        ## Pick ones specified in nonnormal (a vector with 1s(normal) and 2s(nonnormal))
        pValues <- sapply(seq_along(nonnormal),    # loop over nonnormal
                          FUN = function(i) {
                              ## Pick from a matrix i-th row, nonnormal[i]-th column
                              ## Logical NA must be converted to a numeric
                              as.numeric(pValues[i, nonnormal[i]])
                          },
                          simplify = TRUE)

        ## Format
        fmt <- paste0("%.", pDigits, "f")
        p   <- sprintf(fmt = fmt, pValues)
        ## Column combine with the output
        out <- cbind(out, p = p)
    }

    
    ## Add mean (sd) or median [IQR] explanation if requested
    if (explain) {
        what <- c(" (mean (sd))"," (median [IQR])")[nonnormal]
        rownames(out) <- paste0(rownames(out), what)
    }

    ## Add quotes for names if requested
    if (quote) {
        rownames(out) <- paste0('"', rownames(out), '"')
        colnames(out) <- paste0('"', colnames(out), '"')
    }

    ## Print stratification
    if (length(ContTable) > 1 ) {
        cat(paste0("Stratified by ",
                   ## names(attr(ContTable, "dimnames")),
                   paste0(names(attr(ContTable, "dimnames")), collapse = ":"),
                   "\n"))
    }

    ## Print the results
    print(out, quote = quote)
}
