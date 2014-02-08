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
            nonnormal <- varNames %in% nonnormal
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


    ## Obtain the strata sizes in a character vector. This has to be obtained from the original data
    ## Added as the top row later
    strataN <- sapply(ContTable,
                      FUN = function(stratum) { # loop over strata
                          ## Just the first available element may be enough.
                          ## Obtain n from all variables and all levels, and get the mean
                          n <- stratum[,"n"]
                          ## Pick the first non-null element
                          n[!is.null(n)][1]
                          ## Convert NULL to N
                          ifelse(is.null(n), "0", as.character(n))
                      },
                      simplify = TRUE)

    ## Provide indicators to show what columns were added.
    wasPValueColumnAdded <- FALSE



### Conversion of data for printing

    ## These may want to be moved to separate files later.
    ## Define a function to print a normal variable
    ConvertNormal <- function(rowMat) {

        ## Format for SD
        fmt <- paste0(" (%.", digits,"f",")")

        ## Create a DF with numeric mean column and character (SD) column
        data.frame(col1 = rowMat[,"mean"],
                   col2 = sprintf(fmt = fmt, rowMat[,"sd"]),
                   stringsAsFactors = FALSE)
    }
    ## Define a function to print a nonnormal variable
    ConvertNonNormal <- function(rowMat) {
        ## Format for [p25, p75]
        fmt <- paste0(" [%.", digits,"f, %.",digits,"f]")

        ## Create a DF with numeric median column and character [p25, p75] column
        data.frame(col1 = rowMat[,"median"],
                   col2 = sprintf(fmt = fmt, rowMat[,"p25"], rowMat[,"p75"]),
                   stringsAsFactors = FALSE)
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
                          out2 <- rep("-", nRows)

                      } else {

                          ## Apply row by row within each non-empty stratum
                          ## This row-by-row operation is necessary to handle mean (sd) and median [IQR]
                          out2 <- sapply(seq_len(nRows),
                                         FUN = function(i) {

                                             ## Choose between normal or nonnormal function
                                             fun <- listOfFunctions[[i]]
                                             ## Convert a row matrix to 1x2 df (numeric, character)
                                             fun(stratum[i, , drop = FALSE])

                                             ## Create a 1-row DF (numeric, character)
                                         },
                                         simplify = FALSE)

                          ## nx2 data frame by row binding multiple 1-row data frames
                          out2 <- do.call(rbind, out2)

                          ## Format for decimals
                          out2$col1 <- sprintf(fmt = paste0("%.", digits, "f"), out2$col1)

                          ## right justify by adding spaces (to align at the decimal point of mean/median)
                          out2$col1 <- format(out2$col1, justify = "right")

                          ## Create mean (SD) or median [p25, p75] as a character vector
                          out2 <- do.call(paste0, out2)
                      }

                      ## Return
                      out2
                  },
                  simplify = FALSE)
    ## The outer sapply should not simplify to avoid a vector
    ## Column-bind to create variables x strata matrix
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

        ## Create a string like <0.001
        smallPString <- paste0("<0.", paste0(rep("0", pDigits - 1), collapse = ""), "1")
        ## Check positions where it is all zero like 0.000
        posAllZeros <- grepl("^0\\.0*$", p)
        ## Put the string where it is all zero like 0.000
        p[posAllZeros] <- smallPString
        ## Put a preceding space where it is not like 0.000
        p[!posAllZeros] <- paste0(" ", p[!posAllZeros])

        ## Column combine with the output
        out <- cbind(out, p = p)

        ## Change the indicator
        wasPValueColumnAdded <- TRUE
    }


    ## Add mean (sd) or median [IQR] explanation if requested
    if (explain) {
        what <- c(" (mean (sd))"," (median [IQR])")[nonnormal]
        rownames(out) <- paste0(rownames(out), what)
    }

    ## Keep column names (strataN does not have correct names if stratification is by multiple variables)
    outColNames <- colnames(out)
    ## Add n at the correct location depending on the number of columns added (level and/or p)
    out <- rbind(n = c(strataN,
                     p = rep("", wasPValueColumnAdded) # Add "" padding if p-value added
                     ),
                 out)
    ## Put back the column names (overkill for non-multivariable cases)
    colnames(out) <- outColNames

    ## Add stratification information to the column header
    if (length(ContTable) > 1 ) {
        ## Create strata string
        strataString <- paste0("Stratified by ",
                               paste0(names(attr(ContTable, "dimnames")), collapse = ":"))

        ## Name the row dimension with it. 1st dimension name should be empty.
        names(dimnames(out)) <- c("", strataString)
    }

    ## Add quotes for names if requested
    if (quote) {
        rownames(out) <- paste0('"', rownames(out), '"')
        colnames(out) <- paste0('"', colnames(out), '"')
        names(dimnames(out)) <- paste0('"', names(dimnames(out)), '"')
    }

    ## Print the results
    print(out, quote = quote)

    ## Return invisibly
    return(invisible(out))
}
