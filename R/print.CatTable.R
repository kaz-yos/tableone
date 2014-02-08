## Print method for a continuous table
print.CatTable <- function(CatTable, missing = FALSE,
                           format = c("fp","f","p","pf")[1], # Format f_requency and/or p_ercent
                           digits = 1, exact = NULL, quote = FALSE,
                           test = TRUE, pDigits = 3,
                           showAllLevels = FALSE,
                           explain = TRUE,
                           CrossTable = FALSE) {

### Check the data structure first

    ## CatTable has a strata(list)-variable(list)-table(dataframe) structure
    ## Get the position of the non-null element
    logiNonNullElement <- !sapply(CatTable, is.null)
    ## Stop if all elements are null.
    if (sum(logiNonNullElement) == 0) {stop("All strata are null strata. Check data.")}
    ## Get the first non-null position
    posFirstNonNullElement <- which(logiNonNullElement)[1]
    ## Save variable names using the first non-null element
    varNames <- names(CatTable[[posFirstNonNullElement]])
    ## Check the number of variables (list length)
    nVar <- length(varNames)

    ## If null, do approx
    if (is.null(exact)) {

        exact <- rep(1, nVar)

    } else {
        ## If not null, it needs checking.

        ## Check the exact argument
        if (!is.logical(exact) & !is.character(exact)) {
            stop("exact argument has to be FALSE/TRUE or character.")
        }
        ## Extend if it is a logitcal vector with one element.
        if (is.logical(exact)) {

            if (length(exact) != 1) {
                stop("exact has to be a logical vector of length 1")
            }

            exact <- rep(exact, nVar)
        }
        ## Convert to a logical vector if it is a character vector
        if (is.character(exact)) {
            exact <- varNames %in% exact
        }
        ## Convert to numeric (1 for approx, 2 for exact)
        exact <- as.numeric(exact) + 1
    }

    ## Check format argument. If it is broken, choose "fp" for frequency (percent)
    if (!length(format) == 1  | !format %in% c("fp","f","p","pf")) {
        warning("format only accepts one of fp, f, p, or pf. Choosing fp.")
        format <- "fp"
    }

    ## Obtain the strata sizes in a character vector. This has to be obtained from the original data
    ## Added as the top row later
    strataN <- sapply(CatTable,
                      FUN = function(stratum) { # loop over strata
                          ## Just the first available element may be enough.
                          ## Obtain n from all variables and all levels, and get the mean
                          n <- unlist(sapply(stratum, getElement, "n"))
                          ## Pick the first non-null element
                          n[!is.null(n)][1]
                          ## Convert NULL to N
                          ifelse(is.null(n), "0", as.character(n))
                      },
                      simplify = TRUE)

    ## Provide indicators to show what columns were added.
    wasLevelColumnAdded  <- FALSE
    wasPValueColumnAdded <- FALSE
    wasExactColumnAdded  <- FALSE


### Formatting for printing

    ## Create format for percent used in the loop
    fmt1 <- paste0("%.", digits, "f")

    ## Obtain collpased result
    CatTableCollapsed <-
        sapply(X = CatTable,   # Loop over strata
               FUN = function(LIST) {

                   ## Do the following formatting only if the stratum is non-null. Do nothing if null.
                   if (!is.null(LIST)) {

                       ## Returns an empty list if the stratum is null (empty).
                       LIST <- sapply(X = seq_along(LIST), # Loop over variables (list element is DF)
                                      FUN = function(i) {

                                          ## Extract the data frame (list element)
                                          DF <- LIST[[i]]

                                          ## Extract the variable name
                                          varName <- names(LIST)[i]

                                          ## Check number of rows (levels)
                                          nRow <- nrow(DF)

                                          ## Add a variable name to the left as a character vector
                                          DF <- cbind(var = rep(varName, nRow),
                                                      DF)

                                          ## Format percent and cum.percent as strings
                                          DF[c("percent","cum.percent")] <-
                                              lapply(X = DF[c("percent","cum.percent")],
                                                     FUN = sprintf,
                                                     fmt = fmt1)


                                          ## Make all variables strings (freq is an integer, so direct convert ok)
                                          DF[] <- lapply(X = DF, FUN = as.character)

                                          ## Add first row indicator column
                                          DF$firstRowInd <- ""

                                          ## Format based on the number of levels
                                          if (!showAllLevels & nRow == 1) {
                                              ## If showAllLevels is FALSE AND there are only ONE levels,
                                              ## change variable name to "var = level"
                                              DF$var <- with(DF, paste0(var, " = ", level))
                                              ## Add first row indicator (used to add (%))
                                              DF[1,"firstRowInd"] <- "first"

                                          } else if (!showAllLevels & nRow == 2) {
                                              ## If showAllLevels is FALSE AND there are only TWO levels,
                                              ## change variable name, and delete the first level.
                                              DF$var <- with(DF, paste0(var, " = ", level))
                                              DF <- DF[-1, , drop = FALSE]
                                              ## Add first row indicator (used to add (%))
                                              DF[1,"firstRowInd"] <- "first"

                                          } else if (!showAllLevels & nRow > 2) {
                                              ## If showAllLevels is FALSE AND there are MORE THAN two levels,
                                              ## add an empty row and put the var name, then levels below.
                                              DF <- rbind(rep("", ncol(DF)),
                                                          DF)
                                              ## Add variable name in the first row
                                              DF[1,"var"] <- DF[2,"var"]

                                              ## 2nd to last have level names. (nrow has changed by +1)
                                              secondToLastRows <- seq(from = 2,to = nrow(DF), by = 1)
                                              DF[secondToLastRows, "var"] <-
                                                  paste0("   ", DF[secondToLastRows, "level"]) # preceding spaces
                                              ## Add first row indicator (used to add (%))
                                              DF[1,"firstRowInd"] <- "first"

                                          } else if (showAllLevels) {
                                              ## If showAllLevels is TRUE clear names
                                              DF[-1, c("var","n","miss")] <- ""
                                              ## Add first row indicator (used to add (%))
                                              DF[1,"firstRowInd"] <- "first"
                                          }

                                          ## Return a data frame
                                          DF
                                      },
                                      simplify = FALSE)


                       ## Collapse DFs within each stratum
                       DF <- do.call(rbind, LIST)

                       ## Justification should happen here after combining variable DFs into a stratum DF.
                       ## Check non-empty rows
                       posNonEmptyRows <- DF$freq != ""

                       ## Right justify frequency
                       DF$freq <- format(DF$freq, justify = "right")
                       ## Right justify percent
                       DF$percent <- format(DF$percent, justify = "right")

                       ## Add freq (percent) column (only in non-empty rows)
                       DF$freqPer <- ""
                       DF[posNonEmptyRows,]$freqPer <- sprintf(fmt = "%s (%s) ",
                                                               DF[posNonEmptyRows,]$freq,
                                                               DF[posNonEmptyRows,]$percent)

                       ## Add percent (freq) column  (only in non-empty rows)
                       DF$perFreq <- ""
                       DF[posNonEmptyRows,]$perFreq <- sprintf(fmt = "%s (%s) ",
                                                               DF[posNonEmptyRows,]$percent,
                                                               DF[posNonEmptyRows,]$freq)

                       ## Return a data frame
                       DF
                   } # end of non-null condition (Null strata skip all this. No action.)

               }, simplify = FALSE)


    ## Fill the null element using the first non-null element's dimension (Make sure to erase data)
    CatTableCollapsed[!logiNonNullElement] <- CatTableCollapsed[posFirstNonNullElement]
    ## Access the filled-in data frames, and erase them with place holders.
    for (i in which(!logiNonNullElement)) {
        ## Replace all elements with a place holder variable by variable
        CatTableCollapsed[[i]][] <- lapply(CatTableCollapsed[[i]][],
                                           function(var) {

                                               var <- rep("-", length(var))
                                           })
    }


    ## Choose the column name for the right format
    nameResCol <- c("freqPer","freq","percent","perFreq")[format == c("fp","f","p","pf")]


    ## Create output matrix without variable names with the right format
    out <- do.call(cbind, lapply(CatTableCollapsed, getElement, nameResCol))
    out <- as.matrix(out)

    ## Add column names if multivariable stratification is used. (No column names added automatically)
    if (length(attr(CatTable, "dimnames")) > 1) {

        colnames(out) <-
            ## Create all combinations and collapse as strings. 1st variable cycles fastest.
            apply(expand.grid(attr(CatTable, "dimnames")),
                  MARGIN = 1,
                  paste0, collapse = ":")
    }

    ## Set the variables names
    rownames(out) <- CatTableCollapsed[[posFirstNonNullElement]][,"var"]
    ## Get positions of rows with variable names
    logiNonEmptyRowNames <- CatTableCollapsed[[posFirstNonNullElement]][, "firstRowInd"] != ""



    ## Add level names if showAllLevels is TRUE. This adds the level column. Need come after column naming.
    if (showAllLevels) {
        out <- cbind(level = CatTableCollapsed[[posFirstNonNullElement]][,"level"], # Cannot be DF
                     out)
        ## Changed the indicator
        wasLevelColumnAdded  <- TRUE
    }


    ## Add p-values when requested and available
    if (test == TRUE & !is.null(attr(CatTable, "pValues"))) {

        ## nVariables x 2 (pNormal,pNonNormal) data frame
        pValues <- attr(CatTable, "pValues")

        ## Pick ones specified in exact (a vector with 1s(approx) and 2s(exact))
        pValues <- sapply(seq_along(exact),    # loop over exact
                          FUN = function(i) {
                              ## Pick from a matrix i-th row, exact[i]-th column
                              ## Logical NA must be converted to a numeric
                              as.numeric(pValues[i, exact[i]])
                          },
                          simplify = TRUE)

        ## Pick test types used
        testTypes <- c("","exact")[exact]

        ## Format p value
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

        ## Create an empty p-value column and test column
        out <- cbind(out,
                     p     = rep("", nrow(out))) # Column for p-values
        ## Put the values at the non-empty positions
        out[posNonEmptyRowNames,"p"] <- p

        ## Change the indicator
        wasPValueColumnAdded <- TRUE


        ## If exact test is used at least onece, add a test type indicator.
        if (any(exact == 2)) {
            ## Create an empty test type column
            out <- cbind(out,
                         test = rep("", nrow(out))) # Column for test types

            ## Put the test types  at the non-empty positions
            out[posNonEmptyRowNames,"test"] <- testTypes

            ## Change the indicator
            wasExactColumnAdded <- TRUE
        }
    }


    ## Add freq () explanation if requested
    if (explain) {
        ## Choose the format of the explanation string
        explainString <- c(" (%)", "", " (%)", " % (freq)")[format == c("fp","f","p","pf")]
        ## Only for rows with row names
        rownames(out)[posNonEmptyRowNames] <- paste0(rownames(out)[posNonEmptyRowNames],
                                                     explainString)
    }

    ## Keep column names (strataN does not have correct names if stratification is by multiple variables)
    outColNames <- colnames(out)
    ## Add n at the correct location depending on the number of columns added (level and/or p)
    out <- rbind(n = c(level = rep("", wasLevelColumnAdded), # Add "" padding if level added
                     strataN,
                     p       = rep("", wasPValueColumnAdded), # Add "" padding if p-value added
                     test    = rep("", wasExactColumnAdded)   # Add "" padding if exact test used
                     ),
                 out)
    ## Put back the column names (overkill for non-multivariable cases)
    colnames(out) <- outColNames

    ## Add stratification information to the column header (This is also in the constructor)
    if (length(CatTable) > 1 ) {
        ## Combine variable names with : in between
        strataVarName <- attributes(CatTable)$strataVarName

        ## Create strata string
        strataString <- paste0("Stratified by ", strataVarName)

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

    ## Print CrossTable() if requested
    if (CrossTable) {
        require(gmodels)

        junk <- lapply(attributes(CatTable)$xtabs, CrossTable)
    }

    ## Return invisibly
    return(invisible(out))
}
