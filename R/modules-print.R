################################################################################
### Modules for print methods
##
## Created on: 2015-08-02
## Author: Kazuki Yoshida
################################################################################


###
### Modules common to both continuous and categorical
################################################################################

## Module to handle TRUE/FALSE or character vector of variable names
## Used for nonnormal argument and exact argument which may be logical or character
## Returns a numeric vector: 1 for default action variable; 2 for alternative action variable
ModuleHandleDefaultOrAlternative <- function(switchVec, nameOfSwitchVec, varNames) {

    ## Check the number of variables
    nVars <- length(varNames)

    ## If null, do default print/test for all variables
    if (is.null(switchVec)) {
        ##  Give one as many as there are variables
        switchVec <- rep(1, nVars)

    } else {
        ## If not null, it needs checking.

        ## Check the switchVec argument
        if (!is.logical(switchVec) & !is.character(switchVec)) {
            stop(paste0(nameOfSwitchVec, " argument has to be FALSE/TRUE or a character vector of variable names."))
        }
        ## Extend if it is a logitcal vector with one element.
        if (is.logical(switchVec)) {

            if (length(switchVec) != 1) {
                stop(paste0(nameOfSwitchVec, " has to be a logical vector of length 1"))
            }

            switchVec <- rep(switchVec, nVars)
        }
        ## Convert to a logical vector if it is a character vector
        if (is.character(switchVec)) {
            switchVec <- varNames %in% switchVec
        }
        ## Convert to numeric (1 for default action, 2 for alternative actions)
        switchVec <- as.numeric(switchVec) + 1
    }

    return(switchVec)
}


## Column name formatter
ModuleCreateStrataNames <- function(TableObject) {

    ## Create all combinations and collapse as strings
    strataNames <-  apply(expand.grid(attr(TableObject, "dimnames")),
                          MARGIN = 1,
                          paste0, collapse = ":")

    ## Return the names as a vector
    return(strataNames)
}


## A generic numeric vector formatter
ModuleFormatNumericVector <- function(x, digits, formatOptions = NULL) {

    ## Reset decimal places
    formatOptions$digits  <- digits
    formatOptions$nsmall  <- digits
    ## Only allow right justification
    formatOptions$justify <- "right"

    ## format() uses significant digits logic, so rounding is needed first
    do.call(format, c(list(x = round(x, digits = digits)),
                      formatOptions))
}

## Percentage formatter for Missing % (just an alias to keep the name)
ModuleFormatPercents <- function(percents, digits, formatOptions = NULL) {
    ModuleFormatNumericVector(percents, digits, formatOptions)
}

## p-value formatter
ModuleFormatPValues <- function(pValues, pDigits, formatOptions = NULL) {

    ## pDigits must be an integer larger than 1.
    if (pDigits < 1) {
        stop("pDigits must be an integer >= 1.")
    }

    ## If not set, set the default decimal.mark
    if (is.null(formatOptions$decimal.mark)) {
        ## getOption("OutDec") is the default used by format()
        formatOptions$decimal.mark <- getOption("OutDec")
    }

    ## Format. Some can be all zero like 0.000
    pVec <- ModuleFormatNumericVector(pValues, pDigits, formatOptions)

    ## Format 0 to obtain the all zero example to check agains.
    pVecZero <- ModuleFormatNumericVector(0, pDigits, formatOptions)

    ## Create a string like <0.001
    smallPString       <- paste0("<0",
                                 ## decimal.mark default or specified.
                                 formatOptions$decimal.mark,
                                 paste0(rep("0", pDigits - 1), collapse = ""),
                                 "1")
    ## Check positions where it is all zero like 0.000
    posAllZeros        <- (pVec == pVecZero)
    ## Put the string where it is all zero like 0.000
    pVec[posAllZeros]  <- smallPString
    ## Put a preceding space where it is not like 0.000
    pVec[!posAllZeros] <- paste0(" ", pVec[!posAllZeros])

    ## Return formatted p-values (as many as there are variables)
    return(pVec)
}


## p-value picker/formatter
ModulePickAndFormatPValues <- function(TableObject, switchVec, pDigits, formatOptions = NULL) {

    ## nVarsiables x 2 (pNormal,pNonNormal) data frame
    pValues <- attr(TableObject, "pValues")

    ## Pick ones specified in exact (a vector with 1s(approx) and 2s(exact))
    pValues <- sapply(seq_along(switchVec),    # loop over exact
                      FUN = function(i) {
                          ## Pick from a matrix i-th row, exact[i]-th column
                          ## Logical NA must be converted to a numeric
                          as.numeric(pValues[i, switchVec[i]])
                      },
                      simplify = TRUE)

    ## Return formatted p-values (as many as there are variables)
    ## e.g. <0.001 if too small to show
    ModuleFormatPValues(pValues, pDigits, formatOptions)
}


## Module to return the dimention headers added to the out 2d matrix
ModuleReturnDimHeaders <- function(TableObject) {

    ## Add stratification information to the column header
    if (length(TableObject) > 1) {
        ## Create strata string
        strataString <- paste0("Stratified by ", attr(TableObject, "strataVarName"))

        ## Name the row dimension with it. 1st dimension name should be empty.
        dimHeaders <- c("", strataString)

    }  else {
        ## If no stratification, no name for the second dimension
        dimHeaders <- c("", "")
    }

    ## Return the dim header a vector of length 2
    return(dimHeaders)
}


## Module to mid justify column names considering max width
ModuleMidJustifyColnames <- function(mat) {

    ## Extract column names
    colNames <- colnames(mat)

    ## Widths of column names
    widthsColNames <- nchar(colNames)

    ## Obtain max width for each column
    maxWidths <- unlist(lapply(seq_len(ncol(mat)), function(i) {
        max(nchar(mat[,i]))
    }))

    ## Half of the difference should be padded to the left.
    nPads <- ceiling((maxWidths - widthsColNames) / 2)
    ## Do not allow negative numbers
    nPads <- nPads * as.numeric(nPads >= 0)

    ## Create a vector of padding spaces
    pads <- unlist(lapply(nPads, function(n) {
        ifelse(n > 0,
               paste0(rep(" ", n), collapse = ""),
               "")
    }))

    ## Manipulate
    colnames(mat) <- paste0(pads, colNames)

    ## Return matrix
    mat
}


## Module to remove spaces from the result matrix
ModuleRemoveSpaces <- function(mat, noSpaces) {

    ## Carry out these replacements to remove spaces if asked
    if (noSpaces) {
        mat <- gsub(pattern = "^ *| *$", replacement = "",  x = mat)
        mat <- gsub(pattern = "\\( *",   replacement = "(", x = mat)
    }

    ## Return the matrix
    mat
}


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



###
### Continuous variable formatters
################################################################################

## Define a function to format a normal variable
ModuleConvertNormal <- function(rowMat, digits, formatOptions = NULL) {

    ## Suppress leading blanks for justification for (SD)
    formatOptions$trim <- TRUE

    ## Create a DF with numeric mean column and character (SD) column
    ## Turn off trim, TODO: maybe add decimal adjustment later
    ## No need to round col1, ModuleContFormatStrata should do this after
    ## stacking up means and medians.
    ## unname() not to leave mean as a data frame row name.
    data.frame(col1 = unname(rowMat[,"mean"]),
               col2 = paste0(" (",
                             ModuleFormatNumericVector(rowMat[,"sd"], digits, formatOptions),
                             ")"),
               stringsAsFactors = FALSE)
}

## Define a function to format a nonnormal variable
ModuleConvertNonNormal <- function(rowMat, digits, minMax = FALSE, formatOptions = NULL) {

    ## Suppress leading blanks for justification for [IQR] and [min, max]
    formatOptions$trim <- TRUE

    ## No need to round col1, ModuleContFormatStrata should do this after
    ## stacking up means and medians.

    if (minMax == FALSE) {
        ## Create a DF with numeric median column and character [p25, p75] column
        ## unname() not to leave median as a data frame row name.
        out <- data.frame(col1 = unname(rowMat[,"median"]),
                          col2 = paste0(" [",
                                        ModuleFormatNumericVector(rowMat[,"p25"], digits, formatOptions),
                                        ", ",
                                        ModuleFormatNumericVector(rowMat[,"p75"], digits, formatOptions),
                                        "]"),
                          stringsAsFactors = FALSE)
    } else if (minMax == TRUE) {
        ## Create a DF with numeric median column and character [min, max] column
        ## unname() not to leave median as a data frame row name.
        out <- data.frame(col1 = unname(rowMat[,"median"]),
                          col2 = paste0(" [",
                                        ModuleFormatNumericVector(rowMat[,"min"], digits, formatOptions),
                                        ", ",
                                        ModuleFormatNumericVector(rowMat[,"max"], digits, formatOptions),
                                        "]"),
                          stringsAsFactors = FALSE)
    } else {
        stop("minMax must be a logical vector of one: FALSE or TRUE")
    }

    return(out)
}


## Module to loop over strata formatting continuous variables
## No variable level looping here as each stratum is a matrix of all variables
ModuleContFormatStrata <- function(ContTable, nVars, listOfFunctions, digits, formatOptions) {

    ## Return a formatted table looping over strata
    sapply(ContTable,
           ## Each stratum is a matrix containing summaries
           ## One row is one variable
           FUN = function(stratum) {

               ## In an empty stratum, return empty
               if (is.null(stratum)) {

                   out <- rep("-", nVars)
                   ## Give NA to the width of the mean/median column in characters
                   nCharMeanOrMedian <- NA

               } else {

                   ## Apply row by row within each non-empty stratum
                   ## This row-by-row operation is necessary to handle mean (SD) and median [IQR]
                   out <- sapply(seq_len(nVars),
                                 FUN = function(i) {

                                     ## Choose between normal or nonnormal function
                                     fun <- listOfFunctions[[i]]
                                     ## Convert a row matrix to 1x2 df (numeric, character)
                                     fun(stratum[i, , drop = FALSE])

                                     ## Create a 1-row DF (numeric, character)
                                 },
                                 simplify = FALSE)

                   ## nx2 data frame by row binding multiple 1-row data frames
                   out <- do.call(rbind, out)

                   ## Format decimal places and decimal mark (+ additonal format options)
                   ## right justify by adding spaces (to align at the decimal point of mean/median)
                   formatOptions$justify <- "right"
                   ## ModuleFormatNumericVector performs rounding.
                   out$col1 <- ModuleFormatNumericVector(out$col1, digits, formatOptions)

                   ## Obtain the width of the mean/median column in characters
                   nCharMeanOrMedian <- nchar(out$col1[1])

                   ## Create mean (SD) or median [p25, p75] as a character vector
                   out <- do.call(paste0, out)
               }

               ## Add attributes
               attributes(out) <- c(attributes(out),
                                    list(nCharMeanOrMedian = nCharMeanOrMedian))

               ## Return
               out
           },
           simplify = FALSE)
}



###
### Categorical variable formatters
################################################################################

## Module to loop over variables within a stratum formatting categorical variables
ModuleCatFormatVariables <- function(lstVars, varsToFormat, digits, level, cramVars, dropEqual, showAllLevels, formatOptions) {

    ## Loop over variables within a stratum
    ## Each list element is a data frame summarizing levels
    sapply(X = seq_along(lstVars),
           FUN = function(i) {

               ## Extract the data frame (list element)
               DF <- lstVars[[i]]

               ## Extract the variable name
               varName <- names(lstVars)[i]

               ## Check number of rows (levels)
               nRow <- nrow(DF)

               ## Add a variable name to the left as a character vector
               DF <- cbind(var = rep(varName, nRow), DF)

               ## Format percent and cum.percent (specified via varsToFormat) as strings
               formatOptions$trim <- TRUE
               DF[varsToFormat] <- lapply(X = DF[varsToFormat],
                                          FUN = function(x) {
                                              ModuleFormatNumericVector(x,
                                                                        digits,
                                                                        formatOptions)
                                          })

               ## Make all variables strings (if freq is an integer, direct convert is ok)
               DF[] <- lapply(X = DF, FUN = as.character)

               ## Add first row indicator column
               DF$firstRowInd <- ""
               ## Add crammed row indicator column
               DF$crammedRowInd <- ""


               ## Format based on the number of levels
               if (!showAllLevels & nRow == 1) {
                   ## If showAllLevels is FALSE AND there are only ONE levels,
                   ## change variable name to "var = level"
                   DF$var <- with(DF, paste0(var, " = ", level))

               } else if (!showAllLevels & nRow == 2) {
                   ## If showAllLevels is FALSE AND there are only TWO levels,
                   ## cram two levels in one row if requested
                   if (unique(DF$var) %in% cramVars) {
                       ## If cramVars includes var, cram into one line
                       ## Cram two freq and count with / in between
                       DF$freq    <- paste0(DF$freq,    collapse = "/")
                       DF$percent <- paste0(DF$percent, collapse = "/")
                       ## change variable name, and delete the first level.
                       DF$var     <- paste0(DF$var, " = ",
                                            paste0(DF$level, collapse = "/"))
                       ## Delete the first row
                       DF <- DF[-1, , drop = FALSE]
                       ## Add crammed row indicator (used for formatting)
                       DF[1,"crammedRowInd"] <- "crammed"

                   } else {
                       ## Otherwise, keep the second level only.
                       ## Change variable name if dropEqual = FALSE.
                       if (!dropEqual) {
                           DF$var <- sprintf("%s = %s", DF$var, DF$level)
                       }
                       ## Delete the first level.
                       DF <- DF[-1, , drop = FALSE]
                   }

               } else if (!showAllLevels & nRow > 2) {
                   ## If showAllLevels is FALSE AND there are MORE THAN two levels,
                   ## add an empty row and put the var name, then levels below.
                   DF <- rbind(rep("", ncol(DF)), DF)
                   ## Add variable name in the first row
                   DF[1,"var"] <- DF[2,"var"]

                   ## 2nd to last have level names. (nrow has changed by +1)
                   secondToLastRows <- seq(from = 2,to = nrow(DF), by = 1)
                   DF[secondToLastRows, "var"] <-
                                             paste0("   ", DF[secondToLastRows, "level"]) # preceding spaces

               } else if (showAllLevels) {
                   ## If showAllLevels is TRUE, clear these except in 1st row
                   DF[-1, c("var","n","miss","p.miss")] <- ""
               }

               ## Add first row indicator (used to add (%))
               DF[1,"firstRowInd"]   <- "first"

               ## Return a data frame
               DF
           },
           simplify = FALSE) # Looped over variables (list element is DF)
}


## Module to loop over strata formatting categorical variables
ModuleCatFormatStrata <- function(CatTable, digits, varsToFormat, cramVars, dropEqual, showAllLevels, formatOptions = NULL) {

    ## Obtain collpased result
    CatTableCollapsed <-
    ## Loop over strata extracting list of variables
    sapply(X = CatTable,
           FUN = function(lstVars) {

               ## Do the following formatting only if the stratum is non-null. Do nothing if null.
               if (!is.null(lstVars)) {
                   ## Returns an empty list if the stratum is null (empty).

                   ## Loop over list of variables formatting them
                   lstVarsFormatted <-
                   ModuleCatFormatVariables(lstVars       = lstVars,
                                            varsToFormat  = varsToFormat,
                                            digits        = digits,
                                            cramVars      = cramVars,
                                            dropEqual     = dropEqual,
                                            showAllLevels = showAllLevels,
                                            formatOptions = formatOptions)


                   ## Collapse DFs within each stratum
                   DF <- do.call(rbind, lstVarsFormatted)

                   ## Justification should happen here after combining variable DFs into a stratum DF.
                   ## Check non-empty rows
                   posNonEmptyRows <- DF$freq != ""


                   ## Create freq to be added on to the right side within ()
                   DF$freqAddOn <- DF$freq
                   ## Right justify frequency (crammed and non-crammed at once)
                   DF$freq <- format(DF$freq, justify = "right")
                   ## Right justify frequency (non-crammed only)
                   DF[DF$crammedRowInd == "","freqAddOn"] <-
                                                        format(DF[DF$crammedRowInd == "","freqAddOn"], justify = "right")
                   ## Obtain the max width of characters
                   nCharFreq <- max(nchar(DF$freq))


                   ## Create percent to be added on to the right side within ()
                   DF$percentAddOn <- DF$percent
                   ## Right justify percent (crammed and non-crammed at once)
                   DF$percent <- format(DF$percent, justify = "right")
                   ## Right justify percent (non-crammed only)
                   DF[DF$crammedRowInd == "","percentAddOn"] <-
                                                           format(DF[DF$crammedRowInd == "","percentAddOn"], justify = "right")
                   ## Obtain the max width of characters
                   nCharPercent <- max(nchar(DF$percent))


                   ## Add freq (percent) column (only in non-empty rows)
                   DF$freqPer <- ""
                   DF[posNonEmptyRows,]$freqPer <- sprintf(fmt = "%s (%s) ",
                                                           DF[posNonEmptyRows,]$freq,
                                                           DF[posNonEmptyRows,]$percentAddOn)

                   ## Add percent (freq) column  (only in non-empty rows)
                   DF$perFreq <- ""
                   DF[posNonEmptyRows,]$perFreq <- sprintf(fmt = "%s (%s) ",
                                                           DF[posNonEmptyRows,]$percent,
                                                           DF[posNonEmptyRows,]$freqAddOn)

                   ## Add aditional attributes
                   attributes(DF) <- c(attributes(DF),
                                       list(nCharFreq    = nCharFreq,
                                            nCharPercent = nCharPercent))

                   ## Return a data frame (2014-02-12 sapply breaks attributes?)
                   DF
               } # end of non-null condition (Null strata skip all this. No action.)

           }, simplify = FALSE)

    CatTableCollapsed
}


## Obtain a vector indictor showing n-th variable's
## correspondence row(s) in  FmtCatTable
ModuleVarToRowFmtCatTable <- function(spcFmtEltTables) {

    ## If no categorical elements, return NULL
    if (!("FmtCatTable" %in% names(spcFmtEltTables))) {
        return(NULL)
    }

    ## Extract logical vector of which rows are title rows
    logiNameRows <- attr(spcFmtEltTables$FmtCatTable, "logiNameRows")

    ## Create a numeric representation of which row(s) belong to which variable
    numNameRows <- as.numeric(logiNameRows)
    numNameRows[logiNameRows] <- seq_len(sum(logiNameRows))
    numNameRows[!logiNameRows] <- NA

    ## LOCF for subheaders (some variables have multiple rows)
    numNameRows <- zoo::na.locf(numNameRows, na.rm = FALSE)

    ## First element is always sample size and should be 0 to avoid NA,
    ## which breaks == use
    numNameRows[1] <- 0
    numNameRows
}


###
### Modules for unifying continuous and categorical tables
################################################################################

## Extract stratumSizesRow from vecColWidths attribute of a FmtTable object
## Used by print.(svy)TableOne()
ModuleStratumSizesRow <- function(FmtTable, showAllLevels) {

    ## Length of vecColWidths is number of strata
    nStrata <- length(attr(FmtTable, "vecColWidths"))

    ## Extract the sample size row from Fmt*Table
    stratumSizesRow <- FmtTable[1, , drop = FALSE]


    ## showAllLevels indicates if level column exists
    if (showAllLevels) {

        ## Teke first nStrata columns after level column (position 1)
        vecColWidths <- nchar(stratumSizesRow[1, 1 + seq_len(nStrata)])
    } else {

        ## Teke first nStrata columns
        vecColWidths <- nchar(stratumSizesRow[1, seq_len(nStrata)])
    }

    ## Add
    attr(stratumSizesRow, "vecColWidths") <- vecColWidths

    ## Return a single row matrix with vecColWidths attribute
    stratumSizesRow
}


## Given a list of tables with vecColWidths,
## return a strata-by-table df containing spaces to add
ModuleNSpacesToAdd <- function(FmtElementTables) {
    ##
    ## Get the column width information for each object
    ## each object has widths as many as strata
    colWidthInfo <- sapply(FmtElementTables,
                           FUN = function(matObj) {

                               attributes(matObj)$vecColWidths
                           },
                           simplify = FALSE)
    ## list to df
    colWidthInfo <- as.data.frame(colWidthInfo)

    ## Get the max values for each stratum
    vecMaxValues <- do.call(function(...) {pmax(..., na.rm = TRUE)}, colWidthInfo)

    ## Get the difference (value - max. Must be negated)
    nSpacesToAdd <- sweep(x      = colWidthInfo,
                          MARGIN = 1,
                          STATS  = vecMaxValues,
                          FUN    = "-")
    ## Make sure these negative numbers are made positive
    nSpacesToAdd <- abs(nSpacesToAdd)
    ## Get rid of NA, so that it does not cause problem in rep() as a times argument
    nSpacesToAdd[is.na(nSpacesToAdd)] <- 0
    nSpacesToAdd
}


## Add spaces to table columns as specified in nSpacesToAdd and considering showAllLevels
ModuleAddSpacesToTable <- function(FmtElementTables, nSpacesToAdd, showAllLevels) {
    ## For each matrix, add spaces
    spaceFormattedTables <-
    sapply(seq_along(FmtElementTables),
           FUN = function(i) {

               ## For i-th table
               matObj  <- FmtElementTables[[i]]
               nSpaces <- nSpacesToAdd[, i]

               ## For j-th stratum (column), add spaces.
               ## Be aware of the p-value column (last. not included in first palce)
               ## and level column (1st. explicitly ignore).
               for (j in seq_along(nSpaces)) {

                   ## If showAllLevels is requested, ignore the first column (level column).
                   if (showAllLevels) {
                       matObj[, (j + 1)] <- paste0(paste0(rep(" ", nSpaces[j]), collapse = ""),
                                                   matObj[, (j + 1)])

                   } else {
                       ## if not, no need to ignore the first column
                       matObj[, j] <- paste0(paste0(rep(" ", nSpaces[j]), collapse = ""),
                                             matObj[, j])
                   }

               }

               ## Return the adjusted table
               matObj
           },
           simplify = FALSE)

    ## Restore names for easy acces
    names(spaceFormattedTables) <- names(FmtElementTables)
    spaceFormattedTables
}


## Extract Cont/CatTable elements of x and dispatch print() appropriately
ModuleFormatTables <- function(x, catDigits, contDigits,
                               ## Generic arguments passed
                               test, smd, missing,
                               explain, pDigits,
                               ## print.CatTable arguments passed
                               format, exact,
                               showAllLevels, cramVars, dropEqual,
                               ## print.ContTable arguments passed
                               nonnormal, minMax, insertLevel,
                               formatOptions
                               ) {

    ## Two-element list(ContTable, CatTable)
    ## Cont first throughout this function
    TableOne <- list(ContTable = x$ContTable,
                     CatTable  = x$CatTable)
    ## Drop NULL element
    TableOne <- TableOne[!sapply(TableOne, is.null)]
    if (length(TableOne) == 0) {
        warning("This object does not have valid ContTable or CatTable")
    }

    ## Get the Cont/Cat status (1st of classes)
    ## Always (ContTable, CatTable) by new definition
    classOfTables <- sapply(TableOne, class)[1,]

    ## Decimal point vector; pick appropriately depending on class
    contCatDigits <- c(ContTable    = contDigits,
                       CatTable     = catDigits,
                       svyContTable = contDigits,
                       svyCatTable  = catDigits)[classOfTables]

    ## Get the formatted tables (FmtContTable, FmtCatTable)
    FmtTables <-
    sapply(seq_along(TableOne),
           ## loop over ContTable and CatTable
           FUN = function(i) {

               ## print.CatTable or print.ContTable called depending on the class
               print(TableOne[[i]],
                     ## Number of digits depends on Cont or CatTable
                     digits = contCatDigits[i],

                     ## Do not print
                     printToggle = FALSE,

                     ## The rests are just passed
                     ## generic arguments passed
                     test = test, smd = smd, missing = missing,
                     explain = explain, pDigits = pDigits,

                     ## print.CatTable arguments passed
                     format = format, exact = exact,
                     showAllLevels = showAllLevels,  # Returns one more column if TRUE
                     cramVars = cramVars,
                     dropEqual = dropEqual,

                     ## print.ContTable arguments passed
                     nonnormal = nonnormal, minMax = minMax, insertLevel = showAllLevels,

                     ## formatOptions passed
                     formatOptions = formatOptions
                     )  # Method dispatch at work
           },
           simplify = FALSE)
    ## Name formatted tables for easier access (Cont first!)
    names(FmtTables) <- paste0("Fmt", names(TableOne))

    FmtTables
}


## Create a list of one variable tables excluding sample size row
ModuleListOfOneVarTables <- function(spcFmtEltTables, MetaData) {

    ## Obtain a vector indictor showing n-th variable's
    ## correspondence row(s) in  FmtCatTable
    vecVarToRow <- ModuleVarToRowFmtCatTable(spcFmtEltTables)

    ## Pick elements and construct a list of rows to rbind
    ## loop over vars picking elements from appropriate objects
    lstOneVarTables <- lapply(seq_along(MetaData$vars), function(i) {

        ## Extract current elements
        var         <- MetaData$vars[i]
        logiFactor  <- MetaData$logiFactors[i]

        ## Conditional on if its logical
        if (logiFactor) {
            ## If cat
            nthElt <- which(var == MetaData$varFactors)
            rowsToPick <- which(nthElt == vecVarToRow)

            spcFmtEltTables$FmtCatTable[rowsToPick, , drop = FALSE]

        } else {
            ## If Cont
            nthElt <- which(var == MetaData$varNumerics)

            ## + 1 because of sample size row
            spcFmtEltTables$FmtContTable[nthElt + 1, , drop = FALSE]
        }
    })
    lstOneVarTables
}
