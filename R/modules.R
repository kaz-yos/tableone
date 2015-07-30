################################################################################
### Modules for tableone
## This file contains common modules used in the tableone package.
## Created on: 2014-02-10
## Author: Kazuki Yoshida
################################################################################

### Modules intended for the constructors
################################################################################
## Check if the data given is a data frame
ModuleStopIfNotDataFrame <- function(data) {

    if (is.data.frame(data) == FALSE) {
        stop("The data argument needs to be a data frame (no quote).")
    }
}

## Extract variables that exist in the data frame
## Also exclude variables that only have NA
ModuleReturnVarsExist <- function(vars, data) {

    ## Check if variables exist. Drop them if not.
    varsNotInData <- setdiff(vars, names(data))

    if (length(varsNotInData) > 0) {
        warning("The data frame does not have: ",
                paste0(varsNotInData, sep = " "), " Dropped")
        ## Only keep variables that exist
        vars <- intersect(vars, names(data))
    }

    ## Check if variables have at least some valid values (not NA/NaN)
    logiAllNaVars <- sapply(X   = data[vars],
                            FUN = function(VAR) {
                                all(is.na(VAR))
                            },
                            simplify = TRUE)

    if (any(logiAllNaVars)) {
        warning("These variables only have NA/NaN: ",
                paste0(vars[logiAllNaVars], sep = " "), " Dropped")

        vars <- vars[!logiAllNaVars]
    }

    ## Return existing and valid variables
    return(vars)
}

## Stop if not vars are left
ModuleStopIfNoVarsLeft <- function(vars) {
    if (length(vars) < 1) {stop("No valid variables.")}
}

## Toggle test FALSE if no strata are given
ModuleReturnFalseIfNoStrata <- function(strata, test) { # Give strata variable names

    if(missing(strata)) {
        ## test cannot be performed if no strata
        test <- FALSE
    }
    return(test)
}

## Check statra variables and conditionally create strata data frame
ModuleReturnStrata <- function(strata, data) {     # Give strata variable names
    ## strata: char vector; data: data frame given

    if(missing(strata)) {
        ## If there is no strata, give "Overall" to every subject (dim1 is nrow)
        strata <- rep("Overall", nrow(data))

    } else { # If strata is given

        ## unique it first to remove duplications
        strata <- unique(strata)

        ## Drop nonexisting and NA/NaN only variables
        strata <- ModuleReturnVarsExist(strata, data)

        ## Conditional on presence of remaining variable
        if (length(strata) == 0) {
            ## Stop if none left
            stop("None of the stratifying variables are present in the data frame.")

        } else {

            ## Check validity of the remaining variables
            logiSingleLevelOnly <-
                lapply(data[c(strata)],
                       function(VEC) {
                           ## Check number of levels
                           nLevels <- ifelse(test = is.factor(VEC),
                                             yes  = nlevels(VEC),
                                             no   = nlevels(factor(VEC)))
                           ## Return logical indicating only one valid level
                           nLevels == 1
                       })
            logiSingleLevelOnly <- unlist(logiSingleLevelOnly)

            ## Only keep variables that have 2+ levels
            if (any(logiSingleLevelOnly)) {
                warning("These variables have only one valid level: ",
                        paste0(strata[logiSingleLevelOnly], sep = " "), " Dropped")

                strata <- strata[!logiSingleLevelOnly]

            }

            ## Stop if no variables are left
            if (length(strata) == 0) {
                ## Stop if none left
                stop("None of the stratifying variables have 2+ valid levels.")
            }

            ## Extract the stratifying variable vector (strata is a data frame)
            strata <- data[c(strata)]
        }
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

    ## Total missing percentage
    freq$p.miss      <- (freq$miss / freq$n) * 100

    ## Category frequency
    freq$freq        <- freqRaw

    ## Category percent
    freq$percent     <- freqRaw / sum(freqRaw) * 100

    ## Category percent (cumulative)
    freq$cum.percent <- cumsum(freqRaw) / sum(freqRaw) * 100

    ## Reorder variables
    freq <- freq[c("n","miss","p.miss","level","freq","percent","cum.percent")]

    ## Return result as a data frame
    return(freq)
}

## Create StrataVarName from multiple dimension headers, for example sex:trt
ModuleCreateStrataVarName <- function(obj) {
    ## Combine variable names with : in between
    paste0(names(attr(obj, "dimnames")), collapse = ":")
}

### ModuleTryCatchWE
## Try catch function           # Taken from demo(error.catching)
## Used to define non-failing functions, that return NA when there is an error
ModuleTryCatchWE <- function(expr) {
    W <- NULL
    w.handler <- function(w) { # warning handler
        W <<- w
        invokeRestart("muffleWarning")
    }
    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
             warning = w.handler),
         warning = W)
}

### ModuleTestSafe
## Function to perform non-failing tests (obj should be xtabs or formula)
ModuleTestSafe <- function(obj, testFunction, testArgs = NULL) {

    ## Result from a function has to have $p.value element
    out <- ModuleTryCatchWE(do.call(testFunction, args = c(list(obj), testArgs))$p.value)

    ## If it contains a numeric value, return it. Otherwise, return NA.
    pValue <- ifelse(is.numeric(out$value), out$value, NA)

    ## When obj is an xtabs object
    if (any(class(obj) %in% "xtabs")) {
        ## and has 1 x M dimension, always return NA, and end there.
        if (dim(obj)[1] == 1) {
            ## ends here, returning NA
            return(NA)

        } else {
            ## If obj is a multi-row xtabs object, return the p-value
            pValue
        }
    } else {
        ## If obj is not an xtabs (formula for continuous variables), return the p-value
        pValue
    }
}


## Define special skewness and kurtosis functions that do not fail (SAS definitions)
ModuleSasSkewness <- function(x) {
    out <- ModuleTryCatchWE(e1071::skewness(x, na.rm = TRUE, type = 2))
    ## If it returns a numeric value, return it. Otherwise, return NaN.
    ifelse(is.numeric(out$value), out$value, NaN)
}
ModuleSasKurtosis <- function(x) {
    out <- ModuleTryCatchWE(e1071::kurtosis(x, na.rm = TRUE, type = 2))
    ## If it returns a numeric value, return it. Otherwise, return NaN.
    ifelse(is.numeric(out$value), out$value, NaN)
}


## Create a single variable representation of multivariable stratification for individuals
## result: by object; strata: data frame of stratifying variable(s)
ModuleCreateStrataVarAsFactor <- function(result, strata) {

    ## Create all possible combinations of strata levels and collapse as a vector.
    dfStrataLevels <- expand.grid(attr(result, "dimnames")) # 1st var cycles fastest, consistent with by()
    ## Create a single variable representing all strata
    strataLevels   <- apply(X = dfStrataLevels, MARGIN = 1, FUN = paste0, collapse = ":")
    ## The length is the number of potential combinations. Used for the levels argument in the next part.

    ## Create the actual variable from the observed levels. NA if any one of the variables is NA.
    strataVar      <- as.character(interaction(strata, sep = ":"))
    ## Make it a factor (kruskal.test requires it). Use levels not to drop defined nonexisting levels.
    strataVar      <- factor(strataVar, levels = strataLevels)

    ## Return stratifying variable. The length is the number of observations in the dataset.
    ## NA for subjects with NA for any of the stratifying variables.
    return(strataVar)
}



### Modules intented for the print methods
################################################################################

## Define a function to format a normal variable
ModuleConvertNormal <- function(rowMat, digits) {

    ## Format for SD
    fmt <- paste0(" (%.", digits,"f",")")

    ## Create a DF with numeric mean column and character (SD) column
    data.frame(col1 = rowMat[,"mean"],
               col2 = sprintf(fmt = fmt, rowMat[,"sd"]),
               stringsAsFactors = FALSE)
}

## Define a function to format a nonnormal variable
ModuleConvertNonNormal <- function(rowMat, digits, minMax = FALSE) {

    ## Format for [p25, p75]
    fmt <- paste0(" [%.", digits,"f, %.",digits,"f]")

    if (minMax == FALSE) {
        ## Create a DF with numeric median column and character [p25, p75] column
        out <- data.frame(col1 = rowMat[,"median"],
                          col2 = sprintf(fmt = fmt, rowMat[,"p25"], rowMat[,"p75"]),
                          stringsAsFactors = FALSE)
    } else if (minMax == TRUE) {
        ## Create a DF with numeric median column and character [p25, p75] column
        out <- data.frame(col1 = rowMat[,"median"],
                          col2 = sprintf(fmt = fmt, rowMat[,"min"], rowMat[,"max"]),
                          stringsAsFactors = FALSE)
    } else {
        stop("minMax must be a logical vector of one: FALSE or TRUE")
    }

    return(out)
}


## Module to handle TRUE/FALSE or character vector of variable names
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


## p-value picker/formatter
ModulePickAndFormatPValues <- function(TableObject, switchVec, pDigits) {

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

    ## Format p value
    fmt  <- paste0("%.", pDigits, "f")
    pVec <- sprintf(fmt = fmt, pValues)

    ## Create a string like <0.001
    smallPString       <- paste0("<0.", paste0(rep("0", pDigits - 1), collapse = ""), "1")
    ## Check positions where it is all zero like 0.000
    posAllZeros        <- grepl("^0\\.0*$", pVec)
    ## Put the string where it is all zero like 0.000
    pVec[posAllZeros]  <- smallPString
    ## Put a preceding space where it is not like 0.000
    pVec[!posAllZeros] <- paste0(" ", pVec[!posAllZeros])

    ## Return formatted p-values (as many as there are variables)
    return(pVec)
}


## Module to return the dimention headers added to the out 2d matrix
ModuleReturnDimHeaders <- function(TableObject) {

    ## Add stratification information to the column header
    if (length(TableObject) > 1) {
        ## Create strata string
        strataString <- paste0("Stratified by ",
                               paste0(names(attr(TableObject, "dimnames")), collapse = ":"))

        ## Name the row dimension with it. 1st dimension name should be empty.
        dimHeaders <- c("", strataString)

    }  else {
        ## If no stratification, no name for the second dimension
        dimHeaders <- c("", "")
    }

    ## Return the dim header a vector of length 2
    return(dimHeaders)
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

## Module to loop over variables within a stratum formatting categorical variables
ModuleCatFormatVariables <- function(lstVars, varsToFormat, fmt, level, cramVars, showAllLevels) {

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

               ## Format percent and cum.percent as strings
               DF[varsToFormat] <- lapply(X = DF[varsToFormat],
                                          FUN = sprintf,
                                          fmt = fmt)

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
                       ## Otherwise, keep the second level only
                       ## change variable name, and delete the first level.
                       DF$var <- sprintf("%s = %s", DF$var, DF$level)
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
ModuleCatFormatStrata <- function(CatTable, digits, varsToFormat, cramVars, showAllLevels) {

    ## Create format for percent used in the loop
    fmt1 <- paste0("%.", digits, "f")

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
                                            fmt           = fmt1,
                                            cramVars      = cramVars,
                                            showAllLevels = showAllLevels)


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


## Module to loop over strata formatting continuous variables
## No variable level looping here as each stratum is a matrix of all variables
ModuleContFormatStrata <- function(ContTable, nVars, listOfFunctions, digits) {

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
                   ## This row-by-row operation is necessary to handle mean (sd) and median [IQR]
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

                   ## Format for decimals
                   out$col1 <- sprintf(fmt = paste0("%.", digits, "f"), out$col1)

                   ## right justify by adding spaces (to align at the decimal point of mean/median)
                   out$col1 <- format(out$col1, justify = "right")

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



### Modules by both print and summary methods
## ModuleQuoteAndPrintMat()
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
