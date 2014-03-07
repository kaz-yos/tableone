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

## Function to perform non-failing tests (obj should be xtabs or formula)
ModuleTestSafe <- function(obj, testFunction, testArgs = NULL) {
    ## Result from a function has to have $p.value element
    out <- ModuleTryCatchWE(do.call(testFunction, args = c(list(obj), testArgs))$p.value)
    ## If it returns a numeric value, return it. Otherwise, return NA.
    ifelse(is.numeric(out$value), out$value, NA)
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
    if (length(TableObject) > 1 ) {
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
