################################################################################
### Modules for contructors
##
## Created on: 2015-08-02
## Author: Kazuki Yoshida
################################################################################


###
### Data check modules
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


###
### Modules for data creation
################################################################################

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


## Convert variables with NA to include NA as a level (for CatTable constructor)
ModuleIncludeNaAsLevel <- function(data) {
    ## Logical vector for variables that have any NA
    logiAnyNA <- (colSums(is.na(data)) > 0)

    ## Add NA as a new level unless already present
    data[logiAnyNA] <-
                  lapply(data[logiAnyNA],
                         function(var) {
                             if (all(!is.na(levels(var)))) {
                                 var <- factor(var, c(levels(var), NA),
                                               exclude = NULL)
                             }
                             var
                         })
    data
}



###
### Modules for stratification
################################################################################

## Create StrataVarName from multiple dimension headers, for example sex:trt
ModuleCreateStrataVarName <- function(obj) {
    ## Combine variable names with : in between
    paste0(names(attr(obj, "dimnames")), collapse = ":")
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



###
### Modules for safe hypothesis testing and numeric summaries
################################################################################

## ModuleTryCatchWE
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


###
### Module for testing multiple variables
################################################################################

ModuleApproxExactTests <- function(result, strata, dat, strataVarName,
                                   testApprox, argsApprox,
                                   testExact,  argsExact) {
    ## Create a single variable representation of multivariable stratification
    strataVar <- ModuleCreateStrataVarAsFactor(result, strata)

    ## Loop over variables in dat, and create a list of xtabs
    ## Empty strata are kept in the corss tables. Different behavior than the cont counterpart!
    listXtabs <- sapply(X = names(dat),
                        FUN = function(var) {
                            ## Create a formula
                            formula <- as.formula(paste0("~ ", var, " + ", "strataVar"))

                            ## Create a 2-dimensional crosstable
                            xtabs(formula = formula, data = dat)
                        },
                        simplify = FALSE)

    ## Rename the second dimension of the xtabs with the newly create name.
    for (i in seq_along(listXtabs)) {

        names(dimnames(listXtabs[[i]]))[2] <- strataVarName
    }

    ## Loop over xtabs, and create p-values
    pValues <-
    sapply(X = listXtabs,
           FUN = function(xtabs) {
               ## Perform tests and return the result as 1x2 DF
               data.frame(pApprox = ModuleTestSafe(xtabs, testApprox, argsApprox),
                          pExact  = ModuleTestSafe(xtabs, testExact,  argsExact))
           },
           simplify = FALSE)

    ## Create a single data frame (n x 2 (normal,nonormal))
    pValues <- do.call(rbind, pValues)

    ## Return both xtabs and p value df
    list(pValues = pValues, xtabs = listXtabs)
}
