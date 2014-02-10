##' Create an object summarizing categorical variables
##' 
##' Create an object summarizing categorical variables optionally stratifying
##' by one or more startifying variables and performing statistical tests. The
##' object gives a table that is easy to use in medical research papers. See
##' also \code{\link{print.CatTable}} and \code{\link{summary.CatTable}}.
##' 
##' @param vars Variable(s) to be summarized given as a character vector.
##' @param strata Stratifying (grouping) variable name(s) given as a character
##' vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables
##' (both vars and strata) must be in this data frame.
##' @param test If TRUE, as in the default and there are more than two groups,
##' groupwise comparisons are performed. Both tests that require the large
##' sample approximation and exact tests are performed. Either one of the
##' result can be obtained from the print method.
##' @param testApprox A function used to perform the large sample approximation
##' based tests. The default is chisq.test. This is not recommended when some
##' of the cell have small counts like fewer than 5.
##' @param testExact A function used to perform the exact tests. The default is
##' fisher.test. If the cells have large numbers, it will fail because of
##' memory limitation. In this situation, the large sample approximation based
##' should suffice.
##' @return An object of class \code{CatTable}, which really is a \code{\link{by}} object with
##' additional attributes. Each element of the \code{\link{by}} part is a matrix with rows
##' representing variables, and columns representing summary statistics.
##' @author Kazuki Yoshida
##' @seealso \code{\link{print.CatTable}}, \code{\link{summary.CatTable}}, \code{\link{CreateContTable}},
##' \code{\link{print.ContTable}}, \code{\link{summary.ContTable}}
##' @examples
##' 
##' ## Load
##' library(tableone)
##' 
##' ## Load Mayo Clinic Primary Biliary Cirrhosis Data
##' library(survival)
##' data(pbc)
##' ## Check variables
##' head(pbc)
##' 
##' ## Create an overall table for categorical variables
##' catVars <- c("status","ascites","hepato","spiders","edema","stage")
##' catTableOverall <- CreateCatTable(vars = catVars, data = pbc)
##' 
##' ## Simply typing the object name will invoke the print.CatTable method,
##' ## which will show the sample size, frequencies and percentages.
##' ## For 2-level variables, only the higher level is shown for simplicity.
##' catTableOverall
##' 
##' ## Use the showAllLevels argument to see all levels for all variables.
##' print(catTableOverall, showAllLevels = TRUE)
##' 
##' ## You can choose form frequencies ("f") and/or percentages ("p") or both.
##' ## "fp" frequency (percentage) is the default. Row names change accordingly.
##' print(catTableOverall, format = "f")
##' print(catTableOverall, format = "p")
##' 
##' ## To further examine the variables, use the summary.CatTable method,
##' ## which will show more details.
##' summary(catTableOverall)
##' 
##' ## The table can be stratified by one or more variables
##' catTableBySexTrt <- CreateCatTable(vars = catVars,
##'                                    strata = c("sex","trt"), data = pbc)
##' 
##' 
##' ## print now includes p-values which are by default calculated by chisq.test.
##' ## It is formatted at the decimal place specified by the pDigits argument
##' ## (3 by default). It does <0.001 for you.
##' catTableBySexTrt
##' 
##' ## The exact argument will toggle the p-values to the example test result from
##' ## fisher.test. It will show which ones are from exact tests.
##' print(catTableBySexTrt, exact = "ascites")
##' 
##' ## summary now includes both types of p-values
##' summary(catTableBySexTrt)
##' 
##' ## If your work flow includes copying to Excel and Word when writing manuscripts,
##' ## you may benefit from the quote argument. This will quote everything so that
##' ## Excel does not mess up the cells.
##' print(catTableBySexTrt, exact = "ascites", quote = TRUE)
##' 
##' @export CreateCatTable
CreateCatTable <- function(vars,                    # vector of characters
                           strata,                  # single element character vector
                           data,                    # data frame
                           test  = TRUE,            # Whether to put p-values
                           testApprox = chisq.test, # approximation test
                           testExact  = fisher.test # exact test
                           ) {

### Data check
    ## Check if the data given is a dataframe.
    if (is.data.frame(data) == FALSE) {
        stop("The data argument needs to be a data frame (no quote).")
    }

    ## Check if variables exist in the data frame. If not, drop them.
    varsNotInData <- setdiff(vars, names(data))
    if (length(varsNotInData) > 0) {
        warning("The data frame does not have ",
                paste0(varsNotInData, sep = " "), ". Dropping them.")
        ## Only keep variables that exist
        vars <- intersect(vars, names(data))
    }

    ## Abort if no variables exist at this point
    if (length(vars) < 1) {stop("No valid variables.")}

    ## Extract necessary variables (unused variables are not included in dat)
    dat <- data[c(vars)]

    ## Convert to a factor if it is not a factor already. (categorical version only)
    ## Not done on factors, to avoid dropping zero levels.
    datNotFactor <- sapply(dat, class) != "factor"
    dat[datNotFactor] <- lapply(dat[datNotFactor], factor)


    ## Condition on the presence/absence of the strata
    if(missing(strata)){
        ## If there is no strata, give "Overall" to every subject
        strata <- rep("Overall", dim(dat)[1])                           # Check if dim(dat)[[1]] is correct.
        ## test cannot be performed
        test <- FALSE
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



### Perform descriptive analysis

    ## Used to define non-failing functions, that return NA when there is an error
    tryCatch.W.E <- function(expr) { # Taken from demo(error.catching)
        W <- NULL
        w.handler <- function(w){ # warning handler
            W <<- w
            invokeRestart("muffleWarning")
        }
        list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                 warning = w.handler),
             warning = W)
    }


    ## Taken from Deducer::frequencies()
    CreateTableForOneVar <- function(x) {

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

    ## strata--variable-CreateTableForOneVar structure
    ## Devide by strata
    result <- by(data = dat, INDICES = strata,

                 ## Work on each stratum
                 FUN = function(dfStrataDat) { # dfStrataDat should be a data frame

                     ## Loop for variables
                     sapply(dfStrataDat,
                            FUN = CreateTableForOneVar,
                            simplify = FALSE)

                 }, simplify = FALSE)

    
    ## Added from the print method. Delete the one in print method once ready
    ## Add stratification information to the column header
    if (length(result) > 1 ) {
        ## Combine variable names with : in between
        strataVarName <- paste0(names(attr(result, "dimnames")), collapse = ":")
        ## Add an attribute for the stratifying variable name
        attributes(result) <- c(attributes(result),
                                list(strataVarName = strataVarName))
    }
    

### Perform tests when necessary
    ## Initialize
    pValues <- NULL
    listXtabs <- list()

    ## Only when test is asked for              # Should always do this?
    if (test == TRUE) {

        ## Define special test functions that do not fail, and return p-values or NA
        tryTestApprox <- function(mat) {

            out <- tryCatch.W.E(testApprox(mat)$p.value)
            ## If it returns a numeric value, return it. Otherwise, return NA.
            ifelse(is.numeric(out$value), out$value, NA)
        }
        tryTestExact <- function(mat) {

            out <- tryCatch.W.E(testExact(mat)$p.value)
            ## If it returns a numeric value, return it. Otherwise, return NA.
            ifelse(is.numeric(out$value), out$value, NA)
        }

        
        ## Create all combinations of strata levels and collapse as a vector for level combinations.
        dfStrataLevels <- expand.grid(attr(result, "dimnames")) # 1st var cycles fastest, consistent with by()
        ## Create a single variable representing all strata        
        strataLevels <- apply(X      = dfStrataLevels,
                              MARGIN = 1,
                              FUN    = paste0, collapse = ":")
        ## Create the actual variable from the observed levels
        strataVar <- as.character(interaction(strata, sep = ":"))

        ## obsolete
        ## ## Create the actual variable from the observed levels
        ## strataVar                   <- apply(X = strata, MARGIN = 1, FUN = paste0, collapse = ":")
        ## ## Give NA if any of the variables are missing
        ## strataVarAnyMiss            <- apply(X = is.na(strata), MARGIN = 1, FUN = sum) > 0
        ## strataVar[strataVarAnyMiss] <- NA
        
        ## Make it a factor (kruskal.test requires it). Use levels not to drop defined nonexisting levels.
        strataVar                   <- factor(strataVar, levels = strataLevels)
        
        ## Loop over variables in dat, and create a list of xtabs
        listXtabs <- sapply(X = names(dat),
                            FUN = function(var) {
                                ## Create a formula
                                formula <- paste0("~ ", var, " + ", "strataVar")
                                formula <- as.formula(formula)
                                
                                ## Create a 2-dimensional crosstable
                                xtabs(formula = formula, data = dat)
                            },
                            simplify = FALSE)

        ## Rename the second dimension of the xtabs with the newly create name.
        for (i in seq_along(listXtabs)) {
            
            names(dimnames(listXtabs[[i]]))[2] <- strataVarName
        }        
        
        ## Loop over xtabs, and create p-values
        pValues <- sapply(X = listXtabs,
                          FUN = function(xtabs) {
                              ## Perform tests and return the result as 1x2 DF
                              data.frame(
                                  pApprox = tryTestApprox(xtabs),
                                  pExact  = tryTestExact(xtabs)
                                  )
                          },
                          simplify = FALSE)        

        ## Create a single data frame (n x 2 (normal,nonormal))
        pValues <- do.call(rbind, pValues)
    } # Conditional for test == TRUE ends here.


    ## Return object
    ## Give an S3 class
    class(result) <- c("CatTable", class(result))

    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues = pValues),
                            list(xtabs   = listXtabs))

    ## Return
    return(result)
}
