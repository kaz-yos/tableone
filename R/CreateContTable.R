## A function to create a table for continuous variables
## Modification of "descriptive.table.R" in Deducer version 0.7-6.1 published on 2013-10-28 by Ian Fellows et al.

CreateContTable <- function(vars,       # vector of characters
                            strata,     # single element character vector
                            data,       # data frame
                            nonnormal = FALSE, # nonnormality indicator
                            func.names = c(     # can pick a subset of them
                                "n","miss",
                                "mean","sd",
                                "median","q25","q75","min","max",
                                "skew","kurt"
                                ),
                            func.additional     # named list of additional functions
                            ) {
    ## Require dependencies
    require(e1071)

    ## Check the dataframe
    if (is.data.frame(data) == FALSE) {
        stop("The data argument needs to be a data frame (no quote).")
    }

    ## Check variables
    varsNotInData <- setdiff(vars, names(data))
    if (length(varsNotInData) > 0) {
        warning("The dataset does not have ", varsNotInData, ". Dropping them.")
        ## Only keep variables that exist
        vars <- intersect(vars, names(data))
    }

    ## Abort if variables exist at this point
    if (length(vars) < 1) {stop("No valid variables.")}


    ## Check strata variable
    if (!strata %in% names(data)) {
        stop(strata, "does not exist in the data.")
    }
    

    ## Extract necessary variables
    dat <- data[c(vars)]

    ## Handle non-numeric elements
    if(any(!sapply(dat, is.numeric))){
        ## If there is any non-numeric variables
        dat <- dat[sapply(dat, is.numeric)]
        warning("Non-numeric variables dropped")
    }

    ## Condition on the presence/absence of the strata
    if(missing(strata)){
        ## If there is no strata, name the list "Overall"
        strata <- rep("Overall", dim(dat)[1])

    } else {
        ## Extract the stratifying variable vector
        strata <- data[c(strata)]
    }

    ## Check if all the variables are continuous, and stop if not
    if(!all(sapply(dat, is.numeric))) {stop("Can only be run on numeric variables")}

    ## Create indexes for default functions by partial string matching with the func.names argument
    func.indexes <- pmatch(func.names, c("n","miss",
                                         "mean","sd",
                                         "median","q25","q75","min","max",
                                         "skew","kurt"))
    ## Remove NA
    func.indexes <- func.indexes[!is.na(func.indexes)]


    ## Create a list of default functions
    functions <- c("n"      = function(x) length(x),
                   "miss"   = function(x) sum(is.na(x)),
                   "mean"   = function(x) mean(x, na.rm = TRUE),
                   "sd"     = function(x) sd(x, na.rm = TRUE),
                   "median" = function(x) median(x, na.rm = TRUE),
                   "q25"    = function(x) quantile(x, probs = 0.25, na.rm = TRUE),
                   "q75"    = function(x) quantile(x, probs = 0.75, na.rm = TRUE),
                   "min"    = function(x) min(x, na.rm = TRUE),
                   "max"    = function(x) max(x, na.rm = TRUE),
                   "skew"   = function(x) skewness(x, na.rm = TRUE, type = 2),     # type 2 as in SAS and SPSS
                   "kurt"   = function(x) kurtosis(x, na.rm = TRUE, type = 2)      # type 2 as in SAS and SPSS
                   )

    ## Keep only functions in use
    functions <- functions[func.indexes]

    ## Check for additional functions
    if(!missing(func.additional)){
        ## When additional functions are given
        if(!is.list(func.additional) || is.null(names(func.additional))) {
            ## Stop if not a named list
            stop("func.additional must be a named list of functions")
        }

        ## If a named list is given, add to the vector of functions and their names
        functions  <- c(functions, unlist(func.additional))
        func.names <- c(func.names, names(func.additional))
    }


    ## strata-functions-variable structure alternative 2014-01-22

    ## Devide by strata
    result <- by(data = dat, INDICES = strata,

                 ## Work on each stratum
                 FUN = function(strataDat) { # strataDat should be a data frame

                     ## Loop for functions
                     sapply(functions,
                            FUN = function(fun) {

                                ## Loop for variables
                                sapply(strataDat, fun)
                            })
                 })

    ## Not sure what this does.
    ## clean out nulls
    ## Drop strata with any null values?
    ## result <- result[!sapply(result, # loop on strata
    ##                          function(x) {all(sapply(x, function(x) {all(is.null(x))}))})]

    ## Give an S3 class
    class(result) <- "ContTable"

    ## Return
    return(result)
}
