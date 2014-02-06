## Print method for a continuous table
print.CatTable <- function(CatTable, missing = FALSE,
                           format = c("fp","f","p","pf")[1], # Format f_requency and/or p_ercent
                           digits = 1, exact = NULL, quote = TRUE,
                           test = TRUE, pDigits = 3,
                           showAllLevels = FALSE,
                           explain = TRUE) {

### Check the data structure first

    ## CatTable has a strata(list)-variable(list)-table(dataframe) structure

    ## Save variable names
    ## This will not work if the first element is NULL
    varNames <- names(CatTable[[1]])
    ## Check the number of variables
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
            exact <- vars %in% exact
        }
        ## Convert to numeric (1 for approx, 2 for exact)
        exact <- as.numeric(exact) + 1
    }

    ## Check format argument. If it is broken, choose "fp" for frequency (percent)
    if (!length(format) == 1  | !format %in% c("fp","f","p","pf")) {
        warning("format only accepts one of fp, f, p, or pf. Choosing fp.")
        format <- "fp"
    }


### Formatting for printing
    
    ## Create format for percent used in the loop
    fmt1 <- paste0("%.", digits, "f")

    ## Obtain collpased result
    CatTableCollapsed <-
        sapply(X = CatTable,   # Loop over strata
               FUN = function(LIST) {

                   ## if (is.null(LIST)) {
                   ##     ## If the stratum is empty.
                   ##     browser()
                   ## }

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

                                      ## Make all variables strings
                                      DF[] <- lapply(X = DF, FUN = as.character)

                                      ## Add freq (percent) column
                                      DF$freqPer <- sprintf(fmt = "%s (%s)",
                                                            DF$freq,
                                                            DF$percent)

                                      ## Add percent (freq) column
                                      DF$perFreq <- sprintf(fmt = "%s (%s)",
                                                            DF$percent,
                                                            DF$freq)

                                      ## If there are only TWO levels and showAllLevels is FALSE,
                                      ## delete the first
                                      if (nRow == 2 & !showAllLevels) {

                                          DF <- DF[-1, , drop = FALSE]
                                      }

                                      ## Delete n and miss except in the first row
                                      DF[-1, c("var","n","miss")] <- ""

                                      ## Return a data frame
                                      DF
                                  },
                                  simplify = FALSE)

                   ## Collapse DFs within each stratum
                   DF <- do.call(rbind, LIST)

                   ## ## Keep only necessary columns
                   ## DF <- DF[c("var","freqPer")]

                   ## Return a data frame
                   DF
               }, simplify = FALSE)


### This part is a very messy hack to fix null list element. 2014-02-05. Come back and fix.
    ## Get the positions of the null elements
    posNullElement <- sapply(CatTableCollapsed, is.null)
    ## Get the positions of the first non-null element
    posFirstNonNullElement <- which(!posNullElement)[1]
    CatTableCollapsed[posNullElement] <- CatTableCollapsed[posFirstNonNullElement]
    ## Access as a data frame that should be empty and erase
    for (i in which(posNullElement)) {

        CatTableCollapsed[[i]][] <- lapply(CatTableCollapsed[[i]][],
                                           function(var) {

                                               var <- rep("empty", length(var))
                                           })
    }
    

    ## Choose the column name for the right format
    nameResCol <- c("freqPer","freq","percent","perFreq")[format == c("fp","f","p","pf")]


    ## Create output matrix without variable names with the right format
    out <- do.call(cbind, lapply(CatTableCollapsed, getElement, nameResCol))
    out <- as.matrix(out)

    ## Set the variables names
    rownames(out) <- CatTableCollapsed[[1]][,"var"]
    ## Get positions of non-emptyenv row names
    posNonEmptyRowNames <- !rownames(out) == ""

    ## Add column names if multivariable stratification is used.
    if (length(attr(CatTable, "dimnames")) > 1) {

        colnames(out) <-
            ## Create all combinations and collapse as strings
            apply(expand.grid(attr(CatTable, "dimnames")),
                  MARGIN = 1,
                  paste0, collapse = ":")
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

        ## Format
        fmt <- paste0("%.", pDigits, "f")
        p   <- sprintf(fmt = fmt, pValues)
        ## Create an empty p-value column
        out <- cbind(out, p = rep("", nrow(out)))
        ## Put the values at the non-empty positions
        out[posNonEmptyRowNames,"p"] <- p
    }


    ## Add freq () explanation if requested
    if (explain) {
        ## Choose the format of the explanation string
        explainString <- c(" (%)", "", " (%)", " % (freq)")[format == c("fp","f","p","pf")]
        ## Only for rows with row names
        rownames(out)[posNonEmptyRowNames] <- paste0(rownames(out)[posNonEmptyRowNames],
                                                     explainString)
    }

    ## Add quotes for names if requested
    if (quote) {
        rownames(out) <- paste0('"', rownames(out), '"')
        colnames(out) <- paste0('"', colnames(out), '"')
    }

    ## Print stratification
    if (length(CatTable) > 1 ) {
        cat(paste0("Stratified by ",
                   ## names(attr(CatTable, "dimnames")),
                   paste0(names(attr(CatTable, "dimnames")), collapse = ":"),
                   "\n"))
    }

    ## Print the results
    print(out, quote = quote)
}
