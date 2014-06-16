##' Format and print the \code{ContTable} class objects
##'
##' This is the \code{print} method for the \code{ContTable} class objects created by \code{\link{CreateContTable}} function.
##'
##' @param x The result of a call to the \code{\link{CreateContTable}} function.
##' @param digits Number of digits to print in the table.
##' @param pDigits Number of digits to print for p-values.
##' @param quote Whether to show everything in quotes. The default is FALSE. If TRUE, everything including the row and column names are quoted so that you can copy it to Excel easily.
##' @param missing Whether to show missing data information (not implemented yet, placeholder)
##' @param explain Whether to add explanation to the variable names, i.e., (mean (sd) or median [IQR]) is added to the variable names.
##' @param printToggle Whether to print the output. If FLASE, no output is created, and a matrix is invisibly returned.
##' @param noSpaces Whether to remove spaces added for alignment. Use this option if you prefer to align numbers yourself in other software.
##' @param nonnormal A character vector to specify the variables for which the p-values should be those of nonparametric tests. By default all p-values are from normal assumption-based tests (oneway.test).
##' @param minMax Whether to use [min,max] instead of [p25,p75] for nonnormal variables. The default is FALSE.
##' @param insertLevel Whether to add an empty level column to the left of strata.
##' @param test Whether to show the p-values. TRUE by default. If FALSE, only the numerical summaries are shown.
##' @param ... For compatibility with generic. Ignored.
##' @return It is mainly for printing the result. But this function does return a matrix containing what you see in the output invisibly. You can assign it to an object to save it.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{CreateCatTable}}, \code{\link{print.CatTable}}, \code{\link{summary.CatTable}},
##' \code{\link{CreateContTable}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}},
##' \code{\link{CreateTableOne}}, \code{\link{print.TableOne}}, \code{\link{summary.TableOne}}
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
##' ## Create an overall table for continuous variables
##' contVars <- c("time","age","bili","chol","albumin","copper",
##'               "alk.phos","ast","trig","platelet","protime")
##' contTableOverall <- CreateContTable(vars = contVars, data = pbc)
##'
##' ## Simply typing the object name will invoke the print.ContTable method,
##' ## which will show the sample size, means and standard deviations.
##' contTableOverall
##'
##' ## To further examine the variables, use the summary.ContTable method,
##' ## which will show more details.
##' summary(contTableOverall)
##'
##' ## c("age","chol","copper","alk.phos","trig","protime") appear highly skewed.
##' ## Specify them in the nonnormal argument, and the display changes to the median,
##' ## and the [25th, 75th] percentile.
##' nonNormalVars <- c("age","chol","copper","alk.phos","trig","protime")
##' print(contTableOverall, nonnormal = nonNormalVars)
##'
##' ## To show median [min,max] for nonnormal variables, use minMax = TRUE
##' print(contTableOverall, nonnormal = nonNormalVars, minMax = TRUE)
##'
##' ## The table can be stratified by one or more variables
##' contTableBySexTrt <- CreateContTable(vars = contVars,
##'                                      strata = c("sex","trt"), data = pbc)
##'
##' ## print now includes p-values which are by default calculated by oneway.test (t-test
##' ## equivalent in the two group case). It is formatted at the decimal place specified
##' ## by the pDigits argument (3 by default). It does <0.001 for you.
##' contTableBySexTrt
##'
##' ## The nonnormal argument toggles the p-values to the nonparametric result from
##' ## kruskal.test (wilcox.test equivalent for the two group case).
##' print(contTableBySexTrt, nonnormal = nonNormalVars)
##'
##' ## The minMax argument toggles whether to show median [range]
##' print(contTableBySexTrt, nonnormal = nonNormalVars, minMax = TRUE)
##'
##' ## summary now includes both types of p-values
##' summary(contTableBySexTrt)
##'
##' ## If your work flow includes copying to Excel and Word when writing manuscripts,
##' ## you may benefit from the quote argument. This will quote everything so that
##' ## Excel does not mess up the cells.
##' print(contTableBySexTrt, nonnormal = nonNormalVars, quote = TRUE)
##'
##' ## If you want to center-align values in Word, use noSpaces option.
##' print(contTableBySexTrt, nonnormal = nonNormalVars, quote = TRUE, noSpaces = TRUE)
##' 
##' @export
print.ContTable <- function(x,                       # ContTable object
                            digits = 2, pDigits = 3, # Number of digits to show
                            quote        = FALSE,    # Whether to show quotes

                            missing      = FALSE,    # show missing values (not implemented yet)
                            explain      = TRUE,     # Whether to show explanation in variable names
                            printToggle  = TRUE,     # Whether to print the result visibly
                            noSpaces     = FALSE,    # Whether to remove spaces for alignments

                            nonnormal    = NULL,     # Which variables should be treated as nonnormal
                            minMax       = FALSE,    # median [range] instead of median [IQR]
                            insertLevel  = FALSE,    # insert the level column to match showAllLevels in print.CatTable

                            test         = TRUE,     # Whether to add p-values

                            ...) {

    ## x and ... required to be consistent with generic print(x, ...)
    ContTable <- x

### Check data structure first

    ## ContTable is by() object
    ## Get the position of the first non-null element
    posFirstNonNullElement <- which(!sapply(ContTable, is.null))[1]
    ## Save variable names using the first non-null element
    varNames <- rownames(ContTable[[posFirstNonNullElement]])
    ## Check the number of variables
    nVars <- length(varNames)


    ## Returns a numeric vector: 1 for normal variable; 2 for nonnormal variable
    nonnormal <- ModuleHandleDefaultOrAlternative(switchVec       = nonnormal,
                                                  nameOfSwitchVec = "nonnormal",
                                                  varNames        = varNames)


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
                          ## each strutum is a data frame with one row for each variable
                          ## Obtain n from all variables (matrix)
                          n <- stratum[,"n"]
                          ## Pick the first non-null element
                          n[!is.null(n)][1]
                          ## Convert NULL to 0
                          ifelse(is.null(n), "0", as.character(n))
                      },
                      simplify = TRUE) # vector with as many elements as strata

    ## Provide indicators to show what columns were added.
    wasPValueColumnAdded     <- FALSE
    wasNonNormalColumnAdded  <- FALSE


### Conversion of data for printing

    ## Define the nonnormal formatter depending on the minMax status
    ConvertNormal <- function(rowMat) {
        ## Take minMax value from outside (NOT A STANDALONE FUNCTION!!)
        ModuleConvertNormal(rowMat, digits)
    }
    ## Define the nonnormal formatter depending on the minMax status
    ConvertNonNormal <- function(rowMat) {
        ## Take minMax value from outside (NOT A STANDALONE FUNCTION!!)
        ModuleConvertNonNormal(rowMat, digits, minMax = minMax)
    }

    ## Create a list of these two functions
    listOfFunctions <- list(normal = ConvertNormal, nonnormal = ConvertNonNormal)

    ## Take functions from the 2-element list, and convert to an nVars-length list
    listOfFunctions <- listOfFunctions[nonnormal]

    ## Loop over strata (There may be just one)
    out <- sapply(ContTable,
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

### Obtain the original column width in characters for alignment in print.TableOne
    vecColWidths <- sapply(out,
                           FUN = function(LIST) {

                               ## Get the width of the column
                               attributes(LIST)$nCharMeanOrMedian
                           },
                           simplify = TRUE)


    ## The outer sapply should not simplify to avoid a vector
    ## Column-bind to create variables x strata matrix
    out <- do.call(cbind, out)

    ## Put the variables names back (looping over rows can solve this)
    rownames(out) <- varNames

    ## Add column names if multivariable stratification is used.
    if (length(attr(ContTable, "dimnames")) > 1) {

        colnames(out) <- ModuleCreateStrataNames(ContTable)
    }


    ## Add p-values when requested and available
    if (test == TRUE & !is.null(attr(ContTable, "pValues"))) {

        ## Pick test types used (used for annonation)
        testTypes <- c("","nonnorm")[nonnormal]

        ## Pick the p-values requested, and format like <0.001
        pVec <- ModulePickAndFormatPValues(TableObject = ContTable,
                                           switchVec   = nonnormal,
                                           pDigits     = pDigits)

        ## Column combine with the output
        out <- cbind(out, p = pVec)

        ## Change the indicator
        wasPValueColumnAdded <- TRUE


        ## Create an empty test type column, and add test types
        out <- cbind(out,
                     test = rep("", nrow(out))) # Column for test types
        ## Put the test types  at the non-empty positions (all rows in continuous!)
        out[ ,"test"] <- testTypes

        ## Change the indicator
        wasNonNormalColumnAdded <- TRUE
    }


    ## Add mean (sd) or median [IQR]/median [range] explanation if requested
    if (explain) {
        ## Create a vector of explanations to be pasted
        if (minMax == FALSE) {
            what <- c(" (mean (sd))"," (median [IQR])")[nonnormal]
        } else if (minMax == TRUE) {
            what <- c(" (mean (sd))"," (median [range])")[nonnormal]
        }
        ## Paste to the rownames
        rownames(out) <- paste0(rownames(out), what)
    }

    ## Keep column names (strataN does not have correct names if stratification is by multiple variables)
    outColNames <- colnames(out)
    ## Add n at the correct location depending on the number of columns added (level and/or p)
    out <- rbind(n = c(strataN,
                     p    = rep("", wasPValueColumnAdded),   # Add "" padding if p-value added
                     test = rep("", wasNonNormalColumnAdded) # Add "" padding if nonnormal test used
                     ),
                 out)
    ## Put back the column names (overkill for non-multivariable cases)
    colnames(out) <- outColNames

    ## Add the level column if requested
    if (insertLevel) {
        out <- cbind(level = rep("", nrow(out)),
                     out)
    }

    ## Add stratification information to the column header depending on the dimension
    names(dimnames(out)) <- ModuleReturnDimHeaders(ContTable)

    ## Remove spaces if asked.
    out <- ModuleRemoveSpaces(mat = out, noSpaces = noSpaces)

    ## (module) Takes an matrix object format, print if requested
    out <- ModuleQuoteAndPrintMat(matObj = out,
                                  quote = quote, printToggle = printToggle)

    ## Add attributes for column widths in characters
    attributes(out) <- c(attributes(out),
                         list(vecColWidths = vecColWidths))

    ## return a matrix invisibly
    return(invisible(out))
}
