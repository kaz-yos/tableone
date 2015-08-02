##' Format and print \code{svyContTable} class objects
##'
##' \code{print} method for the \code{svyContTable} class objects created by \code{\link{CreateContTable}} function.
##'
##' @param x Object returned by \code{\link{svyCreateContTable}} function.
##' @param digits Number of digits to print in the table.
##' @param pDigits Number of digits to print for p-values (also used for standardized mean differences).
##' @param quote Whether to show everything in quotes. The default is FALSE. If TRUE, everything including the row and column names are quoted so that you can copy it to Excel easily.
##' @param missing Whether to show missing data information (not implemented yet, placeholder)
##' @param explain Whether to add explanation to the variable names, i.e., (mean (sd) or median [IQR]) is added to the variable names.
##' @param printToggle Whether to print the output. If FLASE, no output is created, and a matrix is invisibly returned.
##' @param noSpaces Whether to remove spaces added for alignment. Use this option if you prefer to align numbers yourself in other software.
##' @param nonnormal A character vector to specify the variables for which the p-values should be those of nonparametric tests. By default all p-values are from normal assumption-based tests (oneway.test).
##' @param minMax Whether to use [min,max] instead of [p25,p75] for nonnormal variables. The default is FALSE.
##' @param insertLevel Whether to add an empty level column to the left of strata.
##' @param test Whether to show p-values. TRUE by default. If FALSE, only the numerical summaries are shown.
##' @param smd Whether to show standardized mean differences. FALSE by default. If there are more than one contrasts, the average of all possible standardized mean differences is shown. For individual contrasts, use \code{summary}.
##' @param ... For compatibility with generic. Ignored.
##' @return A matrix object containing what you see is also invisibly returned. This can be assinged a name and exported via \code{write.csv}.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{svyCreateTableOne}}, \code{\link{svyCreateCatTable}}, \code{\link{summary.svyCatTable}}
##' @examples
##'
##' ## See the examples for svyCreateTableOne()
##'
##' @export
print.svyContTable <-
function(x,                       # ContTable object
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

         smd          = FALSE,    # Whether to add standardized mean differences

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
                          ifelse(is.null(n),
                                 "0",
                                 sprintf(fmt = paste0("%.", digits, "f"), n))
                      },
                      simplify = TRUE) # vector with as many elements as strata


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
    out <- ModuleContFormatStrata(ContTable       = ContTable,
                                  nVars           = nVars,
                                  listOfFunctions = listOfFunctions,
                                  digits          = digits)


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

        ## Create an empty test type column, and add test types
        out <- cbind(out,
                     test = rep("", nrow(out))) # Column for test types
        ## Put the test types  at the non-empty positions (all rows in continuous!)
        out[ ,"test"] <- testTypes

    }


    ## Add SMDs when requested and available
    if (smd & !is.null(attr(ContTable, "smd"))) {

        ## Create an empty column
        out <- cbind(out,
                     SMD = rep("", nrow(out))) # Column for p-values
        ## Put the values at the non-empty positions
        out[,"SMD"] <- ModuleFormatPValues(attr(ContTable, "smd")[,1],
                                           pDigits = pDigits)
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
    nRow <- c(strataN, rep("", ncol(out) - length(strataN))) # Additional padding to right
    out <- rbind(n = nRow, out)
    ## Put back the column names (overkill for non-multivariable cases)
    colnames(out) <- outColNames

    ## Add the level column if requested
    if (insertLevel) {
        out <- cbind(level = rep("", nrow(out)),
                     out)
    }

    ## Add stratification information to the column header depending on the dimension
    names(dimnames(out)) <- c("", paste0("Stratified by ",
                                         attr(ContTable, "strataVarName")))

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
