##' Format and print the \code{TableOne} class objects
##'
##' This is the print method for the TableOne class objects created by
##' CreateTableOne function.
##'
##'
##' @param x The result of a call to the \code{\link{CreateTableOne}} function.
##' @param missing Whether to show missing data information (not implemented
##' yet, placeholder)
##' @param quote Whether to show everything in quotes. The default is FALSE. If
##' TRUE, everything including the row and column names are quoted so that you
##' can copy it to Excel easily.
##' @param test Whether to show the p-values. TRUE by default. If FALSE, only
##' the numerical summaries are shown.
##' @param pDigits Number of digits to print for p-values.
##' @param format The default is "fp" frequency (percentage). You can also
##' choose from "f" frequency only, "p" percentage only, and "pf" percentage
##' (frequency).
##' @param exact A character vector to specify the variables for which the
##' p-values should be those of exact tests. By default all p-values are from
##' large sample approximation tests (chisq.test).
##' @param showAllLevels Whether to show all levels. FALSE by default, i.e.,
##' for 2-level categorical variables, only the higher level is shown to avoid.
##' @param nonnormal A character vector to specify the variables for which the
##' p-values should be those of nonparametric tests. By default all p-values
##' are from normal assumption-based tests (oneway.test).
##' @param explain Whether to add explanation to the variable names, i.e., (\%)
##' is added to the variable names when percentage is shown.
##' @param printToggle Whether to print the output. If FLASE, no output is
##' created, and a matrix is invisibly returned.
##' @param ... For compatibility with generic. Ignored.
##' @return It is mainly for printing the result. But this function does return
##' a matrix containing what you see in the output invisibly. You can assign it
##' to an object to save it.
##' @author Kazuki Yoshida, Justin Bohn
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{print.TableOne}}, \code{\link{summary.TableOne}},
##' \code{\link{CreateCatTable}}, \code{\link{print.CatTable}}, \code{\link{summary.CatTable}},
##' \code{\link{CreateContTable}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}}
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
##'
##' @export
print.TableOne <- function(x, missing = FALSE,
                           quote = FALSE,
                           test = TRUE, pDigits = 3,

                           ## Categorical options
                           format = c("fp","f","p","pf")[1], # Format f_requency and/or p_ercent
                           exact = NULL,
                           showAllLevels = FALSE,

                           ## Continuous options
                           nonnormal = NULL,

                           ## Common options
                           explain = TRUE,
                           printToggle = TRUE,
                           ...) {

    ## Get the mixed element only
    TableOne <- x$TableOne

    ## Get the formatted tables
    formattedTables <- sapply(TableOne,
                              FUN = function(tableObj) {
                                  
                                  print(tableObj, printToggle = FALSE)  # Method dispatch at work
                              },
                              simplify = FALSE)

    ## Get the column width information (strata x vars format)
    columnWidthInfo <- sapply(formattedTables,
                              FUN = function(matObj) {
                                  
                                  attributes(matObj)$vecColWidths
                              },
                              simplify = TRUE)

    ## Get the max values for each stratum
    vecMaxValues <- apply(columnWidthInfo, MARGIN = 1, FUN = max, na.rm = TRUE)

    ## Get the difference (value - max. Must be negated)
    nSpacesToAdd <- sweep(x      = columnWidthInfo,
                          MARGIN = 1,
                          STATS  = vecMaxValues,
                          FUN    = "-"
                          )
    nSpacesToAdd <- -1 * nSpacesToAdd


    ## For each matrix, add spaces
    spaceFormattedTables <- sapply(seq_along(formattedTables),
                                   FUN = function(i) {

                                       ## For i-th variable
                                       matObj <- formattedTables[[i]]
                                       nSpaces <- nSpacesToAdd[, i]

                                       ## For j-th stratum (column). Be aware of the p-value column
                                       for (j in seq_along(nSpaces)) {

                                           matObj[, j] <- paste0(paste0(rep(" ", nSpaces[j]), collapse = ""),
                                                                 matObj[, j])
                                       }

                                       ## Return the adjusted table
                                       matObj
                                   },
                                   simplify = FALSE)

    ## Set aside the n row (stratum sizes). 1st element, 1st row
    stratumSizesRow <- spaceFormattedTables[[1]][1, , drop = FALSE]

    ## Remove 1st rows from each table (stratum sizes)
    spaceFormattedTables <- sapply(spaceFormattedTables,
                                   FUN = function(matObj) {
                                       
                                       matObj[-1, , drop = FALSE]
                                   },
                                   simplify = FALSE)

    ## Row-combin n and all variables
    out <- do.call(rbind, c(list(stratumSizesRow), spaceFormattedTables))

    ## Modular version of quote/print toggle.
    out <- ModuleQuoteAndPrintMat(matObj = out,
                                  quote = quote, printToggle = printToggle)

    ## Return the result
    return(invisible(out))
}


















