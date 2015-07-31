##' Format and print the \code{TableOne} class objects
##'
##' This is the \code{print} method for the \code{TableOne} class objects created by \code{\link{CreateTableOne}} function.
##'
##' @param x The result of a call to the \code{\link{CreateTableOne}} function.
##' @param catDigits Number of digits to print for proportions. Default 1.
##' @param contDigits Number of digits to print for continuous variables. Default 2.
##' @param pDigits Number of digits to print for p-values. Default 3.
##' @param quote Whether to show everything in quotes. The default is FALSE. If TRUE, everything including the row and column names are quoted so that you can copy it to Excel easily.
##' @param missing Whether to show missing data information (not implemented yet, placeholder)
##' @param explain Whether to add explanation to the variable names, i.e., (\%) is added to the variable names when percentage is shown.
##' @param printToggle Whether to print the output. If FLASE, no output is created, and a matrix is invisibly returned.
##' @param test Whether to show the p-values. TRUE by default. If FALSE, only the numerical summaries are shown.
##' @param smd Whether to show the standardized mean difference. If there are more than one contrasts, the average of all possible standardized mean differences is shown. For categorical variables, Yang and Dalton's definition is used.
##' @param noSpaces Whether to remove spaces added for alignment. Use this option if you prefer to align numbers yourself in other software.
##' @param format The default is "fp" frequency (percentage). You can also choose from "f" frequency only, "p" percentage only, and "pf" percentage (frequency).
##' @param showAllLevels Whether to show all levels. FALSE by default, i.e., for 2-level categorical variables, only the higher level is shown to avoid redundant information.
##' @param cramVars A character vector to specify the two-level categorical variables, for which both levels should be shown in one row.
##' @param exact A character vector to specify the variables for which the p-values should be those of exact tests. By default all p-values are from large sample approximation tests (chisq.test).
##' @param nonnormal A character vector to specify the variables for which the p-values should be those of nonparametric tests. By default all p-values are from normal assumption-based tests (oneway.test).
##' @param minMax Whether to use [min,max] instead of [p25,p75] for nonnormal variables. The default is FALSE.
##' @param ... For compatibility with generic. Ignored.
##' @return It is mainly for printing the result. But this function does return a matrix containing what you see in the output invisibly. You can assign it to an object to save it.
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
##' ## Make categorical variables factors
##' varsToFactor <- c("status","trt","ascites","hepato","spiders","edema","stage")
##' pbc[varsToFactor] <- lapply(pbc[varsToFactor], factor)
##'
##' ## Create Table 1 stratified by sex and trt
##' tableOne <- CreateTableOne(vars = c("time","status","age","ascites","hepato",
##'                                     "spiders","edema","bili","chol","albumin",
##'                                     "copper","alk.phos","ast","trig","platelet",
##'                                     "protime","stage"),
##'                            strata = c("sex","trt"), data = pbc)
##'
##' ## Just typing the object name will invoke the print.TableOne method
##' tableOne
##'
##' ## Specifying nonnormal variables will show the variables appropriately,
##' ## and show nonparametric test p-values. Specify variables in the exact
##' ## argument to obtain the exact test p-values. cramVars can be used to
##' ## show both levels for a 2-level categorical variables.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), cramVars = "hepato")
##'
##' ## Use the summary.TableOne method for detailed summary
##' summary(tableOne)
##'
##' ## See the categorical part only using $ operator
##' tableOne$CatTable
##' summary(tableOne$CatTable)
##'
##' ## See the continuous part only using $ operator
##' tableOne$ContTable
##' summary(tableOne$ContTable)
##'
##' ## If your work flow includes copying to Excel and Word when writing manuscripts,
##' ## you may benefit from the quote argument. This will quote everything so that
##' ## Excel does not mess up the cells.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), cramVars = "hepato", quote = TRUE)
##'
##' ## If you want to center-align values in Word, use noSpaces option.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), cramVars = "hepato", quote = TRUE, noSpaces = TRUE)
##'
##' @export
print.TableOne <-
function(x,                   # TableOne object
         catDigits = 1, contDigits = 2, pDigits = 3, # Number of digits to show
         quote         = FALSE,       # Whether to show quotes

         ## Common options
         missing       = FALSE, # Not implemented yet
         explain       = TRUE,  # Whether to show explanation in variable names
         printToggle   = TRUE,  # Whether to print the result visibly
         test          = TRUE,  # Whether to add p-values
         smd           = FALSE,  # Whether to add standardized mean differences
         noSpaces      = FALSE, # Whether to remove spaces for alignments

         ## Categorical options
         format        = c("fp","f","p","pf")[1], # Format f_requency and/or p_ercent
         showAllLevels = FALSE, # Show all levels of a categorical variable
         cramVars      = NULL,  # Which 2-level variables to show both levels in one row
         exact         = NULL,  # Which variables should be tested with exact tests

         ## Continuous options
         nonnormal     = NULL,  # Which variables should be treated as nonnormal
         minMax        = FALSE, # Whether to show median

         ...) {

    ## Extract Cont/CatTable elements of x and dispatch print() appropriately
    FmtTables <- ModuleFormatTables(x,
                                    catDigits = catDigits, contDigits = contDigits,
                                    test = test, smd = smd,
                                    explain = explain, pDigits = pDigits,

                                    ## print.CatTable arguments passed
                                    format = format, exact = exact,
                                    ## Returns one more column if TRUE
                                    showAllLevels = showAllLevels,
                                    cramVars = cramVars,

                                    ## print.ContTable arguments passed
                                    nonnormal = nonnormal, minMax = minMax,
                                    insertLevel = showAllLevels)

    ## List of stratum sample size row only tables
    FmtStratumSizesTables <- sapply(FmtTables,
                                    FUN = ModuleStratumSizesRow,
                                    showAllLevels = showAllLevels,
                                    simplify = FALSE)
    names(FmtStratumSizesTables) <- paste0(names(FmtStratumSizesTables), "N")

    ## Combine as a list of necessary table elements
    FmtElementTables <- c(FmtTables, FmtStratumSizesTables)


    ## Add space paddings
    ## Given a list of tables with vecColWidths,
    ## return a strata-by-table df containing spaces to add
    nSpacesToAdd <- ModuleNSpacesToAdd(FmtElementTables)
    ## Actually add spaces to tables
    spaceFmtEltTables <- ModuleAddSpacesToTable(FmtElementTables, nSpacesToAdd, showAllLevels)


    ## Create a list of one variable tables excluding sample size row
    lstOneVarTables <- ModuleListOfOneVarTables(spaceFmtEltTables,
                                                MetaData = x$MetaData)


    ## Check if the first row is CatTable element
    ## if so, pick sample size row from CatTable element
    ## Intentionally a one-element list
    lstStratumSizesRow <- ifelse(x$MetaData$logiFactors[1],
                                 ## Change this to spaceFmtEltTables$ after tests pass
                                 ## These are not space-padded yet
                                 list(FmtElementTables$FmtCatTableN),
                                 list(FmtElementTables$FmtContTableN))

    ## Row-combin n and all variables
    out <- do.call(rbind,
                   ## List concatenation (both are lists)
                   c(lstStratumSizesRow, lstOneVarTables))


    ## Add stratification information to the column header (This is also in the constructor)
    if (length(x$ContTable) > 1 ) {
        ## Combine variable names with : in between
        strataVarName <- attributes(x$ContTable)$strataVarName

        ## Create strata string
        strataString <- paste0("Stratified by ", strataVarName)

        ## Name the row dimension with it. 1st dimension name should be empty.
        names(dimnames(out)) <- c("", strataString)
    } else {

        names(dimnames(out)) <- c("", "")
    }

    ## Remove spaces if asked.
    out <- ModuleRemoveSpaces(mat = out, noSpaces = noSpaces)

    ## Modular version of quote/print toggle.
    out <- ModuleQuoteAndPrintMat(matObj = out,
                                  quote = quote, printToggle = printToggle)

    ## Return the result
    return(invisible(out))
}
