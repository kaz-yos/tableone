##' Format and print the \code{CatTable} class objects
##'
##' This is the \code{print} method for the \code{CatTable} class objects created by \code{\link{CreateCatTable}} function.
##'
##' @param x The result of a call to the \code{\link{CreateCatTable}} function.
##' @param digits Number of digits to print in the table.
##' @param pDigits Number of digits to print for p-values.
##' @param quote Whether to show everything in quotes. The default is FALSE. If TRUE, everything including the row and column names are quoted so that you can copy it to Excel easily.
##' @param missing Whether to show missing data information (not implemented yet, placeholder)
##' @param explain Whether to add explanation to the variable names, i.e., (\%) is added to the variable names when percentage is shown.
##' @param printToggle Whether to print the output. If FLASE, no output is created, and a matrix is invisibly returned.
##' @param noSpaces Whether to remove spaces added for alignment. Use this option if you prefer to align numbers yourself in other software.
##' @param format The default is "fp" frequency (percentage). You can also choose from "f" frequency only, "p" percentage only, and "pf" percentage (frequency).
##' @param showAllLevels Whether to show all levels. FALSE by default, i.e., for 2-level categorical variables, only the higher level is shown to avoid redundant information.
##' @param cramVars A character vector to specify the two-level categorical variables, for which both levels should be shown in one row.
##' @param test Whether to show the p-values. TRUE by default. If FALSE, only the numerical summaries are shown.
##' @param exact A character vector to specify the variables for which the p-values should be those of exact tests. By default all p-values are from large sample approximation tests (chisq.test).
##' @param CrossTable Whether to show the cross table objects held internally using gmodels::CrossTable function. This will give an output similar to the PROC FREQ in SAS.
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
##' ## Create an overall table for categorical variables
##' catVars <- c("status","ascites","hepato","spiders","edema","stage")
##' catTableOverall <- CreateCatTable(vars = catVars, data = pbc)
##'
##' ## Simply typing the object name will invoke the print.CatTable method,
##' ## which will show the sample size, frequencies and percentages.
##' ## For 2-level variables, only the higher level is shown for simplicity.
##' catTableOverall
##'
##' ## If you need to show both levels for some 2-level factors, use cramVars
##' print(catTableOverall, cramVars = "hepato")
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
##' ## print now includes p-values which are by default calculated by chisq.test.
##' ## It is formatted at the decimal place specified by the pDigits argument
##' ## (3 by default). It does <0.001 for you.
##' catTableBySexTrt
##'
##' ## The exact argument toggles the p-values to the exact test result from
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
##' ## If you want to center-align values in Word, use noSpaces option.
##' print(catTableBySexTrt, exact = "ascites", quote = TRUE, noSpaces = TRUE)
##'
##' @export
print.CatTable <- function(x,                        # CatTable object
                           digits = 1, pDigits = 3,  # Number of digits to show
                           quote         = FALSE,    # Whether to show quotes

                           missing       = FALSE,    # Show missing values (not implemented yet)
                           explain       = TRUE,     # Whether to show explanation in variable names
                           printToggle   = TRUE,     # Whether to print the result visibly
                           noSpaces      = FALSE,    # Whether to remove spaces for alignments

                           format        = c("fp","f","p","pf")[1], # Format f_requency and/or p_ercent
                           showAllLevels = FALSE,
                           cramVars      = NULL,     # variables to be crammed into one row

                           test          = TRUE,     # Whether to add p-values
                           exact         = NULL,     # Which variables should be tested with exact tests

                           CrossTable    = FALSE,    # Whether to show gmodels::CrossTable

                           ...) {

    ## x and ... required to be consistent with generic print(x, ...)
    CatTable <- x

### Check the data structure first

    ## CatTable has a strata(list)-variable(list)-table(dataframe) structure
    ## Get the position of the non-null element
    logiNonNullElement <- !sapply(CatTable, is.null)
    ## Stop if all elements are null.
    if (sum(logiNonNullElement) == 0) {stop("All strata are null strata. Check data.")}
    ## Get the first non-null position
    posFirstNonNullElement <- which(logiNonNullElement)[1]
    ## Save variable names using the first non-null element
    varNames <- names(CatTable[[posFirstNonNullElement]])
    ## Check the number of variables (list length)
    nVars <- length(varNames)


    ## Returns a numeric vector: 1 for approx test variable; 2 for exact test variable
    exact <- ModuleHandleDefaultOrAlternative(switchVec       = exact,
                                              nameOfSwitchVec = "exact",
                                              varNames        = varNames)


    ## Check format argument. If it is broken, choose "fp" for frequency (percent)
    if (!length(format) == 1  | !format %in% c("fp","f","p","pf")) {
        warning("format only accepts one of fp, f, p, or pf. Choosing fp.")
        format <- "fp"
    }

    ## Obtain the strata sizes in a character vector. This has to be obtained from the original data
    ## Added as the top row later
    strataN <- sapply(CatTable,
                      FUN = function(stratum) { # loop over strata
                          ## each stratum is a list of one data frame for each variable
                          ## Obtain n from all variables and all levels (list of data frames)
                          n <- unlist(sapply(stratum, getElement, "n"))
                          ## Pick the first non-null element
                          n[!is.null(n)][1]
                          ## Convert NULL to 0
                          ifelse(is.null(n), "0", as.character(n))
                      },
                      simplify = TRUE) # vector with as many elements as strata

    ## Provide indicators to show what columns were added.
    wasLevelColumnAdded  <- FALSE
    wasPValueColumnAdded <- FALSE
    wasExactColumnAdded  <- FALSE


### Formatting for printing

    ## Create format for percent used in the loop
    fmt1 <- paste0("%.", digits, "f")

    ## Obtain collpased result
    CatTableCollapsed <-
        sapply(X = CatTable,   # Loop over strata
               FUN = function(LIST) {

                   ## Do the following formatting only if the stratum is non-null. Do nothing if null.
                   if (!is.null(LIST)) {

                       ## Returns an empty list if the stratum is null (empty).
                       LIST <-
                           sapply(X = seq_along(LIST), # Loop over variables (list element is DF)
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
                                      DF[c("p.miss","percent","cum.percent")] <-
                                          lapply(X = DF[c("p.miss","percent","cum.percent")],
                                                 FUN = sprintf,
                                                 fmt = fmt1)


                                      ## Make all variables strings (freq is an integer, so direct convert ok)
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
                                          if (unique(DF$var)  %in% cramVars) {
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
                                              DF$var <- with(DF, paste0(var, " = ", level))
                                              DF <- DF[-1, , drop = FALSE]
                                          }

                                      } else if (!showAllLevels & nRow > 2) {
                                          ## If showAllLevels is FALSE AND there are MORE THAN two levels,
                                          ## add an empty row and put the var name, then levels below.
                                          DF <- rbind(rep("", ncol(DF)),
                                                      DF)
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


                       ## Collapse DFs within each stratum
                       DF <- do.call(rbind, LIST)

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
                                                nCharPercent = nCharPercent)
                                           )

                       ## Return a data frame (2014-02-12 sapply breaks attributes?)
                       DF
                   } # end of non-null condition (Null strata skip all this. No action.)

               }, simplify = FALSE)


### Obtain the original column width in characters for alignment in print.TableOne
    ## Name of the column to keep
    widthCol <- c("nCharFreq","nCharFreq","nCharPercent","nCharPercent")[format == c("fp","f","p","pf")]
    vecColWidths <- sapply(CatTableCollapsed,
                            FUN = function(LIST) {

                                ## Get the width of the column (freq or percent, whichever comes left)
                                out <- attributes(LIST)[widthCol]
                                ## Return NA if null
                                if (is.null(out)) {
                                    return(NA)
                                } else {
                                    return(as.numeric(out))
                                }
                            },
                            simplify = TRUE)


    ## Fill the null element using the first non-null element's dimension (Make sure to erase data)
    CatTableCollapsed[!logiNonNullElement] <- CatTableCollapsed[posFirstNonNullElement]
    ## Access the filled-in data frames, and erase them with place holders.
    for (i in which(!logiNonNullElement)) {
        ## Replace all elements with a place holder variable by variable
        CatTableCollapsed[[i]][] <- lapply(CatTableCollapsed[[i]][],
                                           function(var) {

                                               var <- rep("-", length(var))
                                           })
    }

    ## Choose the column name for the right format
    nameResCol <- c("freqPer","freq","percent","perFreq")[format == c("fp","f","p","pf")]


    ## Create output matrix without variable names with the right format
    out <- do.call(cbind, lapply(CatTableCollapsed, getElement, nameResCol))
    out <- as.matrix(out)

    ## Add column names if multivariable stratification is used. (No column names added automatically)
    if (length(attr(CatTable, "dimnames")) > 1) {

        colnames(out) <- ModuleCreateStrataNames(CatTable)
    }

    
    ## Set the variables names
    rownames(out) <- CatTableCollapsed[[posFirstNonNullElement]][,"var"]
    ## Get positions of rows with variable names
    logiNonEmptyRowNames <- CatTableCollapsed[[posFirstNonNullElement]][, "firstRowInd"] != ""



    ## Add level names if showAllLevels is TRUE. This adds the level column. Need come after column naming.
    if (showAllLevels) {
        out <- cbind(level = CatTableCollapsed[[posFirstNonNullElement]][,"level"], # Cannot be DF
                     out)
        ## Changed the indicator
        wasLevelColumnAdded  <- TRUE
    }


    ## Add p-values when requested and available
    if (test == TRUE & !is.null(attr(CatTable, "pValues"))) {

        ## Pick test types used (used for annonation)
        testTypes <- c("","exact")[exact]

        ## Pick the p-values requested, and format like <0.001
        pVec <- ModulePickAndFormatPValues(TableObject = CatTable,
                                           switchVec   = exact,
                                           pDigits     = pDigits)

        ## Create an empty p-value column and test column
        out <- cbind(out,
                     p     = rep("", nrow(out))) # Column for p-values
        ## Put the values at the non-empty positions
        out[logiNonEmptyRowNames,"p"] <- pVec

        ## Change the indicator
        wasPValueColumnAdded <- TRUE


        ## Create an empty test type column, and add test types
        out <- cbind(out,
                     test = rep("", nrow(out))) # Column for test types
        ## Put the test types  at the non-empty positions (all rows in continuous!)
        out[logiNonEmptyRowNames,"test"] <- testTypes

        ## Change the indicator
        wasExactColumnAdded <- TRUE
    }


    ## Add freq () explanation if requested
    if (explain) {
        ## Choose the format of the explanation string
        explainString <- c(" (%)", "", " (%)", " % (freq)")[format == c("fp","f","p","pf")]
        ## Only for rows with row names
        rownames(out)[logiNonEmptyRowNames] <- paste0(rownames(out)[logiNonEmptyRowNames],
                                                      explainString)
    }

    ## Keep column names (strataN does not have correct names if stratification is by multiple variables)
    outColNames <- colnames(out)
    ## Add n at the correct location depending on the number of columns added (level and/or p)
    out <- rbind(n = c(level = rep("", wasLevelColumnAdded), # Add "" padding if level added
                     strataN,
                     p       = rep("", wasPValueColumnAdded), # Add "" padding if p-value added
                     test    = rep("", wasExactColumnAdded)   # Add "" padding if exact test used
                     ),
                 out)
    ## Put back the column names (overkill for non-multivariable cases)
    colnames(out) <- outColNames

    ## Add stratification information to the column header depending on the dimension
    names(dimnames(out)) <- ModuleReturnDimHeaders(CatTable)

    ## Remove spaces if asked.
    out <- ModuleRemoveSpaces(mat = out, noSpaces = noSpaces)

    ## Modular version of quote/print toggle.
    out <- ModuleQuoteAndPrintMat(matObj = out,
                                  quote = quote, printToggle = printToggle)

    ## Print CrossTable() if requested
    if (CrossTable) {

        junk <- lapply(attributes(CatTable)$xtabs, gmodels::CrossTable)
    }

    ## Add attributes for column widths in characters
    attributes(out) <- c(attributes(out),
                         list(vecColWidths = vecColWidths))

    ## return a matrix invisibly
    return(invisible(out))
}
