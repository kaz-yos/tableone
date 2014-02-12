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

    ## first some prep (convert to matrix)
    ## is there a better way to retrieve the matrix format than below?
    ## this way works, but you can't suppress the printing

    matCatTable  <- print(x$CatTable,
                          test = test, pDigits = pDigits,
                          ## Categorical options
                          exact = exact,
                          format = format,
                          showAllLevels = showAllLevels,
                          ## Common options
                          explain = explain,
                          printToggle = FALSE) # Turn off printing, and return values

    matContTable <- print(x$ContTable,
                          test = test, pDigits = pDigits,
                          ## Continuous options
                          nonnormal = nonnormal,
                          ## Common options
                          explain = explain,
                          printToggle = FALSE) # Turn off printing, and return values

    ## Clean the first row
    matContTable[1, ] <- rep("", length(matContTable[1, ]))
    rownames(matContTable)[1] <- ""

    ## rbind and delete the duplicated row
    matCatContTable <- rbind(matCatTable,
                             matContTable)

    ## Return the result
    return(matCatContTable)
}


















