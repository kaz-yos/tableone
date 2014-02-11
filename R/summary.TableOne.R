### Placeholder
##' Shows all results in a \code{TableOne} class object
##'
##' This method shows all the data a CatTable class object has. This includes
##' the (optionally stratified) part with summary statistics and p-values from
##' the approximation method test (chisq.test by default) and exact method test
##' (fisher.test by default).
##'
##'
##' @param object An object that has the \code{CatTable} class to be shown.
##' @param digits Number of digits to print.
##' @param ... For compatibility with generic. Ignored.
##' @return It will print the results.
##' @author Kazuki Yoshida
##' @seealso \code{\link{CreateCatTable}}, \code{\link{print.CatTable}}, \code{\link{CreateContTable}},
##' \code{\link{print.ContTable}}, \code{\link{summary.ContTable
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
##' catTableOverall <- CreateTableOne(vars = catVars, data = pbc)
##'
##' ## Simply typing the object name will invoke the print.TableOne method,
##' ## which will show the sample size, frequencies and percentages.
##' ## For 2-level variables, only the higher level is shown for simplicity.
##' catTableOverall
##'
##' ## To further examine the variables, use the summary.TableOne method,
##' ## which will show more details.
##' summary(catTableOverall)
##'
##' @export
summary.TableOne <- function(object, digits = 1, ...) {

    ## object and ... required to be consistent with generic summary(object, ...)
    TableOne <- object

    ## Create format
    fmt <- paste0("%.", digits, "f")

    ## Restore the dimnames through attributes()
    attributes(TableOneCollapsed) <- c(attributes(TableOneCollapsed), attributes(TableOne))

    ## Print forcing the print.by method. Do not show row names.
    print.by(TableOneCollapsed, digits = digits, row.names = FALSE)

    ## Print p-values if it exist
    if (!is.null(attributes(TableOne)$pValues)) {
        cat("\np-values\n")
        print(attributes(TableOne)$pValues)
    }
}
