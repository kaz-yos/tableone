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
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{print.TableOne}}, \code{\link{summary.TableOne}},
##' \code{\link{CreateContTable}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}},
##' \code{\link{CreateCatTable}}, \code{\link{print.CatTable}}, \code{\link{summary.CatTable}}
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
##' @export
summary.TableOne <- function(object, digits = 1, ...) {

    ## object and ... required to be consistent with generic summary(object, ...)
    listCatContTables <- object

    ## Categorical
    cat("Summary of categorical variables.\n\n")
    summary(listCatContTables$CatTable, digits = digits)
    
    ## Continuous
    cat("Summary of continuous variables.\n\n")    
    summary(listCatContTables$CatTable, digits = digits)    
}
