##' Shows all results in a \code{ContTable} class object
##'
##' This method shows all the data a \code{ContTable} class object has. This includes
##' the (optionally stratified) part with summary statistics and p-values from
##' the normal assupmtion-based test (oneway.test by default) and nonparametric
##' test (kruskal.test by default).
##'
##'
##' @param object An object that has the \code{ContTable} class to be shown.
##' @param digits Number of digits to print.
##' @param ... For compatibility with generic. Ignored.
##' @return It will print the results.
##' @author Kazuki Yoshida
##' @seealso CreateContTable, print.ContTable, CreateCatTable, print.CatTable,
##' summary.CatTable
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
##' @S3method summary ContTable
##' @export summary.ContTable
summary.ContTable <- function(object, digits = 2, ...) {

    ## Just call print.by
    print.by(object, digits = digits)

    ## Print p-values if it exist
    if (!is.null(attributes(object)$pValues)) {
        cat("\np-values\n")
        print(attributes(object)$pValues)
    }
}
