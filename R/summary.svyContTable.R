##' Shows all results in a \code{svyContTable} class object
##'
##' This method shows all the data a \code{svyContTable} class object has. This includes the (optionally stratified) part with summary statistics and , if available, p-values from the normal assupmtion-based test (\code{\link{oneway.test}} by default) and nonparametric test (\code{\link{kruskal.test}} by default).
##'
##' @param object An object that has the \code{svyContTable} class to be shown.
##' @param digits Number of digits to print.
##' @param ... For compatibility with generic. Ignored.
##' @return It will print the results.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{svyCreateContTable}}, \code{\link{print.svyContTable}}, \code{\link{summary.svyContTable}},
##' \code{\link{svyCreateCatTable}},  \code{\link{print.svyCatTable}},  \code{\link{summary.svyCatTable}},
##' \code{\link{svyCreateTableOne}},  \code{\link{print.TableOne}},  \code{\link{summary.TableOne}}
##' @examples
##'
##' ## See the examples for svyCreateTableOne()
##'
##' @export
summary.svyContTable <- function(object, digits = 2, ...) {

    ## Save the initial object
    ContTable <- object

    ## Force an 1-dimensional array
    object <- as.array(object)
    names(dimnames(object)) <- attr(ContTable, "strataVarName")

    ## Just print as a list
    print.by(object, digits = digits)

    ## Print p-values if it exist
    if (!is.null(attributes(object)$pValues)) {
        cat("\np-values\n")
        print(attributes(object)$pValues)
    }
}
