##' Shows all results in a \code{svyContTable} class object
##'
##' Shows all data a \code{svyContTable} class object has. This includes the (optionally stratified) part with summary statistics and, if available, p-values from the normal assupmtion-based test (\code{regTermTest} with \code{svyglm} by default) and nonparametric test (\code{svyranktest} by default) and standardized mean differences of all possible pairwise contrasts.
##'
##' @param object An object that has the \code{svyContTable} class to be shown.
##' @param digits Number of digits to print.
##' @param ... For compatibility with generic. Ignored.
##' @return None. Results are printed.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{svyCreateTableOne}}, \code{\link{svyCreateContTable}}, \code{\link{print.svyContTable}}
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

    ## Print SMDs if it exist
    if (!is.null(attributes(object)$smd)) {
        cat("\nStandardize mean differences\n")
        print(attributes(object)$smd)
    }
}
