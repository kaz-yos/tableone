##' Shows all results in a \code{ContTable} class object
##'
##' Shows all data a \code{ContTable} class object has. This includes the (optionally stratified) part with summary statistics and, if available, p-values from the normal assupmtion-based test (\code{oneway.test} by default) and nonparametric test (\code{kruskal.test} by default) and standardized mean differences of all possible pairwise contrasts.
##'
##' @param object An object that has the \code{ContTable} class to be shown.
##' @param digits Number of digits to print.
##' @param ... For compatibility with generic. Ignored.
##' @return None. Results are printed.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{CreateContTable}}, \code{\link{print.ContTable}}
##' @examples
##'
##' ## See examples for CreateTableOne
##'
##' @export
summary.ContTable <- function(object, digits = 2, ...) {

    ## Check if addOverall was used -> this option would delete the dimnames
    if (is.null(attr(object, "dimnames"))) {
        ## recreate dim and dimnames attributes for print.by()
        attr(object, "dim") <- length(attr(object, "names"))
        overall_dimnames <- list()
        overall_dimnames[[attr(object, "strataVarName")]] <- attr(object, "names")
        attr(object, "dimnames") <- overall_dimnames
    }
        
    ## Just call print.by
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
