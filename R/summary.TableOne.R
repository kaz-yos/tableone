##' Shows all results in a \code{(svy)TableOne} class object
##'
##' Shows all data a \code{(svy)TableOne} class object has. This includes the (optionally stratified) part with summary statistics and p-values and/or standardized mean differences.
##'
##'
##' @param object An object that has the \code{(svy)TableOne} class to be shown.
##' @param digits Number of digits to print.
##' @param ... For compatibility with generic. Ignored.
##' @return None. Results are printed.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{svyCreateCatTable}}
##' @examples
##'
##' ## See examples for CreateTableOne and svyCreateTableOne
##'
##' @export
summary.TableOne <- function(object, digits = 1, ...) {

    ## Continuous
    if (!is.null(object$ContTable)) {
        cat("\n     ### Summary of continuous variables ###\n\n")
        summary(object$ContTable, digits = digits)
    }

    ## Separator
    if ((!is.null(object$ContTable)) & !is.null(object$CatTable)) {
        cat("\n=======================================================================================\n")
    }

    ## Categorical
    if (!is.null(object$CatTable)) {
        cat("\n     ### Summary of categorical variables ### \n\n")
        summary(object$CatTable, digits = digits)
    }
}











