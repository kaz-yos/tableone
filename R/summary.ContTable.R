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
##' \code{\link{CreateContTable}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}},
##' \code{\link{CreateCatTable}},  \code{\link{print.CatTable}},  \code{\link{summary.CatTable}},
##' \code{\link{CreateTableOne}},  \code{\link{print.TableOne}},  \code{\link{summary.TableOne}}
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
##' @export
summary.ContTable <- function(object, digits = 2, ...) {

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
