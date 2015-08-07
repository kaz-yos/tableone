##' Extract standardized mean differences from a (svy)TableOne object
##'
##' Extracts standardized mean differences data as a vector or matrix from a (svy)TableOne object
##'
##'
##' @param x A stratified (svy)TableOne object containing standardized mean differences.
##' @return A vector or matrix containing the average standardized mean differences (if more than two contrasts exist) as well as the all possible pairwise standardized mean differences. Variables are ordered in the same order as the printed table.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{svyCreateTableOne}}
##' @examples
##'
##' ## See examples for CreateTableOne and svyCreateTableOne
##'
##' @export
ExtractSmd <- function(x) {

    if (class(x)[1] %in% c("TableOne","svyTableOne")) {

        ## Extract SMD from both continuous and categorical
        matSmd <- rbind(attr(x$ContTable, "smd"),
                        attr(x$CatTable,  "smd"))

        ## Order by table variable order
        matSmd[x$MetaData$vars,]

    } else if (class(x)[1] %in% c("ContTable","svyContTable","CatTable","svyCatTable")) {

        ## If not a mixed table object, just get attribute
        attr(x, "smd")

    } else {

        warning("Unsupported object of class: ", class(x))
    }
}
