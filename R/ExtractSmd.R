##' Extract standardized mean differences from a (svy)TableOne object
##'
##' Extracts standardized mean differences data as a vector or matrix from a (svy)TableOne object
##'
##'
##' @param x A stratified (svy)TableOne object containing standardized mean differences.
##' @param varLabels Whether to replace variable names with variable labels obtained from \code{labelled::var_label()} function.
##'
##' @return A vector or matrix containing the average standardized mean differences (if more than two contrasts exist) as well as the all possible pairwise standardized mean differences. Variables are ordered in the same order as the printed table.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{svyCreateTableOne}}
##' @examples
##'
##' ## See examples for CreateTableOne and svyCreateTableOne
##'
##' @export
ExtractSmd <- function(x, varLabels = FALSE) {

    if (class(x)[1] %in% c("TableOne","svyTableOne")) {

        ## Extract SMD from both continuous and categorical
        matSmd <- rbind(attr(x$ContTable, "smd"),
                        attr(x$CatTable,  "smd"))

        ## Order by table variable order
        matSmd <- matSmd[x$MetaData$vars, , drop = FALSE]

        ## Use variable labels if requested.
        if (varLabels) {
            for (i in seq_along(x$MetaData$vars)) {
                if (!is.null(x$MetaData$varLabels[[i]])) {
                    ## If the corresponding variable label is non-null replace
                    rownames(matSmd)[i] <- x$MetaData$varLabels[[i]]
                }
            }
        }

        ## Return manipulated matrix
        matSmd

    } else if (class(x)[1] %in% c("ContTable","svyContTable","CatTable","svyCatTable")) {

        ## If not a mixed table object, just get attribute
        attr(x, "smd")

    } else {

        warning("Unsupported object of class: ", class(x))
    }
}
