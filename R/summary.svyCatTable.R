##' Shows all results in a \code{svyCatTable} class object
##'
##' Shows all data a \code{svyCatTable} class object has. This includes the (optionally stratified) part with summary statistics and, if available, p-values from the approximation method test (\code{chisq.test} by default) and exact method test (\code{fisher.test} by default) and standardized mean differences of all possible pairwise contrasts.
##'
##' @param object An object that has the \code{svyCatTable} class to be shown.
##' @param digits Number of digits to print.
##' @param ... For compatibility with generic. Ignored.
##' @return None. Results are printed.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{svyCreateTableOne}}, \code{\link{svyCreateCatTable}},  \code{\link{print.svyCatTable}}
##' @examples
##'
##' ## See the examples for svyCreateTableOne()
##'
##' @export
summary.svyCatTable <- function(object, digits = 1, ...) {

    ## object and ... required to be consistent with generic summary(object, ...)
    CatTable <- object

    ## Create format
    fmt <- paste0("%.", digits, "f")
    varsNumeric <- c("n","miss","p.miss","freq","percent","cum.percent")

    ## Obtain collpased result within each stratum
    CatTableCollapsed <-
        sapply(X = CatTable,   # Loop over strata
               FUN = function(LIST) {

                   LIST <- sapply(X = seq_along(LIST), # Loop over variables
                                  FUN = function(i) {

                                      ## Extract the data frame
                                      DF <- LIST[[i]]

                                      ## Extract the variable name
                                      varName <- names(LIST)[i]

                                      ## Check number of rows (levels)
                                      nRow <- nrow(DF)

                                      ## Add a variable name to the left as a character vector
                                      DF <- cbind(var = rep(varName, nRow),
                                                  DF)

                                      ## Format percent and cum.percent
                                      DF[varsNumeric] <- lapply(X = DF[varsNumeric],
                                                                FUN = sprintf,
                                                                fmt = fmt)

                                      ## Make var and level a string
                                      DF[c("var","level")] <-
                                          lapply(X = DF[c("var","level")],
                                                 FUN = as.character)

                                      ## Delete n and miss except in the first row
                                      DF[-1, c("var","n","miss","p.miss")] <- ""

                                      ## row bind an empty row
                                      DF <- rbind(DF,
                                                  rep("", ncol(DF)))

                                      ## Return a data frame
                                      DF
                                  },
                                  simplify = FALSE)

                   ## Collapse DFs within each stratum
                   DF <- do.call(rbind, LIST)

                   ## Return a data frame
                   DF
               }, simplify = FALSE)

    ## Change to an array to use print.by()
    CatTableCollapsed <- as.array(CatTableCollapsed)
    ## Force dimname label as the strataVarName
    names(dimnames(CatTableCollapsed)) <- attr(CatTable, "strataVarName")

    ## Print forcing the print.by method. Do not show row names.
    print.by(CatTableCollapsed, digits = digits, row.names = FALSE)

    ## Print p-values if it exist
    if (!is.null(attributes(CatTable)$pValues)) {
        cat("\np-values\n")
        print(attributes(CatTable)$pValues)
    }

    ## Print SMDs if it exist
    if (!is.null(attributes(CatTable)$smd)) {
        cat("\nStandardize mean differences\n")
        print(attributes(CatTable)$smd)
    }
}
