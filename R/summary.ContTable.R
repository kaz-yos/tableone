summary.ContTable <- function(ContTable, digits = 2) {

    ## Just call print.by
    print.by(ContTable, digits = digits)

    ## Print p-values if it exist
    if (!is.null(attributes(ContTable)$pValues)) {
        cat("\np-values\n")
        print(attributes(ContTable)$pValues)
    }
}
