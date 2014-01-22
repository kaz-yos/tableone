summary.ContTable <- function(ContTable, digits = 2) {

    ## for (i in seq_along(ContTable)) {

    ##     cat(names(ContTable)[i], "\n")
        
    ##     ## print(round(ContTable[[i]], digits = digits))
    ##     print(ContTable[[i]])

    ##     cat("\n")
    ## }

    print.by(ContTable, digits = digits)
}
