summary.ContTable <- function(ContTable, digits = 3) {

    for (i in seq_along(ContTable)) {

        cat(names(ContTable)[i], "\n")
        
        ## print(round(ContTable[[i]], digits = digits))
        print(ContTable[[i]])

        cat("\n")
    }
}
