summary.CatTable <- function(CatTable, digits = 2) {

    ## Obtain collpased result
    CatTableCollapsed <- sapply(X = CatTable,   # Loop over strata
                              function(LIST) {

                                  LIST <- sapply(X = LIST, # Loop over variables
                                                 FUN = function(DF) {

                                                     ## ## Check number of rows (levels)
                                                     nRow <- nrow(DF)

                                                     ## Delete n and miss except in the first row
                                                     DF[-1, c("n","miss")] <- ""

                                                     ## row bind an empty row
                                                     DF <- rbind(DF, rep("", ncol(DF)))

                                                     ## ## Erase row name for empty line
                                                     rownames(DF)[nRow + 1] <- "_"

                                                     ## ## Give a new class
                                                     ## class(DF) <- c("CatTableStratum", class(DF))

                                                     DF
                                                 },
                                                 simplify = FALSE)

                                   ## Collapse DFs within each stratum
                                  DF <- do.call(rbind, LIST)

                                  ## ## Check the empty rows
                                  ## posBlankRows <- which(DF$freq == "")
                                  ## ## Erase row names
                                  ## rownames(DF)[posBlankRows] <- "-"
                                  
                                  ## Return a data frame
                                  DF
                              }, simplify = FALSE)

    ## Restore the dimnames through attributes()
    attributes(CatTableCollapsed) <- c(attributes(CatTableCollapsed), attributes(CatTable))

    ## Print forcing the print.by method
    print.by(CatTableCollapsed, digits = digits)
}

## Does not work

## ## print method for the data frame object within each stratum
## print.CatTableStratum <- function(CatTableStratum, digits = digits) {

##     ## Check the number of rows
##     nRow <- nrow(CatTableStratum)

##     ## Check the positions of non-empty n
##     posOfNonEmptyN <- which(!CatTableStratum$n == "")
##     ## drop the first one, no need for preceding a blank line
##     posOfNonEmptyN <- tail(posOfNonEmptyN, n = -1)

##     ## Print
##     for (i in seq_len(nRow)) {
##         if (i %in% posOfNonEmptyN) {
##             ## Insert a preceding blank line
##             cat("\n")
##         }

##         ## Show data
##         cat(CatTableStratum[[1]][, i, drop = FALSE], "\n")
##     }
## }
