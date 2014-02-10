################################################################################
### Modules for tableone
## This file contains common modules used in the tableone package.
## Created on: 2014-02-10
## Author: Kazuki Yoshida
################################################################################


### Modules intented for the print methods
################################################################################

### ModuleQuoteAndPrintMat()
## Takes an matrix object format, print, and (invisibly) return it
ModuleQuoteAndPrintMat <- function(matObj, quote = FALSE, print = TRUE) {

    ## Add quotes for names if requested
    if (quote) {
        rownames(matObj) <- paste0('"', rownames(matObj), '"')
        colnames(matObj) <- paste0('"', colnames(matObj), '"')
        names(dimnames(matObj)) <- paste0('"', names(dimnames(matObj)), '"')
    }

    ## Print the results with or withmatObj quote
    print(matObj, quote = quote)

    ## Return invisibly if printing
    if (print) {
        return(invisible(matObj))
    } else if (!print) {
        ## If not printing, return visibly
        return(matObj)
    }
}
