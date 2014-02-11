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
## Requires quote and printToggle argument in the printToggle method
ModuleQuoteAndPrintMat <- function(matObj, quote = FALSE, printToggle = TRUE) {

    ## Add quotes for names if requested
    if (quote) {
        ## row and col names
        rownames(matObj) <- paste0('"', rownames(matObj), '"')
        colnames(matObj) <- paste0('"', colnames(matObj), '"')
        ## dimension name
        names(dimnames(matObj)) <- paste0('"', names(dimnames(matObj)), '"')
        ## 1st (row) dimension needs a preceding space for best copy and paste
        names(dimnames(matObj))[1] <- paste0(" ", names(dimnames(matObj))[1])
    }


    ## print if required and return
    if (printToggle) {
        
        print(matObj, quote = quote)
        return(matObj)
        
    } else if (!printToggle) {

        return(matObj)
    }
}





