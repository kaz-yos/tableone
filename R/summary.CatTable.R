summary.CatTable <- function(CatTable, digits = 1) {

    ## Create format
    fmt <- paste0("%.", digits, "f")

    ## Obtain collpased result
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
                                      DF[c("percent","cum.percent")] <-
                                          lapply(X = DF[c("percent","cum.percent")],
                                                 FUN = sprintf,
                                                 fmt = fmt)

                                      ## Make var and level a string
                                      DF[c("var","level")] <-
                                          lapply(X = DF[c("var","level")],
                                                 FUN = as.character)
                                      
                                      ## Delete n and miss except in the first row
                                      DF[-1, c("var","n","miss")] <- ""

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

    ## Restore the dimnames through attributes()
    attributes(CatTableCollapsed) <- c(attributes(CatTableCollapsed), attributes(CatTable))

    ## Print forcing the print.by method. Do not show row names.
    print.by(CatTableCollapsed, digits = digits, row.names = FALSE)
}


















