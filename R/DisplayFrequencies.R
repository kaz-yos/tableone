DisplayFrequencies <-
function(resFrequencies, missing = FALSE, digits = 2) {

    ## Create a list of variables
    listFrequencies <-
        lapply(resFrequencies,
               FUN = function(LIST) {

                   ## Remove cumulative %
                   res <- LIST$Frequencies[, c("Value", "# of Cases", "      %")]

                   if (missing == TRUE) {

                       ## Calculate proportion missing
                       propMissinng <- LIST$case.summary[,"Missing"] / LIST$case.summary[,"Total"]
                       propMissinng <- round(propMissinng, digits = digits)

                       ## Add to matrix
                       res <- cbind(res, "%Missing" = propMissinng)
                   }

                   ## Return results
                   res
               })

    ## Combined them as a data frame
    do.call(rbind, listFrequencies)
}
