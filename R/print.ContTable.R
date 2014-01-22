## Print method for a continuous table
print.ContTable <- function(ContTable, missing = FALSE, digits = 2, nonnormal = FALSE) {

    ## nonnormal: indicator vector for nonnormality.
    if (nonnormal == FALSE) {

        ## If the result is stratified
        if (length(ContTable) > 1) {
            
            ## Loop over strata
            LIST <- lapply(ContTable,
                           function(MAT) {
                               
                               ## mean (sd) version
                               fmt <- paste0("%.",digits,"f"," (%.",digits,"f",")")
                               VEC <- sprintf(fmt = fmt,
                                              MAT[, "mean"],
                                              MAT[, "sd"]
                                              )

                               ## Name variables
                               names(VEC) <- rownames(MAT)

                               ## Return as a vector
                               VEC
                           })

            ## Combine as columns to form a matrix
            out <- do.call(cbind, LIST)

            ## Show with quotes
            print(out, quote = FALSE)

            
            ## If the result is NOT stratified
        } else if (length(ContTable) == 1) {

            MAT <- ContTable[[1]]
            
            ## mean (sd) version
            fmt <- paste0("%.",digits,"f"," (%.",digits,"f",")")
            VEC <- sprintf(fmt = fmt,
                           MAT[, "mean"],
                           MAT[, "sd"]
                           )

            ## Name variables
            names(VEC) <- rownames(MAT)
            
            out <- data.frame(Overall = VEC)

            print(out)
        }

################################################################################

        ## nonormal case
    } else if (nonnormal == TRUE) {

        ## If the result is stratified
        if (length(ContTable) > 1) {
            
            ## Loop over elements
            LIST <- lapply(ContTable,
                           function(MAT) {
                               ## Format median [p25, p75]
                               fmt <- paste0("%.",digits,"f [%.",digits,"f, %.",digits,"f]")
                               VEC <- sprintf(fmt = fmt,
                                              MAT[, "median"],
                                              MAT[, "q25"],
                                              MAT[, "q75"]
                                              )

                               ## Name variables
                               names(VEC) <- rownames(MAT)

                               ## Return as a vector
                               VEC
                           })

            ## Combine as columns to form a matrix
            out <- do.call(cbind, LIST)

            ## Show with quotes
            print(out, quote = FALSE)


            ## If the result is NOT stratified
        } else (length(ContTable) == 1) {
            
            MAT <- ContTable[[1]]
            
            ## Format median [p25, p75]
            fmt <- paste0("%.",digits,"f [%.",digits,"f, %.",digits,"f]")
            VEC <- sprintf(fmt = fmt,
                           MAT[, "median"],
                           MAT[, "q25"],
                           MAT[, "q75"]
                           )

            ## Name variables
            names(VEC) <- rownames(MAT)
            
            out <- data.frame(Overall = VEC)

            print(out)
        } else {stop("Neither stratified or non-stratified!?")}

    } else {stop("Neither nonnormal or normal!?")} ## nonnormal conditions end here
    
}
