## Print method for a continuous table
print.ContTable <- function(ContTable, missing = FALSE, digits = 2, nonnormal) {

    ## nonnormal: indicator vector for nonnormality.
    
    if (length(ContTable) > 1) {
        ## Stratification
        

        ## Loop over elements
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

        out <- do.call(cbind, LIST)

        ## out <- data.frame(out)

        print(out, quote = FALSE)
        
    } else {
        ## No stratification
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
}


################################################################################
DisplayDescTab <-
    function(listDescTab, missing = FALSE, digits = 2) {

        matDescTab <-
            sapply(listDescTab,
                   FUN = function(MAT) {

                       if (missing ==  TRUE) {
                           ## Format median [p25, p75] (missing%)
                           fmt <- paste0("%.",digits,"f [%.",digits,"f, %.",digits,"f] (%.",digits,"f)")
                           VEC <- sprintf(fmt = fmt,
                                          MAT[, "Median"],
                                          MAT[, "25th Percentile"],
                                          MAT[, "75th Percentile"],
                                          1 - (MAT[, "Valid N"] / MAT[, "length"])
                                          )
                           
                       } else if (missing ==  FALSE) {
                           ## Format median [p25, p75]
                           fmt <- paste0("%.",digits,"f [%.",digits,"f, %.",digits,"f]")
                           VEC <- sprintf(fmt = fmt,
                                          MAT[, "Median"],
                                          MAT[, "25th Percentile"],
                                          MAT[, "75th Percentile"]
                                          )
                       }

                       ## Name
                       names(VEC) <- row.names(MAT)

                       ## Return results
                       VEC
                   })
        
        matDescTab
    }

