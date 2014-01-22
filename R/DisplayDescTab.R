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
