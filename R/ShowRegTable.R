ShowRegTable <- function(model, exp = TRUE, digits = 2, pDigits = 3, quote = FALSE) {

    ## Create format
    fmt1 <- paste0("%.",  digits, "f")
    fmt2 <- paste0("%.", pDigits, "f")

    ## model must have summary and confint methods
    modelCoef <- coef(model)
    modelConfInt <- confint(model)

    modelSummaryMat <- coef(summary(model))
    modelP <- modelSummaryMat[,ncol(modelSummaryMat)]


    ##
    resMat <- cbind(beta = modelCoef,
                    modelConfInt)

    if (exp) {
        ## exponentiate if requested
        resMat <- exp(resMat)
    }

    ## Format
    resString <- sprintf(fmt = paste0(fmt1, " [", fmt1, ", ", fmt1 ,"]"),
                         resMat[,1],
                         resMat[,2],
                         resMat[,3]
                         )

    ## Format p-values
    pString <- sprintf(fmt = fmt2,
                       modelP)
    ## Create a string like <0.001 
    smallPString <- paste0("<0.", paste0(rep("0", pDigits - 1), collapse = ""), "1")
    ## Check positions where it is all zero like 0.000
    posAllZeros <- grepl("^0\\.0*$", pString)
    ## Put the string where it is all zero like 0.000
    pString[posAllZeros] <- smallPString
    ## Put a preceding " " where it is not all zero like 0.000
    pString[!posAllZeros] <- paste0(" ", pString[!posAllZeros])

    
    ## Combine with the result column
    outMat <- cbind(resString,
                    "p" = pString)

    ## Add row names
    rownames(outMat) <- names(modelCoef)


    if (exp) {
        colnames(outMat)[1] <- "exp(beta) [confint]"
    } else if (!exp) {
        colnames(outMat)[1] <- "beta [confint]"        
    }

    if (quote) {
        rownames(outMat) <- paste0('"', rownames(outMat), '"')
        colnames(outMat) <- paste0('"', colnames(outMat), '"')                
    }

    
    print(outMat, quote = quote)

    
    return(invisible(outMat))
}
