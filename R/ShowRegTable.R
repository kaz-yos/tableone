ShowRegTable <- function(model, exp = TRUE, digits = 2, pDigits = 3, quote = FALSE) {

    ## Create formats
    fmt1 <- paste0("%.",  digits, "f")
    fmt2 <- paste0("%.", pDigits, "f")

    ## Obtain necessary data
    ## The model must have summary and confint methods
    modelCoef       <- coef(model)
    modelConfInt    <- confint(model)
    modelSummaryMat <- coef(summary(model))
    modelP          <- modelSummaryMat[,ncol(modelSummaryMat)]

    ## Create the result matrix with beta and two columns of confidence interval
    resMat <- cbind(beta = modelCoef,
                    modelConfInt)

    ## exponentiate if requested
    if (exp) {
        resMat <- exp(resMat)
    }

    ## Format
    pointEstimates <- sprintf(fmt = fmt1,
                              resMat[,1])
    pointEstimates <- format(pointEstimates, justify = "right")
    
    resString <- sprintf(fmt = paste0("%s", " [", fmt1, ", ", fmt1 ,"]"),
                         ## resMat[,1],    # point estimate
                         pointEstimates,
                         resMat[,2],    # lower
                         resMat[,3]     # upper
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

    
    ## Combine with the result column. (Need to be after exponentiation)
    outMat <- cbind(resString,
                    "p" = pString)

    ## Add row names
    rownames(outMat) <- names(modelCoef)

    ## Change column names depending on the exponentiation status
    if (exp) {
        colnames(outMat)[1] <- "exp(beta) [confint]"
    } else if (!exp) {
        colnames(outMat)[1] <- "beta [confint]"        
    }

    ## Add quotes if requested
    if (quote) {
        rownames(outMat) <- paste0('"', rownames(outMat), '"')
        colnames(outMat) <- paste0('"', colnames(outMat), '"')                
    }


    ## Print the result
    print(outMat, quote = quote)

    ## Invisibly return for capture as an object
    return(invisible(outMat))
}
