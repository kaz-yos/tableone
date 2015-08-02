##' Format regression results in medically decent format
##'
##' It shows the regression result in the HR [95\% CI] p-value format, which is usually the form used in medical research papers.
##'
##'
##' @param model Regression model result objects that have the summary and confint methods.
##' @param exp TRUE by default. You need to specify exp = FALSE if your model is has the indentity link function (linear regression, etc).
##' @param digits Number of digits to print for the main part.
##' @param pDigits Number of digits to print for the p-values.
##' @param printToggle Whether to print the output. If FLASE, no output is created, and a matrix is invisibly returned.
##' @param quote Whether to show everything in quotes. The default is FALSE. If TRUE, everything including the row and column names are quoted so that you can copy it to Excel easily.
##' @param ciFun Function used for calculation. \code{confint} is the default. For generalized linear models this gives the profile likelihood-based calculation, which may take too much time for large models, use \code{confint.default} for simple normal approximation method (+/- 1.96 * standard error).
##' @return A matrix containing what you see is returned invisibly. You can capture it by assignment to an object.
##' @author Kazuki Yoshida
##' @examples
##'
##' ## Load
##' library(tableone)
##'
##' ## Load Mayo Clinic Primary Biliary Cirrhosis Data
##' library(survival)
##' data(pbc)
##' ## Check variables
##' head(pbc)
##'
##' ## Fit a Cox regression model
##' objCoxph <- coxph(formula = Surv(time, status == 2) ~ trt + age + albumin + ascites,
##'                   data    = pbc)
##'
##' ## Show the simple table
##' ShowRegTable(objCoxph)
##'
##' ## Show with quote to ease copy and paste
##' ShowRegTable(objCoxph, quote = TRUE)
##'
##' @export
ShowRegTable <- function(model, exp = TRUE, digits = 2, pDigits = 3, printToggle = TRUE,
                         quote = FALSE, ciFun = confint) {

    ## Create formats
    fmt1 <- paste0("%.",  digits, "f")
    fmt2 <- paste0("%.", pDigits, "f")

    ## Obtain necessary data
    ## The model must have summary and confint methods
    modelCoef       <- coef(model)
    modelConfInt    <- suppressMessages(ciFun(model))
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
                         ## point estimate
                         pointEstimates,
                         ## lower bound
                         resMat[,2],
                         ## upper bound
                         resMat[,3])

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
        colnames(outMat)[1] <- "exp(coef) [confint]"
    } else if (!exp) {
        colnames(outMat)[1] <- "coef [confint]"
    }

    ## Add quotes if requested
    if (quote) {
        rownames(outMat) <- paste0('"', rownames(outMat), '"')
        colnames(outMat) <- paste0('"', colnames(outMat), '"')
    }


    ## Print the result if asked
    if (printToggle) {
        print(outMat, quote = quote)
    }

    ## Invisibly return for capture as an object
    return(invisible(outMat))
}
