\name{ShowRegTable}
\alias{ShowRegTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ShowRegTable(model, exp = TRUE, digits = 2, pDigits = 3, quote = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{exp}{
%%     ~~Describe \code{exp} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{pDigits}{
%%     ~~Describe \code{pDigits} here~~
}
  \item{quote}{
%%     ~~Describe \code{quote} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (model, exp = TRUE, digits = 2, pDigits = 3, quote = FALSE) 
{
    fmt1 <- paste0("\%.", digits, "f")
    fmt2 <- paste0("\%.", pDigits, "f")
    modelCoef <- coef(model)
    modelConfInt <- confint(model)
    modelSummaryMat <- coef(summary(model))
    modelP <- modelSummaryMat[, ncol(modelSummaryMat)]
    resMat <- cbind(beta = modelCoef, modelConfInt)
    if (exp) {
        resMat <- exp(resMat)
    }
    resString <- sprintf(fmt = paste0(fmt1, " [", fmt1, ", ", 
        fmt1, "]"), resMat[, 1], resMat[, 2], resMat[, 3])
    pString <- sprintf(fmt = fmt2, modelP)
    smallPString <- paste0("<0.", paste0(rep("0", pDigits - 1), 
        collapse = ""), "1")
    posAllZeros <- grepl("^0\\.0*$", pString)
    pString[posAllZeros] <- smallPString
    pString[!posAllZeros] <- paste0(" ", pString[!posAllZeros])
    outMat <- cbind(resString, p = pString)
    rownames(outMat) <- names(modelCoef)
    if (exp) {
        colnames(outMat)[1] <- "exp(beta) [confint]"
    }
    else if (!exp) {
        colnames(outMat)[1] <- "beta [confint]"
    }
    if (quote) {
        rownames(outMat) <- paste0("\"", rownames(outMat), "\"")
        colnames(outMat) <- paste0("\"", colnames(outMat), "\"")
    }
    print(outMat, quote = quote)
    return(invisible(outMat))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
