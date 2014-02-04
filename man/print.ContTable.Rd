\name{print.ContTable}
\alias{print.ContTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.ContTable(ContTable, missing = FALSE, digits = 2, nonnormal = NULL, quote = TRUE, test = TRUE, pDigits = 3, explain = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ContTable}{
%%     ~~Describe \code{ContTable} here~~
}
  \item{missing}{
%%     ~~Describe \code{missing} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{nonnormal}{
%%     ~~Describe \code{nonnormal} here~~
}
  \item{quote}{
%%     ~~Describe \code{quote} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{pDigits}{
%%     ~~Describe \code{pDigits} here~~
}
  \item{explain}{
%%     ~~Describe \code{explain} here~~
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
function (ContTable, missing = FALSE, digits = 2, nonnormal = NULL, 
    quote = TRUE, test = TRUE, pDigits = 3, explain = TRUE) 
{
    varNames <- rownames(ContTable[[1]])
    nRows <- length(varNames)
    if (is.null(nonnormal)) {
        nonnormal <- attr(ContTable, "nonnormal")
    }
    else {
        if (!is.logical(nonnormal) & !is.character(nonnormal)) {
            stop("nonnormal argument has to be FALSE/TRUE or character.")
        }
        if (is.logical(nonnormal)) {
            if (length(nonnormal) != 1) {
                stop("nonormal has to be a logical vector of length 1")
            }
            nonnormal <- rep(nonnormal, nRows)
        }
        if (is.character(nonnormal)) {
            nonnormal <- vars \%in\% nonnormal
        }
        nonnormal <- as.numeric(nonnormal) + 1
        if (test == TRUE & !is.null(attr(ContTable, "pValues"))) {
            nonnormal <- attr(ContTable, "nonnormal")
            warning("The nonnormality specifications from the object are used when tests are requested.")
        }
    }
    statNames <- colnames(ContTable[[1]])
    funcDefault <- c("n", "miss", "mean", "sd", "median", "q25", 
        "q75")
    if (any(!funcDefault \%in\% statNames)) {
        summary(ContTable)
        stop("The object does not contain all necessary statistics. Use summary() method.")
    }
    ConvertNormal <- function(rowMat) {
        fmt <- paste0("\%.", digits, "f", " (\%.", digits, "f", 
            ")")
        out <- sprintf(fmt = fmt, rowMat[, "mean"], rowMat[, 
            "sd"])
        return(out)
    }
    ConvertNonNormal <- function(rowMat) {
        fmt <- paste0("\%.", digits, "f [\%.", digits, "f, \%.", 
            digits, "f]")
        out <- sprintf(fmt = fmt, rowMat[, "median"], rowMat[, 
            "q25"], rowMat[, "q75"])
        return(out)
    }
    listOfFunctions <- list(normal = ConvertNormal, nonnormal = ConvertNonNormal)
    listOfFunctions <- listOfFunctions[nonnormal]
    out <- sapply(ContTable, FUN = function(stratum) {
        if (is.null(stratum)) {
            out2 <- rep("empty", nRows)
        }
        else {
            out2 <- sapply(seq_len(nRows), FUN = function(i) {
                fun <- listOfFunctions[[i]]
                fun(stratum[i, , drop = FALSE])
            }, simplify = TRUE)
        }
        out2
    }, simplify = TRUE)
    rownames(out) <- varNames
    if (length(attr(ContTable, "dimnames")) > 1) {
        colnames(out) <- apply(expand.grid(attr(ContTable, "dimnames")), 
            MARGIN = 1, paste0, collapse = ":")
    }
    if (test == TRUE & !is.null(attr(ContTable, "pValues"))) {
        fmt <- paste0("\%.", pDigits, "f")
        p <- sprintf(fmt = fmt, attr(ContTable, "pValues"))
        out <- cbind(out, p = p)
    }
    if (explain) {
        what <- c(" (mean (sd))", " (median [IQR])")[nonnormal]
        rownames(out) <- paste0(rownames(out), what)
    }
    if (quote) {
        rownames(out) <- paste0("\"", rownames(out), "\"")
        colnames(out) <- paste0("\"", colnames(out), "\"")
    }
    if (length(ContTable) > 1) {
        cat(paste0("Stratified by ", paste0(names(attr(ContTable, 
            "dimnames")), collapse = ":"), "\n"))
    }
    print(out, quote = quote)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
