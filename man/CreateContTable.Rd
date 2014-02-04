\name{CreateContTable}
\alias{CreateContTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
  Create an object summarizing continous variables optionally
  stratifying by one or more startifying variables and performing
  statistical tests.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateContTable(vars, strata, data, func.names = c("n", "miss", "mean", "sd", "median", "q25", "q75", "min", "max", "skew", "kurt"), func.additional, nonnormal = FALSE, test = FALSE, testNormal = oneway.test, testNonNormal = kruskal.test)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{
    %%     ~~Describe \code{vars} here~~
    The variables in the data frame to be summarized given as a
    character vector.
}
  \item{strata}{
    %%     ~~Describe \code{strata} here~~
    The stratifying (grouping) variable(s) in the data frame given as a
    character vector.
}
  \item{data}{
    %%     ~~Describe \code{data} here~~
    The data frame from which these variables are taken from.
}
  \item{func.names}{
    %%     ~~Describe \code{func.names} here~~
    
}
  \item{func.additional}{
%%     ~~Describe \code{func.additional} here~~
}
  \item{nonnormal}{
%%     ~~Describe \code{nonnormal} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{testNormal}{
%%     ~~Describe \code{testNormal} here~~
}
  \item{testNonNormal}{
%%     ~~Describe \code{testNonNormal} here~~
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
function (vars, strata, data, func.names = c("n", "miss", "mean", 
    "sd", "median", "q25", "q75", "min", "max", "skew", "kurt"), 
    func.additional, nonnormal = FALSE, test = FALSE, testNormal = oneway.test, 
    testNonNormal = kruskal.test) 
{
    require(e1071)
    if (is.data.frame(data) == FALSE) {
        stop("The data argument needs to be a data frame (no quote).")
    }
    varsNotInData <- setdiff(vars, names(data))
    if (length(varsNotInData) > 0) {
        warning("The data frame does not have ", paste0(varsNotInData, 
            sep = " "), ". Dropping them.")
        vars <- intersect(vars, names(data))
    }
    if (length(vars) < 1) {
        stop("No valid variables.")
    }
    dat <- data[c(vars)]
    if (missing(strata)) {
        strata <- rep("Overall", dim(dat)[1])
        test <- FALSE
    }
    else {
        presenceOfStrata <- strata \%in\% names(data)
        strata <- strata[presenceOfStrata]
        if (length(strata) == 0) {
            stop("None of the stratifying variables are present in the data frame")
        }
        strata <- data[c(strata)]
    }
    if (any(!sapply(dat, is.numeric))) {
        dat <- dat[sapply(dat, is.numeric)]
        warning("Non-numeric variables dropped")
    }
    if (!all(sapply(dat, is.numeric))) {
        stop("Can only be run on numeric variables")
    }
    if (!is.logical(nonnormal) & !is.character(nonnormal)) {
        stop("nonnormal argument has to be FALSE/TRUE or character.")
    }
    if (is.logical(nonnormal)) {
        if (length(nonnormal) != 1) {
            stop("nonormal has to be a logical vector of length 1")
        }
        nonnormal <- rep(nonnormal, ncol(dat))
    }
    if (is.character(nonnormal)) {
        nonnormal <- vars \%in\% nonnormal
    }
    nonnormal <- as.numeric(nonnormal) + 1
    listOfTests <- list(testNormal, testNonNormal)
    listOfTests <- listOfTests[nonnormal]
    func.indexes <- pmatch(func.names, c("n", "miss", "mean", 
        "sd", "median", "q25", "q75", "min", "max", "skew", "kurt"))
    func.indexes <- func.indexes[!is.na(func.indexes)]
    tryCatch.W.E <- function(expr) {
        W <- NULL
        w.handler <- function(w) {
            W <<- w
            invokeRestart("muffleWarning")
        }
        list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), 
            warning = w.handler), warning = W)
    }
    sasSkewness <- function(x) {
        out <- tryCatch.W.E(skewness(x, na.rm = TRUE, type = 2))
        ifelse(is.numeric(out$value), out$value, NaN)
    }
    sasKurtosis <- function(x) {
        out <- tryCatch.W.E(kurtosis(x, na.rm = TRUE, type = 2))
        ifelse(is.numeric(out$value), out$value, NaN)
    }
    functions <- c(n = function(x) length(x), miss = function(x) sum(is.na(x)), 
        mean = function(x) mean(x, na.rm = TRUE), sd = function(x) sd(x, 
            na.rm = TRUE), median = function(x) median(x, na.rm = TRUE), 
        q25 = function(x) quantile(x, probs = 0.25, na.rm = TRUE), 
        q75 = function(x) quantile(x, probs = 0.75, na.rm = TRUE), 
        min = function(x) min(x, na.rm = TRUE), max = function(x) max(x, 
            na.rm = TRUE), skew = function(x) sasSkewness(x), 
        kurt = function(x) sasKurtosis(x))
    functions <- functions[func.indexes]
    if (!missing(func.additional)) {
        if (!is.list(func.additional) || is.null(names(func.additional))) {
            stop("func.additional must be a named list of functions")
        }
        functions <- c(functions, unlist(func.additional))
        func.names <- c(func.names, names(func.additional))
    }
    result <- by(data = dat, INDICES = strata, FUN = function(strataDat) {
        sapply(functions, FUN = function(fun) {
            sapply(strataDat, fun)
        })
    })
    pValues <- NULL
    if (test == TRUE) {
        resTests <- sapply(seq_len(ncol(dat)), FUN = function(i) {
            test <- listOfTests[[i]]
            var <- dat[, i]
            test(var ~ strata[[1]])
        }, simplify = FALSE)
        pValues <- sapply(resTests, getElement, "p.value")
    }
    class(result) <- c("ContTable", class(result))
    attributes(result) <- c(attributes(result), list(nonnormal = nonnormal, 
        pValues = pValues))
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
