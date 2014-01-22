\name{DisplayDescTab}
\alias{DisplayDescTab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DisplayDescTab(listDescTab, missing = FALSE, digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{listDescTab}{
%%     ~~Describe \code{listDescTab} here~~
}
  \item{missing}{
%%     ~~Describe \code{missing} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function (listDescTab, missing = FALSE, digits = 2) 
{
    matDescTab <- sapply(listDescTab, FUN = function(MAT) {
        if (missing == TRUE) {
            fmt <- paste0("\%.", digits, "f [\%.", digits, "f, \%.", 
                digits, "f] (\%.", digits, "f)")
            VEC <- sprintf(fmt = fmt, MAT[, "Median"], MAT[, 
                "25th Percentile"], MAT[, "75th Percentile"], 
                1 - (MAT[, "Valid N"]/MAT[, "length"]))
        }
        else if (missing == FALSE) {
            fmt <- paste0("\%.", digits, "f [\%.", digits, "f, \%.", 
                digits, "f]")
            VEC <- sprintf(fmt = fmt, MAT[, "Median"], MAT[, 
                "25th Percentile"], MAT[, "75th Percentile"])
        }
        names(VEC) <- row.names(MAT)
        VEC
    })
    matDescTab
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
