\name{CreateContTable}
\alias{CreateContTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
  Create an object summarizing continous variables
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Create an object summarizing continous variables optionally
  stratifying by one or more startifying variables and performing
  statistical tests. The object gives a table that is easy to use in medical research papers.
}
\usage{
CreateContTable(vars, strata, data, func.names = c("n", "miss", "mean", "sd", "median", "p25", "p75", "min", "max", "skew", "kurt"), func.additional, nonnormal = FALSE, test = FALSE, testNormal = oneway.test, testNonNormal = kruskal.test)
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
    The data frame in which these variables exist. All variables must be
    in this data frame.
}
  \item{func.names}{
    %%     ~~Describe \code{func.names} here~~
    The functions to give the group size, number with missing values,
    mean, standard deviations, median, 25th percentile, 75th percentile,
    minimum, maximum, skewness (same definition as in SAS), kurtosis
    (same definition as in SAS). All of them can be seen in the summary
    method output. The print method uses subset of these. You can choose
    subset of them or reorder them.
}
  \item{func.additional}{
    %%     ~~Describe \code{func.additional} here~~
    Additional functions can be given as a named list. For example,
    list(sum = sum). 
}
  \item{test}{
    %%     ~~Describe \code{test} here~~
    If TRUE, as in the default and there are more than two groups,
    groupwise comparison is performed. Both tests that assume normality
    and tests that do not are performed. Either one of the result can be
    obtained from the print method.
}
  \item{testNormal}{
    %%     ~~Describe \code{testNormal} here~~
    A function used to perform the normal assumption based tests. The
    default is oneway.test. This is equivalent of the t-test with
    unequal variance when there are only two groups.
}
  \item{testNonNormal}{
    %%     ~~Describe \code{testNonNormal} here~~
    A function used to perform the nonparametric tests. The default is
    kruskal.test (Kruskal-Wallis Rank Sum Test). This is equivalent of
    the  wilcox.test (Man-Whitney U test) when there are only two groups.
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
