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
    Variable(s) to be summarized given as a character vector.
}
  \item{strata}{
    %%     ~~Describe \code{strata} here~~
    Stratifying (grouping) variable name(s) given as a character
    vector. If omitted, the overall results are returned.
}
  \item{data}{
    %%     ~~Describe \code{data} here~~
    A data frame in which these variables exist. All variables (both
    vars and strata) must be in this data frame.
}
  \item{func.names}{
    %%     ~~Describe \code{func.names} here~~
    The functions to give the group size, number with missing values,
    mean, standard deviations, median, 25th percentile, 75th percentile,
    minimum, maximum, skewness (same definition as in SAS), kurtosis
    (same definition as in SAS). All of them can be seen in the summary
    method output. The print method uses subset of these. You can choose
    subset of them or reorder them. They are all configure to omit NA
    values (na.rm = TRUE).
}
  \item{func.additional}{
    %%     ~~Describe \code{func.additional} here~~
    Additional functions can be given as a named list. For example,
    list(sum = sum). 
}
  \item{test}{
    %%     ~~Describe \code{test} here~~
    If TRUE, as in the default and there are more than two groups,
    groupwise comparisons are performed. Both tests that assume normality
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
An object of class ‘ContTable’, which really is a ‘by’ object with
additional attributes. Each element of the ‘by’ part is a matrix with
rows representing variables, and columns representing summary statistics.
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  %%  ~~who you are~~
  Kazuki YOSHIDA
}
\note{
  %%  ~~further notes~~
  Special Thanks:
  This package was inspired by and heavily based on the Deducer package
  (descriptive.table function).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  print.ContTable, summary.ContTable, CreateCatTable, print.CatTable, summary.CatTable
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## Load Mayo Clinic Primary Biliary Cirrhosis Data
library(survival)
data(pbc)
## Check variables
head(pbc)

## Create an overall table for continuous variables
contVars <- c("time","age","bili","chol","albumin","copper","alk.phos","ast","trig","platelet","protime")
contTableOverall <- CreateContTable(vars = contVars, data = pbc)

## Simply typing the object name will invoke the print.ContTable method,
## which will show the sample size, means and standard deviations.
contTableOverall

## To further examine the variables, use the summary.ContTable method,
## which will show more details.
summary(contTableOverall)

## c("age","chol","copper","alk.phos","trig","protime") appear highly skewed.
## Specify them in the nonnormal argument, and the display changes to the median,
## and the [25th, 75th] percentile.
nonNormalVars <- c("age","chol","copper","alk.phos","trig","protime")
print(contTableOverall, nonnormal = nonNormalVars)

## The table can be stratified by one or more variables
contTableBySexTrt <- CreateContTable(vars = contVars, strata = c("sex","trt"), data = pbc)

## print now includes p-values which are by default calculated by oneway.test (t-test
## equivalent in the two group case). It is formatted at the decimal place specified
## by the pDigits argument (3 by default). It does <0.001 for you.
contTableBySexTrt

## The nonnormal argument will toggle the p-values to the nonparametric result from
## kruskal.test (wilcox.test equivalent for the two group case).
print(contTableBySexTrt, nonnormal = nonNormalVars)

## summary now includes both types of p-values
summary(contTableBySexTrt)

## If your work flow includes copying to Excel and Word when writing manuscripts,
## you may benefit from the quote argument. This will quote everything so that
## Excel does not mess up the cells.
print(contTableBySexTrt, nonnormal = nonNormalVars, quote = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 } 
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
