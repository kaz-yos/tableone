\name{CreateCatTable}
\alias{CreateCatTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
  Create an object summarizing categorical variables  
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Create an object summarizing categorical variables optionally
  stratifying by one or more startifying variables and performing
  statistical tests. The object gives a table that is easy to use in
  medical research papers. See also print.CatTable and summary.CatTable.
  
}
\usage{
CreateCatTable(vars, strata, data, test = TRUE, testApprox = chisq.test, testExact = fisher.test)
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
  \item{test}{
    %%     ~~Describe \code{test} here~~
    If TRUE, as in the default and there are more than two groups,
    groupwise comparisons are performed. Both tests that require the
    large sample approximation and exact tests are performed. Either one of the result can be
    obtained from the print method.    
}
  \item{testApprox}{
    %%     ~~Describe \code{testApprox} here~~
    A function used to perform the large sample approximation based
    tests. The default is chisq.test. This is not recommended when some
    of the cell have small counts like fewer than 5.
}
  \item{testExact}{
    %%     ~~Describe \code{testExact} here~~
    A function used to perform the exact tests. The default is
    fisher.test. If the cells have large numbers, it will fail because
    of memory limitation. In this situation, the large sample
    approximation based should suffice.
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
An object of class ‘CatTable’, which really is a ‘by’ object with
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
  (frequencies function).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
    print.CatTable, summary.CatTable, CreateContTable, print.ContTable, summary.ContTable
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

## Create an overall table for categorical variables
catVars <- c("status","ascites","hepato","spiders","edema","stage")
catTableOverall <- CreateCatTable(vars = catVars, data = pbc)

## Simply typing the object name will invoke the print.CatTable method,
## which will show the sample size, frequencies and percentages.
## For 2-level variables, only the higher level is shown for simplicity.
catTableOverall

## Use the showAllLevels argument to see all levels for all variables.
print(catTableOverall, showAllLevels = TRUE)

## You can choose form frequencies ("f") and/or percentages ("p") or both.
## "fp" frequency (percentage) is the default. Row names change accordingly.
print(catTableOverall, format = "f")
print(catTableOverall, format = "p")

## To further examine the variables, use the summary.CatTable method,
## which will show more details.
summary(catTableOverall)

## The table can be stratified by one or more variables
catTableBySexTrt <- CreateCatTable(vars = catVars, strata = c("sex","trt"), data = pbc)

## print now includes p-values which are by default calculated by chisq.test.
## It is formatted at the decimal place specified by the pDigits argument
## (3 by default). It does <0.001 for you.
catTableBySexTrt

## The exact argument will toggle the p-values to the example test result from
## fisher.test. It will show which ones are from exact tests.
print(catTableBySexTrt, exact = "ascites")

## summary now includes both types of p-values
summary(catTableBySexTrt)

## If your work flow includes copying to Excel and Word when writing manuscripts,
## you may benefit from the quote argument. This will quote everything so that
## Excel does not mess up the cells.
print(catTableBySexTrt, exact = "ascites", quote = TRUE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line