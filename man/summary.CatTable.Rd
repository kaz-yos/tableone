\name{summary.CatTable}
\alias{summary.CatTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
  Shows all results in a ‘CatTable’ class object
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  This method shows all the data a CatTable class object has. This
  includes the (optionally stratified) part with summary statistics and
  p-values from the approximation method test (chisq.test by default)
  and exact method test (fisher.test by default).

}
\usage{
summary.CatTable(CatTable, digits = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CatTable}{
    %%     ~~Describe \code{CatTable} here~~
    An object that has the ‘CatTable’ class to be shown.
}
  \item{digits}{
    %%     ~~Describe \code{digits} here~~
    Number of digits to print.
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
It will print the results.
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
  
  This package was inspired by and based on the Deducer package
  (frequencies function).
  
  Developmental repository is on github. Your contributions are
  appreciated.
  
  https://github.com/kaz-yos/tableone
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  CreateCatTable, print.CatTable, CreateContTable, print.ContTable, summary.ContTable
}
\examples{
## Load
library(tableone)

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

## To further examine the variables, use the summary.CatTable method,
## which will show more details.
summary(catTableOverall)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
