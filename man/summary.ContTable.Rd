\name{summary.ContTable}
\alias{summary.ContTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
  Shows all results in a ‘ContTable’ class object
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  This method shows all the data a ContTable class object has. This
  includes the (optionally stratified) part with summary statistics and
  p-values from the normal assupmtion-based test (oneway.test by default)
  and nonparametric test (kruskal.test by default).
}
\usage{
summary.ContTable(ContTable, digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ContTable}{
    %%     ~~Describe \code{ContTable} here~~
    An object that has the ‘ContTable’ class to be shown.
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
  (descriptive.table function).
  
  Developmental repository is on github. Your contributions are
  appreciated.
  
  https://github.com/kaz-yos/tableone  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  CreateContTable, print.ContTable, CreateCatTable, print.CatTable, summary.CatTable
}
\examples{
## Load
library(tableone)

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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
