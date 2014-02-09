\name{tableone-package}
\alias{tableone-package}
\alias{tableone}
\docType{package}
\title{
  Create the Table 1 to describe baseline characteristics
}
\description{
For some reasons, there are no good functions to create the Table 1, i.e., description of baseline characteristics in R although it is essential in every medical research. The package was insipired by descriptive statistics functions in Deducer, Java-based GUI package. This package does not require GUI or Java, and intended for CUI users.
}
\details{
\tabular{ll}{
Package: \tab tableone\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2014-02-08\cr
License: \tab GPL-2\cr
}
%%~~ An overview of how to use the package, including the most important functions ~~
  Create an object summarizing continous and categorical variables
  optionally stratifying by one or more startifying variables and
  performing statistical tests. The object gives a table that is easy to
  use in medical research papers. Continuous variables are handled by
  CreateContTable, and categorical variables are handled by CreateCatTable.

}
\author{
Kazuki Yoshida

Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>

}
\note{
  %%  ~~further notes~~
  Special Thanks:
  
  This package was inspired by and based on the Deducer package.
  
  Developmental repository is on github. Your contributions are
  appreciated.
  
  https://github.com/kaz-yos/tableone
}
\references{

}
\keyword{ package }
\seealso{
%% ~~ Optional links to other man pages, e.g. ~~
%% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
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

## Create an overall table for continuous variables
contVars <- c("time","age","bili","chol","albumin","copper","alk.phos","ast","trig","platelet","protime")
contTableOverall <- CreateContTable(vars = contVars, data = pbc)

## Simply typing the object name will invoke the print.ContTable method,
## which will show the sample size, means and standard deviations.
contTableOverall
}
