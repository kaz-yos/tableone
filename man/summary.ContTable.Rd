\name{summary.ContTable}
\alias{summary.ContTable}
\title{
  Shows all results in a ‘ContTable’ class object
}
\description{
  This method shows all the data a ContTable class object has. This
  includes the (optionally stratified) part with summary statistics and
  p-values from the normal assupmtion-based test (oneway.test by default)
  and nonparametric test (kruskal.test by default).
}
\usage{
summary.ContTable(ContTable, digits = 2)
}
\arguments{
  \item{ContTable}{
    An object that has the ‘ContTable’ class to be shown.
}
  \item{digits}{
    Number of digits to print.
}
}
\details{
}
\value{
It will print the results.
}
\references{
}
\author{
  Kazuki YOSHIDA
}
\note{
  Special Thanks:
  
  This package was inspired by and based on the Deducer package
  (descriptive.table function).
  
  Developmental repository is on github. Your contributions are
  appreciated.
  
  https://github.com/kaz-yos/tableone  
}


\seealso{
  CreateContTable, print.ContTable, CreateCatTable, print.CatTable,
  summary.CatTable  
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
contVars <- c("time","age","bili","chol","albumin","copper",
              "alk.phos","ast","trig","platelet","protime")
contTableOverall <- CreateContTable(vars = contVars, data = pbc)

## Simply typing the object name will invoke the print.ContTable method,
## which will show the sample size, means and standard deviations.
contTableOverall

## To further examine the variables, use the summary.ContTable method,
## which will show more details.
summary(contTableOverall)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
