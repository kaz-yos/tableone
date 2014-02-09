\name{summary.CatTable}
\alias{summary.CatTable}
\title{
  Shows all results in a ‘CatTable’ class object
}
\description{
  This method shows all the data a CatTable class object has. This
  includes the (optionally stratified) part with summary statistics and
  p-values from the approximation method test (chisq.test by default)
  and exact method test (fisher.test by default).

}
\usage{
summary.CatTable(CatTable, digits = 1)
}
\arguments{
  \item{CatTable}{
    An object that has the ‘CatTable’ class to be shown.
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
  (frequencies function).
  
  Developmental repository is on github. Your contributions are
  appreciated.
  
  https://github.com/kaz-yos/tableone
}


\seealso{
  CreateCatTable, print.CatTable, CreateContTable, print.ContTable,
  summary.ContTable
  
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
