\name{summary.CatTable}
\alias{summary.CatTable}
\title{Shows all results in a \code{CatTable} class object}
\usage{
\method{summary}{CatTable}(object, digits = 1, ...)
}
\arguments{
  \item{object}{An object that has the \code{CatTable}
  class to be shown.}

  \item{digits}{Number of digits to print.}

  \item{...}{For compatibility with generic. Ignored.}
}
\value{
It will print the results.
}
\description{
This method shows all the data a \code{CatTable} class
object has. This includes the (optionally stratified) part
with summary statistics and , if available, p-values from
the approximation method test (\code{\link{chisq.test}} by
default) and exact method test (\code{\link{fisher.test}}
by default).
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
\author{
Kazuki Yoshida
}
\seealso{
\code{\link{CreateCatTable}}, \code{\link{print.CatTable}},
\code{\link{summary.CatTable}},
\code{\link{CreateContTable}},
\code{\link{print.ContTable}},
\code{\link{summary.ContTable}},
\code{\link{CreateTableOne}}, \code{\link{print.TableOne}},
\code{\link{summary.TableOne}}
}

