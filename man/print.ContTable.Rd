\name{print.ContTable}
\alias{print.ContTable}
\title{Format and print the \code{ContTable} class objects}
\usage{
\method{print}{ContTable}(x, digits = 2, pDigits = 3, quote = FALSE,
  missing = FALSE, explain = TRUE, printToggle = TRUE, nonnormal = NULL,
  minMax = FALSE, test = TRUE, ...)
}
\arguments{
  \item{x}{The result of a call to the
  \code{\link{CreateContTable}} function.}

  \item{digits}{Number of digits to print in the table.}

  \item{pDigits}{Number of digits to print for p-values.}

  \item{quote}{Whether to show everything in quotes. The
  default is FALSE. If TRUE, everything including the row
  and column names are quoted so that you can copy it to
  Excel easily.}

  \item{missing}{Whether to show missing data information
  (not implemented yet, placeholder)}

  \item{explain}{Whether to add explanation to the variable
  names, i.e., (mean (sd) or median [IQR]) is added to the
  variable names.}

  \item{printToggle}{Whether to print the output. If FLASE,
  no output is created, and a matrix is invisibly
  returned.}

  \item{nonnormal}{A character vector to specify the
  variables for which the p-values should be those of
  nonparametric tests. By default all p-values are from
  normal assumption-based tests (oneway.test).}

  \item{minMax}{Whether to use [min,max] instead of
  [p25,p75] for nonnormal variables. The default is FALSE.}

  \item{test}{Whether to show the p-values. TRUE by
  default. If FALSE, only the numerical summaries are
  shown.}

  \item{...}{For compatibility with generic. Ignored.}
}
\value{
It is mainly for printing the result. But this function
does return a matrix containing what you see in the output
invisibly. You can assign it to an object to save it.
}
\description{
This is the \code{print} method for the \code{ContTable}
class objects created by \code{\link{CreateContTable}}
function.
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

## c("age","chol","copper","alk.phos","trig","protime") appear highly skewed.
## Specify them in the nonnormal argument, and the display changes to the median,
## and the [25th, 75th] percentile.
nonNormalVars <- c("age","chol","copper","alk.phos","trig","protime")
print(contTableOverall, nonnormal = nonNormalVars)

## To show median [min,max] for nonnormal variables, use minMax = TRUE
print(contTableOverall, nonnormal = nonNormalVars, minMax = TRUE)

## The table can be stratified by one or more variables
contTableBySexTrt <- CreateContTable(vars = contVars,
                                     strata = c("sex","trt"), data = pbc)

## print now includes p-values which are by default calculated by oneway.test (t-test
## equivalent in the two group case). It is formatted at the decimal place specified
## by the pDigits argument (3 by default). It does <0.001 for you.
contTableBySexTrt

## The nonnormal argument toggles the p-values to the nonparametric result from
## kruskal.test (wilcox.test equivalent for the two group case).
print(contTableBySexTrt, nonnormal = nonNormalVars)

## The minMax argument toggles whether to show median [range]
print(contTableBySexTrt, nonnormal = nonNormalVars, minMax = TRUE)

## summary now includes both types of p-values
summary(contTableBySexTrt)

## If your work flow includes copying to Excel and Word when writing manuscripts,
## you may benefit from the quote argument. This will quote everything so that
## Excel does not mess up the cells.
print(contTableBySexTrt, nonnormal = nonNormalVars, quote = TRUE)
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

