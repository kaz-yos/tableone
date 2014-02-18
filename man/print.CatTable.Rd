\name{print.CatTable}
\alias{print.CatTable}
\title{Format and print the \code{CatTable} class objects}
\usage{
\method{print}{CatTable}(x, digits = 1, pDigits = 3, quote = FALSE,
  missing = FALSE, explain = TRUE, printToggle = TRUE, format = c("fp",
  "f", "p", "pf")[1], showAllLevels = FALSE, cramVars = NULL, test = TRUE,
  exact = NULL, CrossTable = FALSE, ...)
}
\arguments{
  \item{x}{The result of a call to the
  \code{\link{CreateCatTable}} function.}

  \item{digits}{Number of digits to print in the table.}

  \item{pDigits}{Number of digits to print for p-values.}

  \item{quote}{Whether to show everything in quotes. The
  default is FALSE. If TRUE, everything including the row
  and column names are quoted so that you can copy it to
  Excel easily.}

  \item{missing}{Whether to show missing data information
  (not implemented yet, placeholder)}

  \item{explain}{Whether to add explanation to the variable
  names, i.e., (\%) is added to the variable names when
  percentage is shown.}

  \item{printToggle}{Whether to print the output. If FLASE,
  no output is created, and a matrix is invisibly
  returned.}

  \item{format}{The default is "fp" frequency (percentage).
  You can also choose from "f" frequency only, "p"
  percentage only, and "pf" percentage (frequency).}

  \item{showAllLevels}{Whether to show all levels. FALSE by
  default, i.e., for 2-level categorical variables, only
  the higher level is shown to avoid}

  \item{cramVars}{A character vector to specify the
  two-level categorical variables, for which both levels
  should be shown in one row.}

  \item{test}{Whether to show the p-values. TRUE by
  default. If FALSE, only the numerical summaries are
  shown.}

  \item{exact}{A character vector to specify the variables
  for which the p-values should be those of exact tests. By
  default all p-values are from large sample approximation
  tests (chisq.test).}

  \item{CrossTable}{Whether to show the cross table objects
  held internally using gmodels::CrossTable function. This
  will give an output similar to the PROC FREQ in SAS.}

  \item{...}{For compatibility with generic. Ignored.}
}
\value{
It is mainly for printing the result. But this function
does return a matrix containing what you see in the output
invisibly. You can assign it to an object to save it.
}
\description{
This is the \code{print} method for the \code{CatTable}
class objects created by \code{\link{CreateCatTable}}
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

## Create an overall table for categorical variables
catVars <- c("status","ascites","hepato","spiders","edema","stage")
catTableOverall <- CreateCatTable(vars = catVars, data = pbc)

## Simply typing the object name will invoke the print.CatTable method,
## which will show the sample size, frequencies and percentages.
## For 2-level variables, only the higher level is shown for simplicity.
catTableOverall

## If you need to show both levels for some 2-level factors, use cramVars
print(catTableOverall, cramVars = "hepato")

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
catTableBySexTrt <- CreateCatTable(vars = catVars,
                                   strata = c("sex","trt"), data = pbc)

## print now includes p-values which are by default calculated by chisq.test.
## It is formatted at the decimal place specified by the pDigits argument
## (3 by default). It does <0.001 for you.
catTableBySexTrt

## The exact argument toggles the p-values to the exact test result from
## fisher.test. It will show which ones are from exact tests.
print(catTableBySexTrt, exact = "ascites")

## summary now includes both types of p-values
summary(catTableBySexTrt)

## If your work flow includes copying to Excel and Word when writing manuscripts,
## you may benefit from the quote argument. This will quote everything so that
## Excel does not mess up the cells.
print(catTableBySexTrt, exact = "ascites", quote = TRUE)
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

