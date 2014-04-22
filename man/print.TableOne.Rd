\name{print.TableOne}
\alias{print.TableOne}
\title{Format and print the \code{TableOne} class objects}
\usage{
\method{print}{TableOne}(x, catDigits = 1, contDigits = 2, pDigits = 3,
  quote = FALSE, missing = FALSE, explain = TRUE, printToggle = TRUE,
  test = TRUE, noSpaces = FALSE, format = c("fp", "f", "p", "pf")[1],
  showAllLevels = FALSE, cramVars = NULL, exact = NULL,
  nonnormal = NULL, minMax = FALSE, ...)
}
\arguments{
  \item{x}{The result of a call to the
  \code{\link{CreateTableOne}} function.}

  \item{catDigits}{Number of digits to print for
  proportions. Default 1.}

  \item{contDigits}{Number of digits to print for
  continuous variables. Default 2.}

  \item{pDigits}{Number of digits to print for p-values.
  Default 3.}

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

  \item{test}{Whether to show the p-values. TRUE by
  default. If FALSE, only the numerical summaries are
  shown.}

  \item{noSpaces}{Whether to remove spaces added for
  alignment. Use this option if you prefer to align numbers
  yourself in other software.}

  \item{format}{The default is "fp" frequency (percentage).
  You can also choose from "f" frequency only, "p"
  percentage only, and "pf" percentage (frequency).}

  \item{showAllLevels}{Whether to show all levels. FALSE by
  default, i.e., for 2-level categorical variables, only
  the higher level is shown to avoid redundant
  information.}

  \item{cramVars}{A character vector to specify the
  two-level categorical variables, for which both levels
  should be shown in one row.}

  \item{exact}{A character vector to specify the variables
  for which the p-values should be those of exact tests. By
  default all p-values are from large sample approximation
  tests (chisq.test).}

  \item{nonnormal}{A character vector to specify the
  variables for which the p-values should be those of
  nonparametric tests. By default all p-values are from
  normal assumption-based tests (oneway.test).}

  \item{minMax}{Whether to use [min,max] instead of
  [p25,p75] for nonnormal variables. The default is FALSE.}

  \item{...}{For compatibility with generic. Ignored.}
}
\value{
It is mainly for printing the result. But this function
does return a matrix containing what you see in the output
invisibly. You can assign it to an object to save it.
}
\description{
This is the \code{print} method for the \code{TableOne}
class objects created by \code{\link{CreateTableOne}}
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

## Make categorical variables factors
varsToFactor <- c("status","trt","ascites","hepato","spiders","edema","stage")
pbc[varsToFactor] <- lapply(pbc[varsToFactor], factor)

## Create Table 1 stratified by sex and trt
tableOne <- CreateTableOne(vars = c("time","status","age","ascites","hepato",
                                    "spiders","edema","bili","chol","albumin",
                                    "copper","alk.phos","ast","trig","platelet",
                                    "protime","stage"),
                           strata = c("sex","trt"), data = pbc)

## Just typing the object name will invoke the print.TableOne method
tableOne

## Specifying nonnormal variables will show the variables appropriately,
## and show nonparametric test p-values. Specify variables in the exact
## argument to obtain the exact test p-values. cramVars can be used to
## show both levels for a 2-level categorical variables.
print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
      exact = c("status","stage"), cramVars = "hepato")

## Use the summary.TableOne method for detailed summary
summary(tableOne)

## See the categorical part only using $ operator
tableOne$CatTable
summary(tableOne$CatTable)

## See the continuous part only using $ operator
tableOne$ContTable
summary(tableOne$ContTable)

## If your work flow includes copying to Excel and Word when writing manuscripts,
## you may benefit from the quote argument. This will quote everything so that
## Excel does not mess up the cells.
print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
      exact = c("status","stage"), cramVars = "hepato", quote = TRUE)

## If you want to center-align values in Word, use noSpaces option.
print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
      exact = c("status","stage"), cramVars = "hepato", quote = TRUE, noSpaces = TRUE)
}
\author{
Kazuki Yoshida, Justin Bohn
}
\seealso{
\code{\link{CreateTableOne}}, \code{\link{print.TableOne}},
\code{\link{summary.TableOne}},
\code{\link{CreateCatTable}}, \code{\link{print.CatTable}},
\code{\link{summary.CatTable}},
\code{\link{CreateContTable}},
\code{\link{print.ContTable}},
\code{\link{summary.ContTable}}
}

