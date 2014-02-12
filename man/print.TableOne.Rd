\name{print.TableOne}
\alias{print.TableOne}
\title{Format and print the \code{TableOne} class objects}
\usage{
\method{print}{TableOne}(x, missing = FALSE, quote = FALSE, test = TRUE,
  pDigits = 3, format = c("fp", "f", "p", "pf")[1], exact = NULL,
  showAllLevels = FALSE, nonnormal = NULL, explain = TRUE,
  printToggle = TRUE, ...)
}
\arguments{
  \item{x}{The result of a call to the
  \code{\link{CreateTableOne}} function.}

  \item{missing}{Whether to show missing data information
  (not implemented yet, placeholder)}

  \item{quote}{Whether to show everything in quotes. The
  default is FALSE. If TRUE, everything including the row
  and column names are quoted so that you can copy it to
  Excel easily.}

  \item{test}{Whether to show the p-values. TRUE by
  default. If FALSE, only the numerical summaries are
  shown.}

  \item{pDigits}{Number of digits to print for p-values.}

  \item{format}{The default is "fp" frequency (percentage).
  You can also choose from "f" frequency only, "p"
  percentage only, and "pf" percentage (frequency).}

  \item{exact}{A character vector to specify the variables
  for which the p-values should be those of exact tests. By
  default all p-values are from large sample approximation
  tests (chisq.test).}

  \item{showAllLevels}{Whether to show all levels. FALSE by
  default, i.e., for 2-level categorical variables, only
  the higher level is shown to avoid.}

  \item{nonnormal}{A character vector to specify the
  variables for which the p-values should be those of
  nonparametric tests. By default all p-values are from
  normal assumption-based tests (oneway.test).}

  \item{explain}{Whether to add explanation to the variable
  names, i.e., (\%) is added to the variable names when
  percentage is shown.}

  \item{printToggle}{Whether to print the output. If FLASE,
  no output is created, and a matrix is invisibly
  returned.}

  \item{...}{For compatibility with generic. Ignored.}
}
\value{
It is mainly for printing the result. But this function
does return a matrix containing what you see in the output
invisibly. You can assign it to an object to save it.
}
\description{
This is the print method for the TableOne class objects
created by CreateTableOne function.
}
\examples{
## Load
library(tableone)

## Load Mayo Clinic Primary Biliary Cirrhosis Data
library(survival)
data(pbc)
## Check variables
head(pbc)
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

