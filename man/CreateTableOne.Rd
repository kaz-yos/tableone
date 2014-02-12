\name{CreateTableOne}
\alias{CreateTableOne}
\title{Create an object|output summarizing both categorical and continuous variables}
\usage{
CreateTableOne(vars, strata, data, ...)
}
\arguments{
  \item{vars}{Variables to be summarized given as a
  character vector. Factors are handled as categorical
  variables, whereas numeric variables are handled as
  continuous variables.}

  \item{strata}{Stratifying (grouping) variable name(s)
  given as a character vector. If omitted, the overall
  results are returned.}

  \item{data}{A data frame in which these variables exist.
  All variables (both vars and strata) must be in this data
  frame.}

  \item{...}{placeholder for now (FIX: common to both types
  of variables)}
}
\value{
An object of class \code{TableOne}, which really is a list
of two \code{\link{by}} objects with additional attributes.
These correspond to structures holding results for
}
\description{
Create an object|output summarizing categorical variables
optionally stratifying by one or more startifying variables
and performing statistical tests. The object gives a table
that is easy to use in medical research papers.
}
\examples{
library(tableone)
library(survival)
data(pbc)
}
\author{
Justin Bohn, Kazuki Yoshida
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

