\name{CreateTableOne}
\alias{CreateTableOne}
\title{Create an object summarizing both categorical and continuous variables}
\usage{
CreateTableOne(vars, strata, data, factorVars, test = TRUE,
  testApprox = chisq.test, argsApprox = list(correct = TRUE),
  testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
  testNormal = oneway.test, argsNormal = list(var.equal = TRUE),
  testNonNormal = kruskal.test, argsNonNormal = list(NULL))
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

  \item{factorVars}{Numerically coded variables that should
  be handled as categorical variables given as a character
  vector. If omitted, only factors are considered
  categorical variables. If all categorical variables in
  the dataset are already factors, this option is not
  necessary. The variables specified here must also be
  specified in the \code{vars} argument.}

  \item{test}{If TRUE, as in the default and there are more
  than two groups, groupwise comparisons are performed.}

  \item{testNormal}{A function used to perform the normal
  assumption based tests. The default is
  \code{\link{oneway.test}}. This is equivalent of the
  t-test when there are only two groups.}

  \item{argsNormal}{A named list of arguments passed to the
  function specified in \code{testNormal}. The default is
  \code{list(var.equal = TRUE)}, which makes it the
  ordinary ANOVA that assumes equal variance across
  groups.}

  \item{testNonNormal}{A function used to perform the
  nonparametric tests. The default is \code{kruskal.test}
  (Kruskal-Wallis Rank Sum Test). This is equivalent of the
  wilcox.test (Man-Whitney U test) when there are only two
  groups.}

  \item{argsNonNormal}{A named list of arguments passed to
  the function specified in \code{testNonNormal}. The
  default is \code{list(NULL)}, which is just a
  placeholder.}

  \item{testApprox}{A function used to perform the large
  sample approximation based tests. The default is
  \code{\link{chisq.test}}. This is not recommended when
  some of the cell have small counts like fewer than 5.}

  \item{argsApprox}{A named list of arguments passed to the
  function specified in testApprox. The default is
  \code{list(correct = TRUE)}, which turns on the
  continuity correction for \code{\link{chisq.test}}.}

  \item{testExact}{A function used to perform the exact
  tests. The default is fisher.test. If the cells have
  large numbers, it will fail because of memory limitation.
  In this situation, the large sample approximation based
  should suffice.}

  \item{argsExact}{A named list of arguments passed to the
  function specified in testExact. The default is
  \code{list(workspace = 2*10^5)}, which specifies the
  memory space allocated for \code{\link{fisher.test}}.}
}
\value{
An object of class \code{TableOne}, which really is a list
of three objects.

\item{TableOne}{a categorical-continuous mixture data
formatted and printed by the \code{\link{print.TableOne}}
method}

\item{ContTable}{an object of class \code{ContTable},
containing continuous variables only}

\item{CatTable}{ an object of class \code{CatTable},
containing categorical variables only}

The second and third objects can be then be examined with
the \code{print} and \code{summary} method, for example,
\code{summary(object$CatTable)} to examine the categorical
variables in detail.
}
\description{
Create an object summarizing all baseline variables
optionally stratifying by one or more startifying variables
and performing statistical tests. The object gives a table
that is easy to use in medical research papers. See also
\code{\link{print.TableOne}} and
\code{\link{summary.TableOne}}.
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

## Create a variable list
dput(names(pbc))
vars <- c("time","status","age","sex","ascites","hepato",
          "spiders","edema","bili","chol","albumin",
          "copper","alk.phos","ast","trig","platelet",
          "protime","stage")

## Create Table 1 stratified by trt
tableOne <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc)

## Just typing the object name will invoke the print.TableOne method
tableOne

## Specifying nonnormal variables will show the variables appropriately,
## and show nonparametric test p-values. Specify variables in the exact
## argument to obtain the exact test p-values.
print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
      exact = c("status","stage"))

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
      exact = c("status","stage"), quote = TRUE)

## If you want to center-align values in Word, use noSpaces option.
print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
      exact = c("status","stage"), quote = TRUE, noSpaces = TRUE)
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

