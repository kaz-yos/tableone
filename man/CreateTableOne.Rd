\name{CreateTableOne}
\alias{CreateTableOne}
\title{Create an object summarizing both categorical and continuous variables}
\usage{
CreateTableOne(vars, strata, data, test = TRUE, testApprox = chisq.test,
  argsApprox = list(correct = TRUE), testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5), testNormal = oneway.test,
  argsNormal = list(var.equal = TRUE), testNonNormal = kruskal.test,
  argsNonNormal = list(NULL))
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
of two \code{\link{by}} objects with additional attributes.
These correspond to structures holding results for
}
\description{
Create an object summarizing categorical variables
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
## argument to obtain the exact test p-values.
print(tableOne, nonnormal = c("time"), exact = c("ascites"))

## Use the summary.TableOne method for depth summary
summary(tableOne)
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

