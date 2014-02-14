\docType{package}
\name{tableone-package}
\alias{tableone}
\alias{tableone-package}
\title{Create "Table 1" to describe baseline characteristics}
\description{
This package creates "Table 1", i.e., description of
baseline patient characteristics, which is essential every
medical research. This package provides functions to create
such summaries for continuous and categorical variables,
optionally with subgroups and groupwise comparison. The
package was insipired by and based on descriptive
statistics functions in Deducer, a Java-based GUI package
by Ian Fellows. This package does not require GUI or Java,
and intended for CUI users.
}
\note{
Special Thanks:

Ian Fellows for developing the Deducer package, which this
package is based on.

Hadley Wickham for packaging advice and for creating tools
this package was made with (roxygen2, devtools, testthat).

Developmental repository is on github. Your contributions
are appreciated.

https://github.com/kaz-yos/tableone
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
Kazuki Yoshida, Justin Bohn

Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
}
\seealso{
\code{\link{CreateTableOne}}, \code{\link{print.TableOne}},
\code{\link{summary.TableOne}},
\code{\link{CreateContTable}},
\code{\link{print.ContTable}},
\code{\link{summary.ContTable}},
\code{\link{CreateCatTable}}, \code{\link{print.CatTable}},
\code{\link{summary.CatTable}}, \code{\link{ShowRegTable}}
}

