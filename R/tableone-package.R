##' Create "Table 1" to describe baseline characteristics
##'
##' This package creates "Table 1", i.e., description of baseline patient characteristics, which is essential in every medical research. This package provides functions to create such summaries for continuous and categorical variables, optionally with subgroup comparisons. The package was insipired by and based on descriptive statistics functions in Deducer, a Java-based GUI package by Ian Fellows. This package does not require GUI or Java, and intended for command-line users.
##'
##' @name tableone-package
##' @aliases tableone-package tableone
##' @docType package
##' @import e1071 gmodels
##' @note Special Thanks:
##'
##' Ian Fellows for developing the Deducer package, which this package is based on.
##'
##' Hadley Wickham for packaging advice and for creating tools this package was made with (roxygen2, devtools, testthat).
##' 
##' Yoshinobu Kanda for design advice.
## and for (future) integration into \code{RcmdrPlugin.EZR}.
##'
##' Members of the Facebook Organization of R Users for Medical Statistics in Japan (FORUMS-J) for testing pre-release versions.
##'
##' Developmental repository is on github. Your contributions are appreciated.
##'
##' https://github.com/kaz-yos/tableone
##'
##' @author Kazuki Yoshida, Justin Bohn
##'
##' Maintainer: Kazuki Yoshida <kazukiyoshida@@mail.harvard.edu>
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{print.TableOne}}, \code{\link{summary.TableOne}},
##' \code{\link{CreateContTable}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}},
##' \code{\link{CreateCatTable}}, \code{\link{print.CatTable}}, \code{\link{summary.CatTable}},
##' \code{\link{ShowRegTable}}
##' @examples
##'
##' ## Load
##' library(tableone)
##'
##' ## Load Mayo Clinic Primary Biliary Cirrhosis Data
##' library(survival)
##' data(pbc)
##' ## Check variables
##' head(pbc)
##'
##' ## List numerically coded categorical variables for later conversion.
##' ## Factor variables are automatically handled as categorical variables.
##' factorVars <- c("status","trt","ascites","hepato","spiders","edema","stage")
##'
##' ## Create a variable list
##' dput(names(pbc))    # This shows a character vector-creating syntax.
##' vars <- c("time","status","age","sex","ascites","hepato",
##'           "spiders","edema","bili","chol","albumin",
##'           "copper","alk.phos","ast","trig","platelet",
##'           "protime","stage")
##'
##' ## Create Table 1 stratified by trt. Use factorVars to convert numerically
##' ## coded categorical variables as factors without changing the dataset.
##' tableOne <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc,
##'                            factorVars = factorVars)
##'
##' ## Just typing the object name will invoke the print.TableOne method
##' tableOne
##'
##' ## Specifying nonnormal variables will show the variables appropriately,
##' ## and show nonparametric test p-values. Specify variables in the exact
##' ## argument to obtain the exact test p-values. For two-level categorical
##' ## variables specified in cramVars, both levels are shown. Use minMax
##' ## argument to show median [min, max] for nonnormal variables.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), cramVars = "sex")
##'
##' ## Use the summary.TableOne method for detailed summary
##' summary(tableOne)
##'
##' ## See the categorical part only using $ operator
##' tableOne$CatTable
##' summary(tableOne$CatTable)
##'
##' ## See the continuous part only using $ operator
##' tableOne$ContTable
##' summary(tableOne$ContTable)
##'
##' ## If your work flow includes copying to Excel and Word when writing manuscripts,
##' ## you may benefit from the quote argument. This will quote everything so that
##' ## Excel does not mess up the cells.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), cramVars = "sex", quote = TRUE)
##'
NULL
