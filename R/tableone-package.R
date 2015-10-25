##' Create "Table 1" to describe baseline characteristics
##'
##' Creates "Table 1", i.e., description of baseline patient characteristics, which is essential in every medical research. Supports both continuous and categorical variables, as well as p-values and standardized mean differences. Weighted data are supported via the survey package. See github for a screencast. tableone was inspired by descriptive statistics functions in Deducer , a Java-based GUI package by Ian Fellows. This package does not require GUI or Java, and intended for command-line users. Most important functions are \code{\link{CreateTableOne}} and \code{\link{svyCreateTableOne}}.
##'
##' @name tableone-package
##' @aliases tableone-package tableone
##' @docType package
##' @import survey
##' @importFrom stats as.formula chisq.test coef confint fisher.test kruskal.test median oneway.test quantile sd var xtabs
##' @importFrom utils combn
##' @note Acknowledgement:
##'
##' Ian Fellows for developing the \code{deducer} package, which this package is based on.
##'
##' Hadley Wickham for packaging advice and for creating tools this package was made with (\code{roxygen2}, \code{devtools}, \code{testthat}).
##'
##' Yoshinobu Kanda for design advice and for integration into \code{RcmdrPlugin.EZR}.
##'
##' H.Tachibana and Hiroki Matsui for inputs regarding standardized mean differences.
##'
##' jomuller, Raja Sriswan Mamidi, Atsushi Shiraishi, and Jacques Ropers for bug reports and/or feature suggestions.
##'
##' Members of the Facebook Organization of R Users for Medical Statistics in Japan (FORUMS-J) for testing pre-release versions and suggestions.
##'
##'
##' Developmental repository is on github. Your contributions are appreciated.
##'
##' \url{https://github.com/kaz-yos/tableone}
##'
##' @author Kazuki Yoshida, Justin Bohn
##'
##' Maintainer: Kazuki Yoshida <kazukiyoshida@@mail.harvard.edu>
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{svyCreateTableOne}}, \code{\link{print.TableOne}}, \code{\link{summary.TableOne}}, \code{\link{ShowRegTable}}
##' @examples
##'
##' ## See examples for CreateTableOne and svyCreateTableOne
##'
NULL
