#' Export TableOne Objects to Markdown
#'
#' @param x an object created by a tableone function
#' @param ... arguments passed to \code{knitr::\link{kable}()}
#'
#' @return A character vector of the table source code
#' @export
#'
#' @examples
#' 
#' library(survival)
#' data(pbc)
#' 
#' tableOne <- CreateTableOne(data = pbc)
#' 
#' kableone(tableOne)
#' 
kableone <- function(x, ...) {
  capture.output(x <- print(x))
  knitr::kable(x, ...)
}