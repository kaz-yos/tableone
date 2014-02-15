##' Shows all results in a \code{TableOne} class object
##'
##' This method shows all the data a CatTable class object has. This includes
##' the (optionally stratified) part with summary statistics and p-values from
##' the approximation method test (chisq.test by default) and exact method test
##' (fisher.test by default).
##'
##'
##' @param object An object that has the \code{CatTable} class to be shown.
##' @param digits Number of digits to print.
##' @param ... For compatibility with generic. Ignored.
##' @return It will print the results.
##' @author Kazuki Yoshida
##' @seealso
##' \code{\link{CreateTableOne}}, \code{\link{print.TableOne}}, \code{\link{summary.TableOne}},
##' \code{\link{CreateContTable}}, \code{\link{print.ContTable}}, \code{\link{summary.ContTable}},
##' \code{\link{CreateCatTable}}, \code{\link{print.CatTable}}, \code{\link{summary.CatTable}}
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
##' ## Make categorical variables factors
##' varsToFactor <- c("status","trt","ascites","hepato","spiders","edema","stage")
##' pbc[varsToFactor] <- lapply(pbc[varsToFactor], factor)
##'
##' ## Create Table 1 stratified by sex and trt
##' tableOne <- CreateTableOne(vars = c("time","status","age","ascites","hepato",
##'                                     "spiders","edema","bili","chol","albumin",
##'                                     "copper","alk.phos","ast","trig","platelet",
##'                                     "protime","stage"),
##'                            strata = c("sex","trt"), data = pbc)
##'
##' ## Just typing the object name will invoke the print.TableOne method
##' tableOne
##'
##' ## Use the summary.TableOne method for detailed summary
##' summary(tableOne)
##'
##' @export
summary.TableOne <- function(object, digits = 1, ...) {

    ## object and ... required to be consistent with generic summary(object, ...)
    listCatContTables <- object

    ## Continuous
    cat("\n     ### Summary of continuous variables ###\n\n")    
    summary(listCatContTables$ContTable, digits = digits)    

    ## Separator
    cat("\n=======================================================================================\n")
    
    ## Categorical    
    cat("\n     ### Summary of categorical variables ### \n\n")
    summary(listCatContTables$CatTable, digits = digits)
}
