### 2014-02-09 Unifying function suggested by Justin Bohn
##' 
##' Create an object|output summarizing both categorical and continuous variables
##' 
##' Create an object|output summarizing categorical variables optionally stratifying
##' by one or more startifying variables and performing statistical tests. The
##' object gives a table that is easy to use in medical research papers.
##' 
##' @param vars Variables to be summarized given as a character vector. Factors are handled as categorical variables, whereas numeric variables are handled as continuous variables.
##' @param strata Stratifying (grouping) variable name(s) given as a character
##' vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables
##' (both vars and strata) must be in this data frame.
##' @param ... placeholder for now (FIX: common to both types of variables)
##' @return An object of class \code{TableOne}, which really is a list of two \code{\link{by}} objects with
##' additional attributes. These correspond to structures holding results for 
##' @author Justin Bohn, Kazuki Yoshida
##' @seealso \code{\link{print.TableOne}}
##' @examples
##' library(tableone)
##' library(survival)
##' data(pbc)
##' 
CreateTableOne <- function(vars, strata, data, ...) {
    
    ## get the classes of the variables
    varClasses <- sapply(data[,vars], class)
    names(varClasses[varClasses == 'factor']) -> factors
    names(varClasses[varClasses == 'numeric' | varClasses == 'integer']) -> numerics
    if(length(factors) == 0) {
        cat('NOTE: no factor variables supplied, using CreateContTable()\n')
        CreateContTable(vars = numerics, strata = strata, data = data, ...) 
    }
    if(length(numerics) == 0) {
        cat('NOTE: no numeric/integer variables supplied, using CreateCatTable()\n')
        CreateCatTable(vars = factors, strata = strata, data = data, ...) 
    }
    ## create the table for numeric variables
    cat_table <- CreateCatTable(vars = factors, strata = strata, data = data, ...)
    ## create the table for continuous variables
    cont_table <- CreateContTable(vars = numerics, strata = strata, data = data, ...)
    ## save in a list (currently has no use, but maybe it would be desirable
    ## to be able to capure the individual objects separately too)
    lst <- list(cat_table, cont_table)

### Should output this list object with class "TableOne"
    ## print method: Do the following matrix rbind()ing (realign horizontal justification)
    ## summary method: just call summary.CatTable and summary.ContTable? (or stratum by stratum?)
    
    ## first some prep (convert to matrix)
    ## is there a better way to retrieve the matrix format than below?
    ## this way works, but you can't suppress the printing
    m1 <- as.matrix(print(cat_table))
    m2 <- as.matrix(print(cont_table))
    ## rbind and delete the duplicated row
    m3 <- rbind(m1, m2[which(rownames(m2) != 'n'),])
    
    return(m3)
}

## ideas/comments (Justin):
##   1. smartly process variables based on their class
##      (i.e. do not make user specify which vars are continuous vs categorical)
##   2. figure out a good print method
##   3. if user specifies additional func.names for CreateContTable, 
##      the dimensions will be wrong and rbind will not work. Could consider
##      extra functions creating extra rows instead of extra columns.
##   4. it looks like your print method does some kind of alignment with spaces
##      that might lead to strange formatting after rbind()
