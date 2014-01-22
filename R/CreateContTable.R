## A function to create a table for continuous variables
CreateContTable <- function(vars,       # vector of characters
                            strata,     # single element character vector
                            data,       # data frame
                            func.names = c(
                                "N","Miss",
                                "Mean","SD",
                                "Median","Q25","Q75","Min","Max",
                                "Skew","Kurt"
                                ),
                            func.additional) {

    ## Extract necessary variables
    ## dat <- eval(substitute(vars), data, parent.frame())
    dat <- data[c(vars)]

    ## if(length(dim(dat))<1.5)
    ##     dat <- d(dat)

    if(any(!sapply(dat, is.numeric))){
        ## If there is any non-numeric variables
        dat <- dat[sapply(dat, is.numeric)]
        warning("Non-numeric variables dropped")
    }
    if(missing(strata)){
        ## If there is no strata
        strata <- rep("Overall", dim(dat)[1])
    }else{
        ## Extract the stratifying variable vector
        ## strata <- eval(substitute(strata), data, parent.frame())
        strata <- data[c(strata)]
        ## if(length(dim(strata))<1.5)
        ##     strata <- d(strata)
    }

    ## Check if all the variables are continuous
    if(!all(sapply(dat, is.numeric))) stop("Can only be run on numeric variables")

    ## Create indexes for default variables
    func.indexes <- pmatch(func.names, c("N","Miss",
                                         "Mean","SD",
                                         "Median","Q25","Q75","Min","Max",
                                         "Skew","Kurt"))

    ## Create a list of default functions
    functions <- c(function(x) length(x),
                   function(x) sum(is.na(x)),
                   function(x) mean(x, na.rm = TRUE),
                   function(x) sd(x, na.rm = TRUE),
                   function(x) median(x, na.rm = TRUE),
                   function(x) quantile(x, probs = 0.25, na.rm = TRUE),
                   function(x) quantile(x, probs = 0.75, na.rm = TRUE),
                   function(x) min(x, na.rm = TRUE),
                   function(x) max(x, na.rm = TRUE),
                   function(x) skewness(x, na.rm = TRUE, type = 2),     # type 2 as in SAS and SPSS
                   function(x) kurtosis(x, na.rm = TRUE, type = 2)      # type 2 as in SAS and SPSS
                   )

    functions <- functions[func.indexes]

    if(!missing(func.additional)){
        ## When additional functions are given
        if(!is.list(func.additional) || is.null(names(func.additional)))

            stop("func.additional must be a named list of functions")
        functions  <- c(functions, unlist(func.additional))
        func.names <- c(func.names, names(func.additional))
    }

    ## calculate statistics
    tbl.list <- list()

    for(ind in 1:length(functions)){
        tbl.list[[func.names[ind]]] <- by(dat, strata,
                                          function(x) sapply(x, functions[[ind]]) , simplify = FALSE)
    }
    ## format into table
    result <- list()

    ## for(ind in 1:length(tbl.list[[1]])){
    for(ind in seq_along(tbl.list[[1]])){

        d <- dim(tbl.list[[1]])
        dn <- dimnames(tbl.list[[1]])
        dnn <- names(dn)
        ii <- ind - 1
        name <- ""
        for (j in seq_along(dn)) {
            iii <- ii%%d[j] + 1
            ii <- ii%/%d[j]
            name <- paste(name, dnn[j], ":", dn[[j]][iii], " ", sep = "")
        }
        result[[name]]<-sapply(tbl.list,function(x) x[[ind]])
    }

    ## clean out nulls
    result <- result[!sapply(result, function(x) all(sapply(x,function(x)all(is.null(x)))))]

    ## Give an S3 class
    class(result) <- "ContTable"

    ## Return
    return(result)
}
