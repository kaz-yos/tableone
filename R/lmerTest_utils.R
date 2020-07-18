## Minimized version of the helper function provided by lmerTest authors.
lmerTest_summary <- function(object, ...) {
    if (requireNamespace("lmerTest", quietly = TRUE)) {
        if (inherits(object, "lmerModLmerTest")) {
            ## lmerTest object
            return(summary(object, ...))
        } else {
            ## lme4 object
            return(summary(lmerTest::as_lmerModLmerTest(object), ...))
        }
        ## *merModLmerTest objects and/or 'lmerTest' is not available
        return(summary(object, ...))
    } else {
        stop("ShowRegTable: Please install the package \"lmerTest\":\n install.package('lmerTest')", call. = F)
    }
}
