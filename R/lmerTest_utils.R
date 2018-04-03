
#' @importFrom utils packageVersion
#' @importFrom methods as
#' @importFrom stats anova
lmerTest_anova <- function(object, ...) {
    # Produce lmerTest-anova table for lmer-model fits (lme4 or lmerTest) with old
    # as well as new lmerTest package.
    # Standard method dispatch for all non-lmerMod objects.
    pkg_version <- "2.0-37.9005"
    if(!inherits(object, "lmerMod")) return(anova(object, ...)) # non-lmer objects
    if(requireNamespace("lmerTest", quietly=TRUE) && packageVersion("lmerTest") < pkg_version) {
        if(inherits(object, "merModLmerTest"))
            return(lmerTest::anova(object, ...))  else # lmerTest object
                return(lmerTest::anova(as(object, "merModLmerTest"), ...)) # lme4 object
    }
    if(requireNamespace("lmerTest", quietly=TRUE) && packageVersion("lmerTest") >= pkg_version) {
        if(inherits(object, "lmerModLmerTest"))
            return(anova(object, ...)) else # lmerTest object
                return(anova(lmerTest::as_lmerModLmerTest(object), ...)) # lme4 object
    }
    return(anova(object, ...)) # *merModLmerTest objects and/or 'lmerTest' is not available
}

lmerTest_summary <- function(object, ...) {
    # Produce lmerTest-summary for lmer-model fits (lme4 or lmerTest) with old
    # as well as new lmerTest package.
    # Standard method dispatch for all non-lmerMod objects.
    pkg_version <- "2.0-37.9005"
    if(!inherits(object, "lmerMod")) return(summary(object, ...)) # non-lmer objects
    if(requireNamespace("lmerTest", quietly=TRUE) && packageVersion("lmerTest") < pkg_version) {
        if(inherits(object, "merModLmerTest"))
            return(lmerTest::summary(object, ...))  else # lmerTest object
                return(lmerTest::summary(as(object, "merModLmerTest"), ...)) # lme4 object
    }
    if(requireNamespace("lmerTest", quietly=TRUE) && packageVersion("lmerTest") >= pkg_version) {
        if(inherits(object, "lmerModLmerTest"))
            return(summary(object, ...)) else # lmerTest object
                return(summary(lmerTest::as_lmerModLmerTest(object), ...)) # lme4 object
    }
    return(summary(object, ...)) # *merModLmerTest objects and/or 'lmerTest' is not available
}

is_lmerTest_class <- function(object)
    # Check if an object is of class merModLmerTest or lmerModLmerTest
    # Bridges across versions of lmerTest
    inherits(object, "merModLmerTest") || inherits(object, "lmerModLmerTest")

