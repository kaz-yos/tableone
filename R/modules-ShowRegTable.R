#!/usr/local/bin/Rscript

################################################################################
### Modules to support ShowRegTable()
##
## Created on: 2016-03-19
## Author: Kazuki Yoshida
################################################################################

### geepack support

## cov.unscaled
## the unscaled (dispersion = 1) estimated covariance matrix of the estimated coefficients.
## cov.scaled (This is appropriate.)
## ditto, scaled by dispersion.
vcov.geeglm <- function(object, ...) {
    summary(object)$cov.scaled
}

confint.geeglm <- function(fit, level = 0.95) {
    coefs <- coef(fit)
    ses   <- sqrt(diag(vcov(fit)))
    q     <- qnorm(p = 1 - level, lower.tail = FALSE)

    data.frame(lower = coefs - q * ses,
               upper = coefs + q * ses)
}
