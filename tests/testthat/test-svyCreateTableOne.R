################################################################################
### Unit tests for the svyCreateTableOne
## Reference: http://adv-r.had.co.nz/Testing.html
## Created on: 2015-07-24
## Author: Kazuki Yoshida
################################################################################

### Structure
## expectations within tests within context

### Prepare environment
################################################################################
library(testthat)
library(survey)


### Context (1 for each file)
################################################################################
context("Unit tests for the survey-related modules")



### Provide data
################################################################################

## Three-group dataset with matching weight (Li & Greene 2013)
datMw <- structure(list(E = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,
2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), C = c(1L,
1L, 2L, 2L, 3L, 3L, 4L, 4L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 1L,
1L, 2L, 2L, 3L, 3L, 4L, 4L), Y = c(0, 1, 0, 1, 0, 1, 0, 1, 0,
1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1), C1 = c(0, 0, 0,
0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1
), C2 = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0,
0, 1, 1, 0, 0, 1, 1), n = c(252L, 28L, 105L, 45L, 12L, 28L, 1L,
9L, 72L, 8L, 63L, 27L, 24L, 56L, 1L, 9L, 36L, 4L, 42L, 18L, 24L,
56L, 8L, 72L), Mw = c(0.1429, 0.1429, 0.4, 0.4, 1, 1, 1, 1, 0.5,
0.5, 0.6667, 0.6667, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 0.5, 0.5, 0.125,
0.125)), class = "data.frame", .Names = c("E", "C", "Y", "C1",
"C2", "n", "Mw"), row.names = c(NA, -24L))

## Create individual level data
datMw <- datMw[rep(seq_along(datMw$n), datMw$n),]
## Introduce missingness
datMw[c(1,150), "Y"] <- NA
## Create a survey design object
datSvy <- svydesign(ids = ~ 1, data = datMw, weights = ~ Mw)


### Tests
################################################################################
## A test should group together expectations for one functionality.



test_that("Data check assessment", {

    ## Should give
    ## Error in ModuleStopIfNoVarsLeft(vars) (from modules.R#52) : No valid variables.
    ## In addition: Warning message:
    ## In ModuleReturnVarsExist(vars, data$variables) :
    ##   The data frame does not have: none  Dropped
    expect_warning(expect_error(svyCreateTableOne(vars = "none", data = datSvy)))

    
})
