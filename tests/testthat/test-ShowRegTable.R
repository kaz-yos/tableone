################################################################################
### Unit tests for ShowRegTable
## Reference: http://adv-r.had.co.nz/Testing.html
## Created on: 2015-08-01
## Author: Kazuki Yoshida
################################################################################

### Structure
## expectations within tests within context

### Prepare environment
################################################################################
library(testthat)
library(survey)
library(survival)


### Context (1 for each file)
################################################################################
context("Unit tests for regression summary function")

## Load Mayo Clinic Primary Biliary Cirrhosis Data
data(pbc)


### coxph
test_that("coxph works", {

    ## Cox
    coxph1 <- coxph(formula = Surv(time, status == 2) ~ trt + age + albumin + ascites,
                    data    = pbc)

    ## For coxph normal approximation is uses
    expect_true(all(confint(coxph1) == confint.default(coxph1)))

    ## confint
    ShowRegTable(coxph1)
    expect_output(ShowRegTable(coxph1),
                  "0.72, 1.47")

    ## contint.default
    ShowRegTable(coxph1, ciFun = confint.default)
    expect_output(ShowRegTable(coxph1, ciFun = confint.default),
                  "0.72, 1.47")

})


### glm
test_that("glm works", {

    ## Logistic
    glm1 <- glm(formula = (status == 2) ~ trt + age + albumin + ascites,
                family  = binomial(link = "logit"),
                data    = pbc)

    ## For GLM profile likelihood method and naive approximation differ
    expect_true(!all(confint(glm1) == confint.default(glm1)))

    ## confint
    ShowRegTable(glm1, digits = 5)
    expect_output(ShowRegTable(glm1, digits = 5),
                  "0.63994, 1.75622")

    ## contint.default
    ShowRegTable(glm1, ciFun = confint.default, digits = 5)
    expect_output(ShowRegTable(glm1, ciFun = confint.default, digits = 5),
                  "0.63975, 1.75230")

})


### lm
test_that("lm works", {

    ## Linear
    lm1 <- lm(formula = time ~ trt + age + albumin + ascites,
              data    = pbc)

    ## For lm t-distribution based method and naive approximation differ
    expect_true(!all(confint(lm1) == confint.default(lm1)))

    ## confint
    ShowRegTable(lm1, digits = 5, exp = FALSE)
    expect_output(ShowRegTable(lm1, digits = 5, exp = FALSE),
                  "-275.96185, 175.16874")

    ## contint.default
    ShowRegTable(lm1, ciFun = confint.default, digits = 5, exp = FALSE)
    expect_output(ShowRegTable(lm1, ciFun = confint.default, digits = 5, exp = FALSE),
                  "-275.07261, 174.27950")

})
