################################################################################
### Unit tests for ShowRegTable
## Reference: http://adv-r.had.co.nz/Testing.html
## Created on: 2015-08-01
## Author: Kazuki Yoshida
################################################################################

###
### Structure
## expectations within tests within context

###
### Prepare environment
################################################################################
library(testthat)
library(survey)
library(survival)


###
### Context (1 for each file)
################################################################################
context("Unit tests for regression summary function")


###
### Independent observations
################################################################################
data(pbc)


### coxph
test_that("coxph works", {

    ## Cox
    coxph1 <- coxph(formula = Surv(time, status == 2) ~ trt + age + albumin + ascites,
                    data    = pbc)

    ## For coxph normal approximation is used
    expect_true(all(confint(coxph1) == confint.default(coxph1)))

    ## confint
    ciCoxph <- confint(coxph1)
    ShowRegTable(coxph1)
    expect_output(ShowRegTable(coxph1, digits = 5, exp = TRUE),
                  sprintf("%.5f, %.5f",
                          exp(ciCoxph[2,1]),
                          exp(ciCoxph[2,2])))

    ## contint.default
    cidCoxph <- confint.default(coxph1)
    ShowRegTable(coxph1, ciFun = confint.default)
    expect_output(ShowRegTable(coxph1, ciFun = confint.default, digits = 5, exp = TRUE),
                  sprintf("%.5f, %.5f",
                          exp(cidCoxph[2,1]),
                          exp(cidCoxph[2,2])))

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
    ciGlm1 <- confint(glm1)
    ShowRegTable(glm1, digits = 5)
    expect_output(ShowRegTable(glm1, digits = 5, exp = TRUE),
                  sprintf("%.5f, %.5f",
                          exp(ciGlm1[2,1]),
                          exp(ciGlm1[2,2])))

    ## contint.default
    cidGlm1 <- confint.default(glm1)
    ShowRegTable(glm1, ciFun = confint.default, digits = 5)
    expect_output(ShowRegTable(glm1, ciFun = confint.default, digits = 5),
                  sprintf("%.5f, %.5f",
                          exp(cidGlm1[2,1]),
                          exp(cidGlm1[2,2])))

})


### lm
test_that("lm works", {

    ## Linear
    lm1 <- lm(formula = time ~ trt + age + albumin + ascites,
              data    = pbc)

    ## For lm t-distribution based method and naive approximation differ
    expect_true(!all(confint(lm1) == confint.default(lm1)))

    ## confint
    ciLm1 <- confint(lm1)
    ShowRegTable(lm1, digits = 5, exp = FALSE)
    expect_output(ShowRegTable(lm1, digits = 5, exp = FALSE),
                  sprintf("%.5f, %.5f",
                          ciLm1[2,1],
                          ciLm1[2,2]))

    ## contint.default
    cidLm1 <- confint.default(lm1)
    ShowRegTable(lm1, ciFun = confint.default, digits = 5, exp = FALSE)
    expect_output(ShowRegTable(lm1, ciFun = confint.default, digits = 5, exp = FALSE),
                  sprintf("%.5f, %.5f",
                          cidLm1[2,1],
                          cidLm1[2,2]))

})


###
### Clustered data
################################################################################

## Ordinal Data from Koch
data(koch, package = "geepack")


### geepack
test_that("geepack works", {

    library(geepack)

    ## Log-linear GEE
    geeglm1 <- geeglm(formula   = y ~ trt + day,
                      family    = poisson(link = "log"),
                      id        = id,
                      data      = koch,
                      corstr    = "exchangeable",
                      scale.fix = FALSE)

    ## confint.default does not work for geepack
    ciGeeglm1 <- confint(geeglm1)

    ## confint
    ShowRegTable(geeglm1, digits = 5, exp = TRUE)
    expect_output(ShowRegTable(geeglm1, digits = 5, exp = TRUE),
                  sprintf("%.5f, %.5f",
                          exp(ciGeeglm1)[2,1],
                          exp(ciGeeglm1)[2,2]))

    ## coef
    expect_output(ShowRegTable(geeglm1, digits = 5, exp = TRUE),
                  sprintf("%.5f", exp(coef(geeglm1))[2]))

})


### nlme
test_that("nlme works", {

    library(nlme)

    ## Linear LME
    lme1 <- lme(fixed  = y ~ trt + day,
                data   = koch,
                random = ~ 1 | id,
                method = "REML")

    ## intervals instead of confint [lower, est, upper] format
    ShowRegTable(lme1, digits = 5, exp = FALSE)
    expect_output(ShowRegTable(lme1, digits = 5, exp = FALSE),
                  sprintf("%.5f, %.5f",
                          intervals(lme1)$fixed[2,1],
                          intervals(lme1)$fixed[2,3]))

    ## coef
    expect_output(ShowRegTable(lme1, digits = 5, exp = TRUE),
                  sprintf("%.5f", exp(intervals(lme1)$fixed[2,2])))

})


### lme4
test_that("lme4 works", {

    cat("### The lme4 version is ", installed.packages()["lme4","Version"], "\n")

    if (installed.packages()["lme4","Version"] == "1.1-14") {
        cat("### Due to the issues with lme4 1.1-14. The relevant test are skipped.\n")
    }

    ## Do not test in version 1.1-14 to avoid warnings.
    ## https://github.com/lme4/lme4/issues/440
    if (installed.packages()["lme4","Version"] != "1.1-14") {
        library(lme4)

        ## Linear LME
        lmer1 <- lmer(formula = y ~ trt + day + (1 | id),
                      data = koch)

        ciLmer1 <- tail(confint(lmer1), nrow(coef(summary(lmer1))))
        summary(lmer1)

        ## confint
        ShowRegTable(lmer1, digits = 5, exp = FALSE)
        expect_output(ShowRegTable(lmer1, digits = 5, exp = FALSE),
                      sprintf("%.5f, %.5f",
                              ciLmer1[2,1],
                              ciLmer1[2,2]))

        ## coef
        expect_output(ShowRegTable(lmer1, digits = 5, exp = FALSE),
                      sprintf("%.5f", coef(summary(lmer1))[2,1]))


        ## For p-values
        ## lmerTest::lmer() masks lme4::lmer()
        library(lmerTest)

        ## Linear LME
        lmer2 <- lmer(formula = y ~ trt + day + (1 | id),
                      data = koch)
        summary(lmer2)

        ciLmer2 <- tail(confint(lmer2), nrow(coef(summary(lmer2))))

        ## confint
        ShowRegTable(lmer2, digits = 5, exp = FALSE)
        expect_output(ShowRegTable(lmer2, digits = 5, exp = FALSE),
                      sprintf("%.5f, %.5f",
                              ciLmer2[2,1],
                              ciLmer2[2,2]))

        ## coef
        expect_output(ShowRegTable(lmer2, digits = 5, exp = FALSE),
                      sprintf("%.5f", coef(summary(lmer2))[2,1]))

        ## p-value
        ## For some reason, need to specify summary explicitly.
        expect_output(ShowRegTable(lmer2, pDigits = 5, exp = FALSE),
                      sprintf("%.5f", coef(lmerTest_summary(lmer2))[2,5]))

        ## GLMM
        glmer1 <- glmer(formula = y ~ trt + day + (1 | id),
                        data = koch,
                        family = poisson(link = "log"))
        summary(glmer1)
        ## Last rows correspond to fixed effects
        ciGlmer1 <- tail(confint(glmer1), nrow(coef(summary(glmer1))))

        ## confint
        ShowRegTable(glmer1, digits = 5, exp = TRUE)
        expect_output(ShowRegTable(glmer1, digits = 5, exp = TRUE),
                      sprintf("%.5f, %.5f",
                              exp(ciGlmer1[2,1]),
                              exp(ciGlmer1[2,2])))

        ## coef
        expect_output(ShowRegTable(glmer1, digits = 5, exp = TRUE),
                      sprintf("%.5f", exp(coef(summary(glmer1)))[2,1]))

    }
})
