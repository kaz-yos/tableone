################################################################################
### Unit tests for the modules
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


test_that("indicator functions work correctly", {

    ## one for counting
    expect_equal(one(c(NA,NA,NA)), c(1,1,1))
    expect_equal(one(c(1,2,3)), c(1,1,1))

    ## missing value indicator
    expect_equal(miss(c(NA,NA,NA)), c(1,1,1))
    expect_equal(miss(c(1,2,3)), c(0,0,0))
    expect_equal(miss(c(1,NA,3)), c(0,1,0))
})


test_that("variable names are transformed to correct formula", {

    ## Expectations
    expect_equal(FormulaString(c("a","b","c")), " ~ a + b + c")
    expect_equal(FormulaString("a"), " ~ a")
})


test_that("post-weighting dataset has 150 x 3 weighted subjects", {

    ## Expectations
    expect_equal(as.integer(round(svyN(vars = "E", datSvy))), 450)
    expect_equal(as.numeric(svyN(vars = "E", datSvy)),
                 as.numeric(svytotal( ~ one(E), datSvy)))

    expect_equal(as.integer(round(svyN(vars = "Y", datSvy))), 450)
    expect_equal(as.numeric(svyN(vars = "Y", datSvy)),
                 as.numeric(svytotal( ~ one(Y), datSvy)))

    expect_equal(as.integer(round(svyN(vars = "E", subset(datSvy, E == 1)))), 150)
    expect_equal(as.integer(round(svyN(vars = "E", subset(datSvy, E == 2)))), 150)
    expect_equal(as.integer(round(svyN(vars = "E", subset(datSvy, E == 3)))), 150)

    expect_equal(as.numeric(svyN(vars = "E", subset(datSvy, E == 1))),
                 as.numeric(svytotal( ~ one(E), subset(datSvy, E == 1))))
    expect_equal(as.numeric(svyN(vars = "E", subset(datSvy, E == 2))),
                 as.numeric(svytotal( ~ one(E), subset(datSvy, E == 2))))
    expect_equal(as.numeric(svyN(vars = "E", subset(datSvy, E == 3))),
                 as.numeric(svytotal( ~ one(E), subset(datSvy, E == 3))))

    expect_equal(as.integer(round(svyN(vars = "E", subset(datSvy, E == 1)))), 150)
    expect_equal(as.integer(round(svyN(vars = "E", subset(datSvy, E == 2)))), 150)
    expect_equal(as.integer(round(svyN(vars = "E", subset(datSvy, E == 3)))), 150)

    expect_equal(as.integer(round(svyN(vars = c("E","Y"), subset(datSvy, E == 3)))), c(150,150))
})


test_that("NA counting works", {

    ## Expectations
    expect_true(svyMiss(vars = "E", datSvy) == 0)
    ## Two observations with a weight of 0.1429 each
    expect_true(svyMiss(vars = "Y", datSvy) == 0.2858)
    ## Can work on multiple variables at a time
    expect_true(all(svyMiss(vars = c("E","C1"), datSvy) == c(0, 0)))
    expect_true(all(svyMiss(vars = c("E","C1","Y"), datSvy) == c(0, 0, 0.2858)))
})


test_that("Means are correct", {

    ## Expectations
    expect_equal(as.numeric(svyMean(vars = "E", datSvy)),
                 as.numeric(svymean( ~ E, datSvy)))

    ## NA's are always removed
    expect_equal(as.numeric(svyMean(vars = "Y", datSvy)),
                 as.numeric(svymean( ~ Y, datSvy, na.rm = TRUE)))

    expect_equal(as.numeric(svyMean(vars = "Y", subset(datSvy, E == 1))),
                 as.numeric(svymean( ~ Y, subset(datSvy, E == 1), na.rm = TRUE)))
})


test_that("SDs are correct", {

    ## Expectations
    expect_equal(as.numeric(svySd(vars = "E", datSvy)),
                 as.numeric(sqrt(svyvar( ~ E, datSvy))))

    ## NA's are always removed
    expect_equal(as.numeric(svySd(vars = "Y", datSvy)),
                 as.numeric(sqrt(svyvar( ~ Y, datSvy, na.rm = TRUE))))

    ## Take diagnoal of matrix for multiple variables
    expect_equal(as.numeric(svySd(vars = c("C1","C2"), datSvy)),
                 as.numeric(sqrt(diag(svyvar( ~ C1 + C2, datSvy, na.rm = TRUE)))))
})


test_that("Quantiles are correct", {

    ## Expectations
    expect_equal(as.numeric(svyQuant(vars = "E", datSvy, q = 0.5)),
                 as.numeric(svyquantile( ~ E, datSvy, quantiles = 0.5)))

    expect_equal(as.numeric(svyQuant(vars = "E", datSvy, q = 1)),
                 as.numeric(svyquantile( ~ E, datSvy, quantiles = 1)))

    expect_equal(as.numeric(svyQuant(vars = "Y", datSvy, q = 0)),
                 as.numeric(svyquantile( ~ Y, datSvy, quantiles = 0, na.rm = TRUE)))

    ## Only first of q is used
    expect_equal(as.numeric(svyQuant(vars = "E", datSvy, q = c(0.5, 0, 1))),
                 as.numeric(svyquantile( ~ E, datSvy, quantiles = 0.5)))
})


test_that("Regression test for svyContSummary", {

    res1 <- structure(c(450.015, 450.015, 0, 0.2858, 0, 0.0635089941446396,
                        2.00060881081326, 0.39357484459537, 0.816777838825025, 0.488786864487986,
                        2, 0, 1, 0, 3, 1, 1, 0, 3, 1), .Dim = c(2L, 10L), .Dimnames = list(
    c("E", "Y"), c("n", "miss", "p.miss", "mean", "sd", "median",
    "p25", "p75", "min", "max")))
    expect_equal(svyContSummary(vars = c("E","Y"), datSvy), res1)
})


test_that("table works", {

    ## Expectations
    expect_true(all(svyTable("E", datSvy) == svytable( ~ E, datSvy)))
    expect_true(all(svyTable("Y", datSvy) == svytable( ~ Y, datSvy)))
})


test_that("Levels are obtained", {

    ## Expectations
    expect_equal(svyLevel("E", datSvy),  c("1","2","3"))
    expect_equal(svyLevel("Y", datSvy),  c("0","1"))
})


test_that("Prop table works", {

    ## Expectations
    expect_equal(as.numeric(round(svyPropTable("E", datSvy), 3)),
                 round(c(1/3,1/3,1/3), 3))
    expect_equal(as.numeric(svyPropTable("Y", datSvy)),
                 as.numeric(prop.table(svytable( ~ Y, datSvy))))
})


test_that("Regression test for one variable categorical summary", {

    ## Expectations
    res1 <- structure(list(n = c(450.015, 450.015), miss = c(0.2858, 0.2858
), p.miss = c(0.0635089941446396, 0.0635089941446396), level = structure(1:2, .Label = c("0",
"1"), class = "factor"), freq = c(272.7271, 177.0021), percent = c(60.642515540463,
39.357484459537), cum.percent = c(60.642515540463, 100)), .Names = c("n",
"miss", "p.miss", "level", "freq", "percent", "cum.percent"), row.names = c(NA,
-2L), class = "data.frame")
    expect_equal(svyCatSummaryForOneVar("Y", datSvy), res1)

    res2 <- structure(list(n = c(450.015, 450.015, 450.015), miss = c(0,
0, 0), p.miss = c(0, 0, 0), level = structure(1:3, .Label = c("1",
"2", "3"), class = "factor"), freq = c(150.012, 150.003, 150),
    percent = c(33.3348888370388, 33.3328889037032, 33.332222259258
    ), cum.percent = c(33.3348888370388, 66.667777740742, 100
    )), .Names = c("n", "miss", "p.miss", "level", "freq", "percent",
"cum.percent"), row.names = c(NA, -3L), class = "data.frame")
    expect_equal(svyCatSummaryForOneVar("E", datSvy), res2)
})


test_that("Regression test for multiple variable categorical summary", {

    ## Expectations

    res1 <- structure(list(E = structure(list(n = c(450.015, 450.015, 450.015
), miss = c(0, 0, 0), p.miss = c(0, 0, 0), level = structure(1:3, .Label = c("1",
"2", "3"), class = "factor"), freq = c(150.012, 150.003, 150),
    percent = c(33.3348888370388, 33.3328889037032, 33.332222259258
    ), cum.percent = c(33.3348888370388, 66.667777740742, 100
    )), .Names = c("n", "miss", "p.miss", "level", "freq", "percent",
"cum.percent"), row.names = c(NA, -3L), class = "data.frame"),
    Y = structure(list(n = c(450.015, 450.015), miss = c(0.2858,
    0.2858), p.miss = c(0.0635089941446396, 0.0635089941446396
    ), level = structure(1:2, .Label = c("0", "1"), class = "factor"),
        freq = c(272.7271, 177.0021), percent = c(60.642515540463,
        39.357484459537), cum.percent = c(60.642515540463, 100
        )), .Names = c("n", "miss", "p.miss", "level", "freq",
    "percent", "cum.percent"), row.names = c(NA, -2L), class = "data.frame"),
    C1 = structure(list(n = c(450.015, 450.015), miss = c(0,
    0), p.miss = c(0, 0), level = structure(1:2, .Label = c("0",
    "1"), class = "factor"), freq = c(300.015, 150), percent = c(66.667777740742,
    33.332222259258), cum.percent = c(66.667777740742, 100)), .Names = c("n",
    "miss", "p.miss", "level", "freq", "percent", "cum.percent"
    ), row.names = c(NA, -2L), class = "data.frame")), .Names = c("E",
                                                                  "Y", "C1"))
    expect_equal(svyCatSummary(c("E","Y","C1"), datSvy), res1)
})


### Statistical test functions

test_that("Statistical test wrappers work", {

    ## Expectations
    ## ANOVA equivalent
    expect_true(regTermTest(svyglm(Y ~ E, datSvy), test.terms = "E")$p[1,1] ==  svyTestNormal("Y ~ E", design = datSvy, test.terms = "E", method = "Wald"))

    ## Kruskal-Wallis test equivalent (the whole htest object match)
    expect_equal(svyranktest(Y ~ E, datSvy), svyTestNonNormal("Y ~ E", design = datSvy))

    ## Chi-squared test equivalent (the whole thing had partial mismatch)
    expect_equal(svychisq(~ C1 + E, datSvy)$p.value, svyTestChisq("~ C1 + E", design = datSvy)$p.value)

})
