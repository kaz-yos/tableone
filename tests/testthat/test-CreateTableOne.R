################################################################################
### Unit tests for the CreateTableOne function
## Reference: http://adv-r.had.co.nz/Testing.html
## Created on: 2014-06-01
## Author: Kazuki Yoshida
################################################################################

### Structure
## expectations within tests within context

### Prepare environment
################################################################################
library(testthat)

### Context (1 for each file)
context("Unit tests for the CreateTableOne function")


### Tests for
## Tests for ModuleTestSafe, a wrapper for test functions such as oneway.test and chisq.test

## Create a dataset for a table
dat <- read.table(header = TRUE, text = "
rowVar  colVar
m       s
f       s
m       d
f       d
")

## Increase numbers to avoid "cells are too small" warnings
dat1   <- dat[rep(seq_len(nrow(dat)), 100), ]
## m's only dataset but level f is retained
dat2 <- dat1[dat1$rowVar == "m", ]
## m's only dataset without level f
dat3 <- dat2
dat3$rowVar <- factor(dat3$rowVar)
## m's only dataset without level f. Strata imbalance
dat4 <- dat3[c(1:100, seq(101,200,2)), ]


test_that("P-values are returned for appropriate 2x2 xtabs, even with an empty factor level.", {

    ## Create tables
    tab1 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat1)
    tab2 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat2)

    ## Create corresponding xtabs
    xtabs1 <- xtabs(~ rowVar + colVar, dat1)
    xtabs2 <- xtabs(~ rowVar + colVar, dat2)

    ## chisq.test
    expect_that(attributes(tab1)$pValues["rowVar","pApprox"], equals(chisq.test(xtabs1)$p.value))
    expect_that(attributes(tab2)$pValues["rowVar","pApprox"], equals(chisq.test(xtabs2)$p.value))
    ## fisher.test
    expect_that(attributes(tab1)$pValues["rowVar","pExact"],  equals(fisher.test(xtabs1)$p.value))
    expect_that(attributes(tab2)$pValues["rowVar","pExact"],  equals(fisher.test(xtabs2)$p.value))
})


test_that("P-values should be NA for 1xM xtabs", {

    ## Create a table
    tab3 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat3)
    tab4 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat4)

    ## Create corresponding xtabs
    xtabs3 <- xtabs(~ rowVar + colVar, dat3)
    xtabs4 <- xtabs(~ rowVar + colVar, dat4)

    ## chisq.test
    expect_that(attributes(tab3)$pValues["rowVar","pApprox"], equals(NA))
    expect_that(attributes(tab4)$pValues["rowVar","pApprox"], equals(NA))
    ## fisher.test
    expect_that(attributes(tab3)$pValues["rowVar","pExact"],  equals(NA))
    expect_that(attributes(tab4)$pValues["rowVar","pExact"],  equals(NA))
})



### Regression tests
################################################################################

library(survival)
data(pbc)

## Make categorical variables factors
varsToFactor <- c("status","trt","ascites","hepato","spiders","edema","stage")
pbc[varsToFactor] <- lapply(pbc[varsToFactor], factor)

## Create a variable list
vars <- c("time","status","age","sex","ascites","hepato",
          "spiders","edema","bili","chol","albumin",
          "copper","alk.phos","ast","trig","platelet",
          "protime","stage")

## Create a table to test
pbcByTrt <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc)


test_that("Regression test", {

    ## Expectations
    res1 <- structure(c("158", "2015.62 (1094.12)", "     ", "     83 (52.5) ",
"     10 ( 6.3) ", "     65 (41.1) ", "  51.42 (11.01)", "    137 (86.7) ",
"     14 (8.9) ", "     73 (46.2) ", "     45 (28.5) ", "    ",
"    132 (83.5) ", "     16 (10.1) ", "     10 ( 6.3) ", "   2.87 (3.63)",
" 365.01 (209.54)", "   3.52 (0.44)", "  97.64 (90.59)", "2021.30 (2183.44)",
" 120.21 (54.52)", " 124.14 (71.54)", " 258.75 (100.32)", "  10.65 (0.85)",
"     ", "     12 ( 7.6) ", "     35 (22.2) ", "     56 (35.4) ",
"     55 (34.8) ", "154", "1996.86 (1155.93)", "     ", "     85 (55.2) ",
"      9 ( 5.8) ", "     60 (39.0) ", "  48.58 (9.96)", "    139 (90.3) ",
"     10 (6.5) ", "     87 (56.5) ", "     45 (29.2) ", "    ",
"    131 (85.1) ", "     13 ( 8.4) ", "     10 ( 6.5) ", "   3.65 (5.28)",
" 373.88 (252.48)", "   3.52 (0.40)", "  97.65 (80.49)", "1943.01 (2101.69)",
" 124.97 (58.93)", " 125.25 (58.52)", " 265.20 (90.73)", "  10.80 (1.14)",
"     ", "      4 ( 2.6) ", "     32 (20.8) ", "     64 (41.6) ",
"     54 (35.1) ", "", " 0.883", " 0.894", "", "", "", " 0.018",
" 0.421", " 0.567", " 0.088", " 0.985", " 0.877", "", "", "",
" 0.131", " 0.748", " 0.874", " 0.999", " 0.747", " 0.460", " 0.886",
" 0.555", " 0.197", " 0.201", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", ""), .Dim = c(29L, 4L), .Dimnames = structure(list(
    c("n", "time (mean (sd))", "status (%)", "   0", "   1",
    "   2", "age (mean (sd))", "sex = f (%)", "ascites = 1 (%)",
    "hepato = 1 (%)", "spiders = 1 (%)", "edema (%)", "   0",
    "   0.5", "   1", "bili (mean (sd))", "chol (mean (sd))",
    "albumin (mean (sd))", "copper (mean (sd))", "alk.phos (mean (sd))",
    "ast (mean (sd))", "trig (mean (sd))", "platelet (mean (sd))",
    "protime (mean (sd))", "stage (%)", "   1", "   2", "   3",
    "   4"), `Stratified by trt` = c("1", "2", "p", "test")), .Names = c("",
"Stratified by trt")))
    expect_equal(print(pbcByTrt, printToggle = FALSE), res1)


    res2 <- structure(c("158", "2015.62 (1094.12)", "     ", "     83 (52.5) ",
"     10 ( 6.3) ", "     65 (41.1) ", "  51.42 (11.01)", "    137 (86.7) ",
"     14 (8.9) ", "     73 (46.2) ", "     45 (28.5) ", "    ",
"    132 (83.5) ", "     16 (10.1) ", "     10 ( 6.3) ", "   1.40 [0.80, 3.20]",
" 315.50 [247.75, 417.00]", "   3.52 (0.44)", "  73.00 [40.00, 121.00]",
"1214.50 [840.75, 2028.00]", " 120.21 (54.52)", " 106.00 [84.50, 146.00]",
" 258.75 (100.32)", "  10.65 (0.85)", "     ", "     12 ( 7.6) ",
"     35 (22.2) ", "     56 (35.4) ", "     55 (34.8) ", "154",
"1996.86 (1155.93)", "     ", "     85 (55.2) ", "      9 ( 5.8) ",
"     60 (39.0) ", "  48.58 (9.96)", "    139 (90.3) ", "     10 (6.5) ",
"     87 (56.5) ", "     45 (29.2) ", "    ", "    131 (85.1) ",
"     13 ( 8.4) ", "     10 ( 6.5) ", "   1.30 [0.72, 3.60]",
" 303.50 [254.25, 377.00]", "   3.52 (0.40)", "  73.00 [43.00, 139.00]",
"1283.00 [922.50, 1949.75]", " 124.97 (58.93)", " 113.00 [84.50, 155.00]",
" 265.20 (90.73)", "  10.80 (1.14)", "     ", "      4 ( 2.6) ",
"     32 (20.8) ", "     64 (41.6) ", "     54 (35.1) ", "",
" 0.883", " 0.884", "", "", "", " 0.018", " 0.421", " 0.567",
" 0.088", " 0.985", " 0.877", "", "", "", " 0.842", " 0.544",
" 0.874", " 0.717", " 0.812", " 0.460", " 0.370", " 0.555", " 0.197",
" 0.205", "", "", "", "", "", "", "exact", "", "", "", "", "",
"", "", "", "", "", "", "", "nonnorm", "nonnorm", "", "nonnorm",
"nonnorm", "", "nonnorm", "", "", "exact", "", "", "", ""), .Dim = c(29L,
4L), .Dimnames = structure(list(c("n", "time (mean (sd))", "status (%)",
"   0", "   1", "   2", "age (mean (sd))", "sex = f (%)", "ascites = 1 (%)",
"hepato = 1 (%)", "spiders = 1 (%)", "edema (%)", "   0", "   0.5",
"   1", "bili (median [IQR])", "chol (median [IQR])", "albumin (mean (sd))",
"copper (median [IQR])", "alk.phos (median [IQR])", "ast (mean (sd))",
"trig (median [IQR])", "platelet (mean (sd))", "protime (mean (sd))",
"stage (%)", "   1", "   2", "   3", "   4"), `Stratified by trt` = c("1",
"2", "p", "test")), .Names = c("", "Stratified by trt")))
    expect_equal(print(pbcByTrt, nonnormal = c("bili","chol","copper","alk.phos","trig"),
                       exact = c("status","stage"), printToggle = FALSE), res2)


    res3 <- structure(c("158", "2015.62 (1094.12)", "0.89 (0.96)", "51.42 (11.01)",
"137 (86.7)", "0.09 (0.29)", "0.46 (0.50)", "0.28 (0.45)", "0.11 (0.28)",
"1.40 [0.80, 3.20]", "315.50 [247.75, 417.00]", "3.52 (0.44)",
"73.00 [40.00, 121.00]", "1214.50 [840.75, 2028.00]", "120.21 (54.52)",
"106.00 [84.50, 146.00]", "258.75 (100.32)", "10.65 (0.85)",
"2.97 (0.94)", "154", "1996.86 (1155.93)", "0.84 (0.96)", "48.58 (9.96)",
"139 (90.3)", "0.06 (0.25)", "0.56 (0.50)", "0.29 (0.46)", "0.11 (0.27)",
"1.30 [0.72, 3.60]", "303.50 [254.25, 377.00]", "3.52 (0.40)",
"73.00 [43.00, 139.00]", "1283.00 [922.50, 1949.75]", "124.97 (58.93)",
"113.00 [84.50, 155.00]", "265.20 (90.73)", "10.80 (1.14)", "3.09 (0.81)",
"", "0.883", "0.657", "0.018", "0.421", "0.434", "0.069", "0.886",
"0.828", "0.842", "0.544", "0.874", "0.717", "0.812", "0.460",
"0.370", "0.555", "0.197", "0.243", "", "", "", "", "", "", "",
"", "", "nonnorm", "nonnorm", "", "nonnorm", "nonnorm", "", "nonnorm",
"", "", ""), .Dim = c(19L, 4L), .Dimnames = structure(list(c("n",
"time (mean (sd))", "status (mean (sd))", "age (mean (sd))",
"sex = f (%)", "ascites (mean (sd))", "hepato (mean (sd))", "spiders (mean (sd))",
"edema (mean (sd))", "bili (median [IQR])", "chol (median [IQR])",
"albumin (mean (sd))", "copper (median [IQR])", "alk.phos (median [IQR])",
"ast (mean (sd))", "trig (median [IQR])", "platelet (mean (sd))",
"protime (mean (sd))", "stage (mean (sd))"), `Stratified by trt` = c("1",
"2", "p", "test")), .Names = c("", "Stratified by trt")))
    out3 <- print(pbcByTrt, nonnormal = c("bili","chol","copper","alk.phos","trig"),
                       exact = c("status","stage"), noSpaces = TRUE, printToggle = FALSE)
    expect_equal(out3, res3)


    res4 <- structure(c("", "", "", "", "m", "f", "", "", "", "", "", "", 
"", "", "", "", "", "", "", "", "158", "2015.62 (1094.12)", "   0.89 (0.96)", 
"  51.42 (11.01)", "     21 (13.3) ", "    137 (86.7) ", "   0.09 (0.29)", 
"   0.46 (0.50)", "   0.28 (0.45)", "   0.11 (0.28)", "   1.40 [0.80, 3.20]", 
" 315.50 [247.75, 417.00]", "   3.52 (0.44)", "  73.00 [40.00, 121.00]", 
"1214.50 [840.75, 2028.00]", " 120.21 (54.52)", " 106.00 [84.50, 146.00]", 
" 258.75 (100.32)", "  10.65 (0.85)", "   2.97 (0.94)", "154", 
"1996.86 (1155.93)", "   0.84 (0.96)", "  48.58 (9.96)", "     15 ( 9.7) ", 
"    139 (90.3) ", "   0.06 (0.25)", "   0.56 (0.50)", "   0.29 (0.46)", 
"   0.11 (0.27)", "   1.30 [0.72, 3.60]", " 303.50 [254.25, 377.00]", 
"   3.52 (0.40)", "  73.00 [43.00, 139.00]", "1283.00 [922.50, 1949.75]", 
" 124.97 (58.93)", " 113.00 [84.50, 155.00]", " 265.20 (90.73)", 
"  10.80 (1.14)", "   3.09 (0.81)", "", " 0.883", " 0.657", " 0.018", 
" 0.421", "", " 0.434", " 0.069", " 0.886", " 0.828", " 0.842", 
" 0.544", " 0.874", " 0.717", " 0.812", " 0.460", " 0.370", " 0.555", 
" 0.197", " 0.243", "", "", "", "", "", "", "", "", "", "", "nonnorm", 
"nonnorm", "", "nonnorm", "nonnorm", "", "nonnorm", "", "", ""
), .Dim = c(20L, 5L), .Dimnames = structure(list(c("n", "time (mean (sd))", 
"status (mean (sd))", "age (mean (sd))", "sex (%)", "", "ascites (mean (sd))", 
"hepato (mean (sd))", "spiders (mean (sd))", "edema (mean (sd))", 
"bili (median [IQR])", "chol (median [IQR])", "albumin (mean (sd))", 
"copper (median [IQR])", "alk.phos (median [IQR])", "ast (mean (sd))", 
"trig (median [IQR])", "platelet (mean (sd))", "protime (mean (sd))", 
"stage (mean (sd))"), `Stratified by trt` = c("level", "1", "2", 
"p", "test")), .Names = c("", "Stratified by trt")))
    expect_equal(print(pbcByTrt, nonnormal = c("bili","chol","copper","alk.phos","trig"),
                       exact = c("status","stage"), showAllLevels = TRUE, printToggle = FALSE), res4)
})
