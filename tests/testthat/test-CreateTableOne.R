################################################################################
### Unit tests for the CreateTableOne function
## Reference: http://adv-r.had.co.nz/Testing.html
## Created on: 2014-06-01
## Author: Kazuki Yoshida
################################################################################

###
### Prepare environment
################################################################################
library(testthat)


expect_known_output <- function(...) {
    testthat::expect_known_output(..., width = 120)
}


###
### Context (1 for each file)
################################################################################
context("Unit tests for the CreateTableOne function")


###
### Load data
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
varsContOnly <- c("time","age","protime")
varsCatOnly  <- c("status","trt","sex")


###
### Tests for data checkers
################################################################################

test_that("abnormal data are correctly detected", {

    ## Expectations
    ## Use regular expressions

    ## Error in ModuleStopIfNoVarsLeft(vars) : No valid variables.
    ## In addition: Warning message:
    ## In ModuleReturnVarsExist(vars, data) :
    ##   The data frame does not have: non-existent  Dropped
    expect_error(expect_warning(CreateTableOne(vars = "non-existent", data = pbc),
                                "The data frame does not have: non-existent  Dropped"),
                 "No valid variables.")

    ## Error in ModuleStopIfNotDataFrame(data) :
    ##   The data argument needs to be a data frame (no quote).
    expect_error(CreateTableOne(data = "not a data frame"),
                 "The data argument needs to be a data frame")

    ## Error in ModuleReturnStrata(strata, data) :
    ##   None of the stratifying variables are present in the data frame.
    ## In addition: Warning message:
    ## In ModuleReturnVarsExist(strata, data) :
    ##   The data frame does not have: non-existent  Dropped
    expect_error(expect_warning(CreateTableOne(vars = vars, strata = c("non-existent"), data = pbc),
                                "The data frame does not have: non-existent  Dropped"),
                 "None of the stratifying variables are present in the data frame")

})


###
### Tests for ModuleTestSafe, a wrapper for test functions such as oneway.test and chisq.test
################################################################################

## Create a dataset for a table
dat <- read.table(header = TRUE, text = "
rowVar  colVar
m       s
f       s
m       d
f       d
", stringsAsFactors = TRUE)

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
    expect_that(attributes(tab1$CatTable)$pValues["rowVar","pApprox"],
                equals(chisq.test(xtabs1)$p.value))
    expect_warning(expect_that(attributes(tab2$CatTable)$pValues["rowVar","pApprox"],
                               equals(chisq.test(xtabs2)$p.value)),
                   "Chi-squared approximation may be incorrect")
    ## fisher.test
    expect_that(attributes(tab1$CatTable)$pValues["rowVar","pExact"],
                equals(fisher.test(xtabs1)$p.value))
    expect_that(attributes(tab2$CatTable)$pValues["rowVar","pExact"],
                equals(fisher.test(xtabs2)$p.value))
})


test_that("P-values should be NA for 1xM xtabs", {

    ## Create a table
    tab3 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat3)
    tab4 <- CreateTableOne(vars = "rowVar", strata = "colVar", data = dat4)

    ## Create corresponding xtabs
    xtabs3 <- xtabs(~ rowVar + colVar, dat3)
    xtabs4 <- xtabs(~ rowVar + colVar, dat4)

    ## chisq.test
    expect_that(attributes(tab3$CatTable)$pValues["rowVar","pApprox"],
                equals(NA))
    expect_that(attributes(tab4$CatTable)$pValues["rowVar","pApprox"],
                equals(NA))
    ## fisher.test
    expect_that(attributes(tab3$CatTable)$pValues["rowVar","pExact"],
                equals(NA))
    expect_that(attributes(tab4$CatTable)$pValues["rowVar","pExact"],
                equals(NA))
})


###
### Table construction and printing tests
################################################################################

## Create a table to test
pbcOverall  <- CreateTableOne(vars = vars, data = pbc)
pbcInclNa   <- CreateTableOne(vars = vars, data = pbc, includeNA = TRUE)
pbcByTrt    <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc)
pbcByTrtSex <- CreateTableOne(vars = vars, strata = c("trt","sex"), data = pbc)
pbcContOnlyByTrtSex <- CreateTableOne(vars = varsContOnly, strata = c("trt","sex"), data = pbc)
pbcCatOnlyByTrtSex  <- CreateTableOne(vars = varsCatOnly, strata = c("trt","sex"), data = pbc)
pbcByTrt_addOverall    <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc, addOverall = TRUE)
pbcByTrtSex_addOverall <- CreateTableOne(vars = vars, strata = c("trt","sex"), data = pbc, addOverall = TRUE)
pbcContOnlyByTrtSex_addOverall <- CreateTableOne(vars = varsContOnly, strata = c("trt","sex"), data = pbc, addOverall = TRUE)
pbcCatOnlyByTrtSex_addOverall  <- CreateTableOne(vars = varsCatOnly, strata = c("trt","sex"), data = pbc, addOverall = TRUE)



## Specify variables for special handling
nonnormalVars <- c("bili","chol","copper","alk.phos","trig")
exactVars <- c("status","stage")


test_that("TableOne objects are always returned", {

    expect_equal(class(pbcOverall),          "TableOne")
    expect_equal(class(pbcInclNa),           "TableOne")
    expect_equal(class(pbcByTrt),            "TableOne")
    expect_equal(class(pbcByTrtSex),         "TableOne")
    expect_equal(class(pbcContOnlyByTrtSex), "TableOne")
    expect_equal(class(pbcCatOnlyByTrtSex),  "TableOne")
    ## Extra-Tests for addOverall Option
    expect_equal(class(pbcByTrt_addOverall),            "TableOne")
    expect_equal(class(pbcByTrtSex_addOverall),         "TableOne")
    expect_equal(class(pbcContOnlyByTrtSex_addOverall), "TableOne")
    expect_equal(class(pbcCatOnlyByTrtSex_addOverall),  "TableOne")

})


test_that("Missing percentages are correctly stored and printed", {

    ## Extract from dataset
    percentMissing <- unlist(lapply(pbc[vars], function(x) {sum(is.na(x)) / length(x) * 100}))
    ## Sanity check for the standard.
    expect_equal(length(percentMissing), length(vars))

    ## Unstratified table
    expect_equal(pbcOverall$MetaData$percentMissing, percentMissing)
    ## Including NA as a category should not matter.
    expect_equal(pbcInclNa$MetaData$percentMissing, percentMissing)
    ## Stratification should not matter
    expect_equal(pbcByTrt$MetaData$percentMissing, percentMissing)
    expect_equal(pbcByTrtSex$MetaData$percentMissing, percentMissing)
    ## addOverall should not matter
    expect_equal(pbcByTrt_addOverall$MetaData$percentMissing, percentMissing)
    expect_equal(pbcByTrtSex_addOverall$MetaData$percentMissing, percentMissing)

    ## Check printing
    ## Gold standard
    percentMissingString <- format(sprintf("%.1f", percentMissing), justify = "right")
    ## Function to drop empty elements like "" or "   ".
    DropEmptyString <- function(x) {
        ## as.character() drops names.
        as.character(Filter(f = function(elt) {
            !grepl("^ *$", elt)
        },
        x = x))
    }
    ## Check against gold standard
    expect_equal(DropEmptyString(print(pbcOverall, missing = TRUE)[,"Missing"]),
                 percentMissingString)
    expect_equal(DropEmptyString(print(pbcInclNa, missing = TRUE)[,"Missing"]),
                 percentMissingString)
    expect_equal(DropEmptyString(print(pbcByTrt, missing = TRUE)[,"Missing"]),
                 percentMissingString)
    expect_equal(DropEmptyString(print(pbcByTrtSex, missing = TRUE)[,"Missing"]),
                 percentMissingString)
    expect_equal(DropEmptyString(print(pbcByTrt_addOverall, missing = TRUE)[,"Missing"]),
                 percentMissingString)
    expect_equal(DropEmptyString(print(pbcByTrtSex_addOverall, missing = TRUE)[,"Missing"]),
                 percentMissingString)

    ## Regression test for missing column
    ## This corner case breaks alignment.
    data(pbc)
    vars <- names(pbc)[-1]
    ## Create Table 1 stratified by trt (can add more stratifying variables)
    tableOne <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc,
                               factorVars = c("status","edema","stage"))
    ## Specifying nonnormal variables will show the variables appropriately,
    ## and show nonparametric test p-values. Specify variables in the exact
    ## argument to obtain the exact test p-values.
    expect_equal_to_reference(print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
                                    exact = c("status","stage"), test = FALSE, smd = TRUE, missing = TRUE,
                                    printToggle = TRUE),
                              "ref-TableOne_print_missing.rds")

})


test_that("dropEqual options correctly retail two-level categorical variable names", {

    ## Default table matrix
    mat_default <- print(pbcOverall)
    mat_modified <- mat_default
    ## Drop " = 1" etc by regex
    rownames(mat_modified) <- gsub(" = .* \\(", " (", rownames(mat_modified))

    ## dropEqual = TRUE to avoid creation of " = 1"
    mat_dropEqual <- print(pbcOverall, dropEqual = TRUE)

    ## Expectations
    ## These must differ.
    expect_false(identical(mat_default, mat_modified))
    expect_false(identical(rownames(mat_default), rownames(mat_modified)))
    expect_false(all(rownames(mat_default) == rownames(mat_modified)))
    ## These must match.
    expect_equal(rownames(mat_modified), rownames(mat_dropEqual))
    expect_equal(mat_modified, mat_dropEqual)
})


test_that("variable labels are correctly shown", {

    ## Construct a dataset with some variable labels.
    pbc_labeled <- pbc
    ## Continuous
    age_label <- "Age in Years"
    ## Two-level categorical
    sex_label <- "Female Sex"
    ## Multi-level categorical
    stage_label <- "Stage of the Disease"
    ## Apply labels
    labelled::var_label(pbc_labeled$age) <- age_label
    labelled::val_label(pbc_labeled$age, 1) <- "youngest"
    labelled::var_label(pbc_labeled$sex) <- sex_label
    labelled::var_label(pbc_labeled$stage) <-stage_label
    ## Show
    labelled::var_label(pbc_labeled)

    ## Construct a TableOne object.
    ## Using factorVars should not break
    pbcOverall  <- CreateTableOne(vars = vars, data = pbc_labeled, factorVars = "stage")

    mat_default  <- print(pbcOverall)
    mat_labelled <- print(pbcOverall, varLabels = TRUE)

    ## Expectations
    ## These must differ.
    expect_false(identical(mat_default, mat_labelled))
    expect_false(identical(rownames(mat_default), rownames(mat_labelled)))
    ## These labels should not exist in the original row names.
    expect_true(sum(grepl(age_label, rownames(mat_default))) == 0)
    expect_true(sum(grepl(sex_label, rownames(mat_default))) == 0)
    expect_true(sum(grepl(stage_label, rownames(mat_default))) == 0)
    ## These labels should appear only once in labelled table.
    expect_true(sum(grepl(age_label, rownames(mat_labelled))) == 1)
    expect_true(sum(grepl(sex_label, rownames(mat_labelled))) == 1)
    expect_true(sum(grepl(stage_label, rownames(mat_labelled))) == 1)

})


###
### Regression tests for the print method
################################################################################

test_that("printing of a TableOne object does not regress", {

    ## Expectations
    expect_equal_to_reference(print(pbcByTrt, printToggle = TRUE),
                              "ref-TableOne_defaultPrint.rds")

    expect_equal_to_reference(print(pbcOverall, printToggle = TRUE),
                              "ref-TableOne_overallPrint.rds")

    expect_equal_to_reference(print(pbcInclNa, printToggle = TRUE),
                              "ref-TableOne_IncludeNA.rds")

    expect_equal_to_reference(print(pbcByTrtSex, printToggle = TRUE),
                              "ref-TableOne_2StrataVars.rds")

    expect_equal_to_reference(print(pbcByTrt, catDigits = 3, contDigits = 4, pDigits = 5, printToggle = TRUE),
                              "ref-TableOne_digits.rds")

    expect_equal_to_reference(print(pbcByTrt, test = FALSE, printToggle = TRUE),
                              "ref-TableOne_noTests.rds")

    expect_equal_to_reference(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, printToggle = TRUE),
                              "ref-TableOne_nonnormal_exact.rds")

    expect_equal_to_reference(print(pbcByTrt, nonnormal = nonnormalVars, minMax = TRUE, printToggle = TRUE),
                              "ref-TableOne_nonnormal_minMax.rds")

    expect_equal_to_reference(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, noSpaces = TRUE, printToggle = TRUE),
                              "ref-TableOne_noSpaces.rds")

    expect_equal_to_reference(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, showAllLevels = TRUE, printToggle = TRUE),
                              "ref-TableOne_showAllLevels.rds")

    expect_equal_to_reference(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, noSpaces = TRUE, showAllLevels = FALSE, quote = TRUE, printToggle = TRUE),
                              "ref-TableOne_noSpaces_showAllLevels_quote.rds")

    expect_equal_to_reference(print(pbcContOnlyByTrtSex),
                              "ref-TableOne_ContOnly.rds")

    expect_equal_to_reference(print(pbcCatOnlyByTrtSex),
                              "ref-TableOne_CatOnly.rds")

    ## Add Overall Tests always with smd and tests
    expect_equal_to_reference(print(pbcByTrt_addOverall, nonnormal = nonnormalVars, exact = exactVars, noSpaces = TRUE, showAllLevels = FALSE, quote = TRUE, printToggle = TRUE, smd = TRUE, test = TRUE),
                              "ref-TableOne_noSpaces_showAllLevels_quote_addOverall.rds")

    expect_equal_to_reference(print(pbcByTrtSex_addOverall, printToggle = TRUE, smd = TRUE, test = TRUE),
                              "ref-TableOne_2StrataVars_addOverall.rds")

    expect_equal_to_reference(print(pbcContOnlyByTrtSex_addOverall, smd = TRUE, test = TRUE),
                              "ref-TableOne_ContOnly_addOverall.rds")

    expect_equal_to_reference(print(pbcCatOnlyByTrtSex_addOverall, smd = TRUE, test = TRUE),
                              "ref-TableOne_CatOnly_addOverall.rds")

    ## Regression tests for formatOptions always with smd and tests
    expect_equal_to_reference(
        print(pbcByTrt_addOverall, nonnormal = nonnormalVars, exact = exactVars,
              noSpaces = TRUE, showAllLevels = FALSE, quote = TRUE,
              printToggle = TRUE, smd = TRUE, test = TRUE,
              formatOptions = list(big.mark = ",", big.interval = 2, decimal.mark = "D")),
        "ref-TableOne_noSpaces_showAllLevels_quote_addOverall_formatOptions.rds")

    expect_equal_to_reference(
        print(pbcByTrtSex_addOverall, printToggle = TRUE, smd = TRUE, test = TRUE,
              formatOptions = list(big.mark = ",", big.interval = 2, decimal.mark = "D")),
        "ref-TableOne_2StrataVars_addOverall_formatOptions.rds")

    expect_equal_to_reference(
        print(pbcContOnlyByTrtSex_addOverall, smd = TRUE, test = TRUE,
              formatOptions = list(big.mark = ",", big.interval = 2, decimal.mark = "D")),
        "ref-TableOne_ContOnly_addOverall_formatOptions.rds")

    expect_equal_to_reference(
        print(pbcCatOnlyByTrtSex_addOverall, smd = TRUE, test = TRUE,
              formatOptions = list(big.mark = ",", big.interval = 2, decimal.mark = "D")),
        "ref-TableOne_CatOnly_addOverall_formatOptions.rds")
})


test_that("printing of a TableOne$CatTable object do not regress", {

    ## Expectations
    expect_equal_to_reference(print(pbcByTrt$CatTable, printToggle = TRUE),
                              "ref-CatTable_defaultPrint.rds")

    expect_equal_to_reference(print(pbcOverall$CatTable, printToggle = TRUE),
                              "ref-CatTable_overallPrint.rds")

    expect_equal_to_reference(print(pbcInclNa$CatTable, printToggle = TRUE),
                              "ref-CatTable_IncludeNA.rds")

    expect_equal_to_reference(print(pbcByTrtSex$CatTable, printToggle = TRUE),
                              "ref-CatTable_2StrataVars.rds")

    expect_equal_to_reference(print(pbcByTrtSex$CatTable, digits = 3, pDigits = 5, printToggle = TRUE),
                              "ref-CatTable_digits.rds")

    expect_equal_to_reference(print(pbcByTrtSex$CatTable, test = FALSE, printToggle = TRUE),
                              "ref-CatTable_noTests.rds")

    expect_equal_to_reference(print(pbcByTrt$CatTable, noSpaces = TRUE, printToggle = TRUE),
                              "ref-CatTable_noSpaces.rds")

    expect_equal_to_reference(print(pbcByTrt$CatTable, showAllLevels = TRUE, printToggle = TRUE),
                              "ref-CatTable_showAllLevels.rds")

    expect_equal_to_reference(print(pbcByTrt$CatTable, explain = FALSE, printToggle = TRUE),
                              "ref-CatTable_explain.rds")

    expect_equal_to_reference(print(pbcByTrt$CatTable, format = "f", printToggle = TRUE),
                              "ref-CatTable_format_f.rds")

    expect_equal_to_reference(print(pbcByTrt$CatTable, format = "p", printToggle = TRUE),
                              "ref-CatTable_format_p.rds")

    expect_equal_to_reference(print(pbcByTrt$CatTable, format = "pf", printToggle = TRUE),
                              "ref-CatTable_format_pf.rds")

    expect_equal_to_reference(print(pbcByTrt$CatTable, cramVars = "sex", printToggle = TRUE),
                              "ref-CatTable_cramVars.rds")

    expect_equal_to_reference(print(pbcByTrt$CatTable, noSpaces = TRUE, showAllLevels = TRUE, quote = TRUE, printToggle = TRUE),
                              "ref-CatTable_noSpaces_showAllLevels_quote.rds")

    expect_equal_to_reference(print(pbcByTrt_addOverall$CatTable, noSpaces = TRUE, showAllLevels = TRUE, quote = TRUE, printToggle = TRUE),
                              "ref-CatTable_noSpaces_showAllLevels_quote_addOverall.rds")

    expect_equal_to_reference(print(pbcByTrtSex_addOverall$CatTable, printToggle = TRUE),
                              "ref-CatTable_2StrataVars_addOverall.rds")

    ## gmodels::CrossTable
    print(pbcByTrt$CatTable, CrossTable = TRUE)
    expect_output(print(pbcByTrt$CatTable, CrossTable = TRUE),
"|-------------------------|
|                       N |
| Chi-square contribution |
|           N / Row Total |
|           N / Col Total |
|         N / Table Total |
|-------------------------|")

})


test_that("printing of a TableOne$ContTable object do not regress", {

    ## Expectations
    expect_equal_to_reference(print(pbcByTrt$ContTable, printToggle = TRUE),
                              "ref-ContTable_defaultPrint.rds")

    expect_equal_to_reference(print(pbcOverall$ContTable, printToggle = TRUE),
                              "ref-ContTable_overallPrint.rds")

    expect_equal_to_reference(print(pbcByTrtSex$ContTable, printToggle = TRUE),
                              "ref-ContTable_2StrataVars.rds")

    expect_equal_to_reference(print(pbcByTrt$ContTable, digits = 3, pDigits = 5, printToggle = TRUE),
                              "ref-ContTable_digits.rds")

    expect_equal_to_reference(print(pbcByTrt$ContTable, test = FALSE, printToggle = TRUE),
                              "ref-ContTable_noTests.rds")

    expect_equal_to_reference(print(pbcByTrt$ContTable, nonnormal = nonnormalVars, exact = exactVars, printToggle = TRUE),
                              "ref-ContTable_nonnormal_exact.rds")

    expect_equal_to_reference(print(pbcByTrt$ContTable, nonnormal = nonnormalVars, minMax = TRUE, printToggle = TRUE),
                              "ref-ContTable_nonnormal_minMax.rds")

    expect_equal_to_reference(print(pbcByTrt$ContTable, noSpaces = TRUE, printToggle = TRUE),
                              "ref-ContTable_noSpaces.rds")

    expect_equal_to_reference(print(pbcByTrt$ContTable, explain = FALSE, printToggle = TRUE),
                              "ref-ContTable_explain.rds")

    expect_equal_to_reference(print(pbcByTrt$ContTable, noSpaces = TRUE, showAllLevels = TRUE, quote = TRUE, printToggle = TRUE),
                              "ref-ContTable_noSpaces_showAllLevels_quote.rds")

    expect_equal_to_reference(print(pbcByTrt_addOverall$ContTable, noSpaces = TRUE, showAllLevels = TRUE, quote = TRUE, printToggle = TRUE, smd = TRUE),
                              "ref-ContTable_noSpaces_showAllLevels_quote_addOverall.rds")

    expect_equal_to_reference(print(pbcByTrtSex_addOverall$ContTable, printToggle = TRUE),
                              "ref-ContTable_2StrataVars_addOverall.rds")
})


###
### Regression tests for print.TableOne via git
################################################################################

test_that("printing of a TableOne object does not regress (via git)", {

    ## Expectations
    expect_known_output(print(pbcByTrt, printToggle = TRUE),
                        "ref-TableOne_defaultPrint.txt")

    expect_known_output(print(pbcOverall, printToggle = TRUE),
                        "ref-TableOne_overallPrint.txt")

    expect_known_output(print(pbcInclNa, printToggle = TRUE),
                        "ref-TableOne_IncludeNA.txt")

    expect_known_output(print(pbcByTrtSex, printToggle = TRUE),
                        "ref-TableOne_2StrataVars.txt")

    expect_known_output(print(pbcByTrt, catDigits = 3, contDigits = 4, pDigits = 5, printToggle = TRUE),
                        "ref-TableOne_digits.txt")

    expect_known_output(print(pbcByTrt, test = FALSE, printToggle = TRUE),
                        "ref-TableOne_noTests.txt")

    expect_known_output(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, printToggle = TRUE),
                        "ref-TableOne_nonnormal_exact.txt")

    expect_known_output(print(pbcByTrt, nonnormal = nonnormalVars, minMax = TRUE, printToggle = TRUE),
                        "ref-TableOne_nonnormal_minMax.txt")

    expect_known_output(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, noSpaces = TRUE, printToggle = TRUE),
                        "ref-TableOne_noSpaces.txt")

    expect_known_output(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, showAllLevels = TRUE, printToggle = TRUE),
                        "ref-TableOne_showAllLevels.txt")

    expect_known_output(print(pbcByTrt, nonnormal = nonnormalVars, exact = exactVars, noSpaces = TRUE, showAllLevels = FALSE, quote = TRUE, printToggle = TRUE),
                        "ref-TableOne_noSpaces_showAllLevels_quote.txt")

    expect_known_output(print(pbcContOnlyByTrtSex),
                        "ref-TableOne_ContOnly.txt")

    expect_known_output(print(pbcCatOnlyByTrtSex),
                        "ref-TableOne_CatOnly.txt")

    ## Add Overall Tests always with smd and tests
    expect_known_output(print(pbcByTrt_addOverall, nonnormal = nonnormalVars, exact = exactVars, noSpaces = TRUE, showAllLevels = FALSE, quote = TRUE, printToggle = TRUE, smd = TRUE, test = TRUE),
                        "ref-TableOne_noSpaces_showAllLevels_quote_addOverall.txt")

    expect_known_output(print(pbcByTrtSex_addOverall, printToggle = TRUE, smd = TRUE, test = TRUE),
                        "ref-TableOne_2StrataVars_addOverall.txt")

    expect_known_output(print(pbcContOnlyByTrtSex_addOverall, smd = TRUE, test = TRUE),
                        "ref-TableOne_ContOnly_addOverall.txt")

    expect_known_output(print(pbcCatOnlyByTrtSex_addOverall, smd = TRUE, test = TRUE),
                        "ref-TableOne_CatOnly_addOverall.txt")

    ## Regression tests for formatOptions always with smd and tests
    expect_known_output(
        print(pbcByTrt_addOverall, nonnormal = nonnormalVars, exact = exactVars,
              noSpaces = TRUE, showAllLevels = FALSE, quote = TRUE,
              printToggle = TRUE, smd = TRUE, test = TRUE,
              formatOptions = list(big.mark = ",", big.interval = 2, decimal.mark = "D")),
        "ref-TableOne_noSpaces_showAllLevels_quote_addOverall_formatOptions.txt")

    expect_known_output(
        print(pbcByTrtSex_addOverall, printToggle = TRUE, smd = TRUE, test = TRUE,
              formatOptions = list(big.mark = ",", big.interval = 2, decimal.mark = "D")),
        "ref-TableOne_2StrataVars_addOverall_formatOptions.txt")

    expect_known_output(
        print(pbcContOnlyByTrtSex_addOverall, smd = TRUE, test = TRUE,
              formatOptions = list(big.mark = ",", big.interval = 2, decimal.mark = "D")),
        "ref-TableOne_ContOnly_addOverall_formatOptions.txt")

    expect_known_output(
        print(pbcCatOnlyByTrtSex_addOverall, smd = TRUE, test = TRUE,
              formatOptions = list(big.mark = ",", big.interval = 2, decimal.mark = "D")),
        "ref-TableOne_CatOnly_addOverall_formatOptions.txt")
})


###
### Tests for the summary method
################################################################################
test_that("summary method works without errors", {

    ## Expectations
    summary(pbcOverall)
    expect_output(summary(pbcOverall), "strata: Overall")
    summary(pbcInclNa)
    expect_output(summary(pbcInclNa), "<NA>")
    summary(pbcByTrt)
    expect_output(summary(pbcByTrt), "Standardize mean differences")
    summary(pbcByTrtSex)
    expect_output(summary(pbcByTrtSex),
                  "Standardize mean differences")
    summary(pbcContOnlyByTrtSex)
    expect_output(summary(pbcContOnlyByTrtSex),
                  "### Summary of continuous variables ###")
    summary(pbcCatOnlyByTrtSex)
    expect_output(summary(pbcCatOnlyByTrtSex), "3 vs 4")

    ## AddOverall Test
    expect_output(summary(pbcByTrtSex_addOverall), "trt:sex: Overall")
    expect_output(summary(pbcByTrtSex_addOverall), "trt:sex: 1:f")

})
