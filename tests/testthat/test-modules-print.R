################################################################################
### Define behaviors of formatters for printing
##
## Created on: 2020-07-22
## Author: Kazuki Yoshida
################################################################################

library(testthat)

set.seed(384452428)


###
### Describe formatters that are common to all variables
################################################################################

describe("ModuleFormatNumericVector", {

    it("creates a character vector from numeric", {
        eg_num <- rnorm(n = 1000, sd = 10000)
        expect_true(is.character(ModuleFormatNumericVector(x = eg_num,
                                                           digits = 1)))
    })

    it("aligns numbers vertically with appropriate digits", {
        eg_num <- c(1480.61787738437,
                    4110.08315419201,
                    0,
                    -2072.69756465216,
                    -679.687900350949,
                    -4313.08599182373,
                    -8542.3392630109,
                    -7823.95523336236,
                    -21234.6667893914,
                    -274.448648953072,
                    -186.40513038433,
                    NA,
                    NaN,
                    Inf)
        ## Decimal alignment example. Copied and edited below.
        c("  1480.61787738437",
          "  4110.08315419201",
          "     0",
          " -2072.69756465216",
          "  -679.687900350949",
          " -4313.08599182373",
          " -8542.3392630109",
          " -7823.95523336236",
          "-21234.6667893914",
          "  -274.448648953072",
          "  -186.40513038433",
          "    NA",
          "   NaN",
          "   Inf")
        expect_equal(ModuleFormatNumericVector(x = eg_num,
                                               digits = 0),
                     c("  1481",
                       "  4110",
                       "     0",
                       " -2073",
                       "  -680",
                       " -4313",
                       " -8542",
                       " -7824",
                       "-21235",
                       "  -274",
                       "  -186",
                       "    NA",
                       "   NaN",
                       "   Inf"))
        expect_equal(ModuleFormatNumericVector(x = eg_num,
                                               digits = 1),
                     c("  1480.6",
                       "  4110.1",
                       "     0.0",
                       " -2072.7",
                       "  -679.7",
                       " -4313.1",
                       " -8542.3",
                       " -7824.0",
                       "-21234.7",
                       "  -274.4",
                       "  -186.4",
                       "      NA",
                       "     NaN",
                       "     Inf"))
        expect_equal(ModuleFormatNumericVector(x = eg_num,
                                               digits = 2),
                     c("  1480.62",
                       "  4110.08",
                       "     0.00",
                       " -2072.70",
                       "  -679.69",
                       " -4313.09",
                       " -8542.34",
                       " -7823.96",
                       "-21234.67",
                       "  -274.45",
                       "  -186.41",
                       "       NA",
                       "      NaN",
                       "      Inf"))
        expect_equal(ModuleFormatNumericVector(x = eg_num,
                                               digits = 3),
                     c("  1480.618",
                       "  4110.083",
                       "     0.000",
                       " -2072.698",
                       "  -679.688",
                       " -4313.086",
                       " -8542.339",
                       " -7823.955",
                       "-21234.667",
                       "  -274.449",
                       "  -186.405",
                       "        NA",
                       "       NaN",
                       "       Inf"))
    })

    it("uses decimal.mark and big.mark when specified", {
        eg_num <- c(1480.61787738437,
                    4110.08315419201,
                    0,
                    -2072.69756465216,
                    -679.687900350949,
                    -4313.08599182373,
                    -8542.3392630109,
                    -7823.95523336236,
                    -21234.6667893914,
                    -274.448648953072,
                    -186.40513038433,
                    NA,
                    NaN,
                    Inf)
        expect_equal(ModuleFormatNumericVector(x = eg_num,
                                               digits = 0,
                                               formatOptions = list(decimal.mark = "_",
                                                                    big.mark = ",")),
                     c("  1,481",
                       "  4,110",
                       "      0",
                       " -2,073",
                       "   -680",
                       " -4,313",
                       " -8,542",
                       " -7,824",
                       "-21,235",
                       "   -274",
                       "   -186",
                       "     NA",
                       "    NaN",
                       "    Inf"))
        expect_equal(ModuleFormatNumericVector(x = eg_num,
                                               digits = 1,
                                               formatOptions = list(decimal.mark = "_",
                                                                    big.mark = ",")),
                     c("  1,480_6",
                       "  4,110_1",
                       "      0_0",
                       " -2,072_7",
                       "   -679_7",
                       " -4,313_1",
                       " -8,542_3",
                       " -7,824_0",
                       "-21,234_7",
                       "   -274_4",
                       "   -186_4",
                       "       NA",
                       "      NaN",
                       "      Inf"))
        expect_equal(ModuleFormatNumericVector(x = eg_num,
                                               digits = 2,
                                               formatOptions = list(decimal.mark = "_",
                                                                    big.mark = ",")),
                     c("  1,480_62",
                       "  4,110_08",
                       "      0_00",
                       " -2,072_70",
                       "   -679_69",
                       " -4,313_09",
                       " -8,542_34",
                       " -7,823_96",
                       "-21,234_67",
                       "   -274_45",
                       "   -186_41",
                       "        NA",
                       "       NaN",
                       "       Inf"))
        expect_equal(ModuleFormatNumericVector(x = eg_num,
                                               digits = 3,
                                               formatOptions = list(decimal.mark = "_",
                                                                    big.mark = ",")),
                     c("  1,480_618",
                       "  4,110_083",
                       "      0_000",
                       " -2,072_698",
                       "   -679_688",
                       " -4,313_086",
                       " -8,542_339",
                       " -7,823_955",
                       "-21,234_667",
                       "   -274_449",
                       "   -186_405",
                       "         NA",
                       "        NaN",
                       "        Inf"))
    })
})

describe("ModuleFormatPercents (for missing percentage)", {

    it("creates a character vector from numeric", {
        eg_perc <- c(0, runif(n = 1000, min = 0, max = 1), 1) * 100
        expect_true(is.character(ModuleFormatPercents(percents = eg_perc,
                                                      digits = 1)))
    })

    it("aligns numbers vertically with appropriate digits", {
        eg_perc <- as.double(c(0, 1.1111, 100, NA, NaN, Inf))
        expect_equal(ModuleFormatPercents(percents = eg_perc,
                                          digits = 0),
                     c("  0",
                       "  1",
                       "100",
                       " NA",
                       "NaN",
                       "Inf"))
        expect_equal(ModuleFormatPercents(percents = eg_perc,
                                          digits = 1),
                     c("  0.0",
                       "  1.1",
                       "100.0",
                       "   NA",
                       "  NaN",
                       "  Inf"))
        expect_equal(ModuleFormatPercents(percents = eg_perc,
                                          digits = 2),
                     c("  0.00",
                       "  1.11",
                       "100.00",
                       "    NA",
                       "   NaN",
                       "   Inf"))
        expect_equal(ModuleFormatPercents(percents = eg_perc,
                                          digits = 3),
                     c("  0.000",
                       "  1.111",
                       "100.000",
                       "     NA",
                       "    NaN",
                       "    Inf"))
    })

    it("uses decimal.mark when specified", {
        eg_perc <- as.double(c(0, 1.1111, 100, NA, NaN, Inf))
        expect_equal(ModuleFormatPercents(percents = eg_perc,
                                          digits = 1),
                     c("  0.0",
                       "  1.1",
                       "100.0",
                       "   NA",
                       "  NaN",
                       "  Inf"))
        expect_equal(ModuleFormatPercents(percents = eg_perc,
                                          digits = 1,
                                          formatOptions = list(decimal.mark = "-")),
                     c("  0-0",
                       "  1-1",
                       "100-0",
                       "   NA",
                       "  NaN",
                       "  Inf"))
        expect_equal(ModuleFormatPercents(percents = eg_perc,
                                          digits = 1,
                                          formatOptions = list(decimal.mark = "D")),
                     c("  0D0",
                       "  1D1",
                       "100D0",
                       "   NA",
                       "  NaN",
                       "  Inf"))
    })
})

describe("ModuleFormatPValues", {

    example_p_values <- c(0, runif(n = 1000, min = 0, max = 1), 1)

    it("creates a character vector", {
        expect_true(is.character(ModuleFormatPValues(pValues = example_p_values,
                                                     pDigits = 2)))
    })

    it("creates <0.01 etc given 0", {
        eg_p <- as.double(0)
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 1),
                     "<0.1")
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 2),
                     "<0.01")
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 3),
                     "<0.001")
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 4),
                     "<0.0001")
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 5),
                     "<0.00001")
    })

    it("returns character version for what pDigits permits, otherwise gives <0.01 etc", {
        eg_p <- as.double(0.0011)
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 1),
                     "<0.1")
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 2),
                     "<0.01")
        ## Leading space when there is no <
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 3),
                     " 0.001")
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 4),
                     " 0.0011")
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 5),
                     " 0.00110")
    })

    it("alignes output vertically", {
        ## Note that for rounding off a 5, the IEC 60559 standard (see also
        ## ‘IEEE 754’) is expected to be used, ‘_go to the even digit_’.
        ## Therefore ‘round(0.5)’ is ‘0’ and ‘round(-1.5)’ is ‘-2’.  However,
        ## this is dependent on OS services and on representation error
        ## (since e.g. ‘0.15’ is not represented exactly, the rounding rule
        ## applies to the represented number and not to the printed number,
        ## and so ‘round(0.15, 1)’ could be either ‘0.1’ or ‘0.2’).
        eg_p <- as.double(c(0, 0.1111111, 0.15, 0.94, 0.9999, 1, NA, NaN))
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 1),
                     c("<0.1",
                       " 0.1",
                       " 0.1", # round(0.15, 1) => 0.1 in R 4.0.2 on macOS 10.15.5.
                       " 0.9",
                       " 1.0",
                       " 1.0",
                       "  NA",
                       " NaN"))
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 2),
                     c("<0.01",
                       " 0.11",
                       " 0.15",
                       " 0.94",
                       " 1.00",
                       " 1.00",
                       "   NA",
                       "  NaN"))
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 3),
                     c("<0.001",
                       " 0.111",
                       " 0.150",
                       " 0.940",
                       " 1.000",
                       " 1.000",
                       "    NA",
                       "   NaN"))
    })

    it("uses decimal.mark when specified", {
        eg_p <- as.double(c(0, 0.1111111, 0.15, 0.94, 0.9999, 1, NA, NaN))
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 2),
                     c("<0.01",
                       " 0.11",
                       " 0.15",
                       " 0.94",
                       " 1.00",
                       " 1.00",
                       "   NA",
                       "  NaN"))
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 2,
                                         formatOption = list(decimal.mark = "-")),
                     c("<0-01",
                       " 0-11",
                       " 0-15",
                       " 0-94",
                       " 1-00",
                       " 1-00",
                       "   NA",
                       "  NaN"))
        expect_equal(ModuleFormatPValues(pValues = eg_p,
                                         pDigits = 2,
                                         formatOption = list(decimal.mark = "D")),
                     c("<0D01",
                       " 0D11",
                       " 0D15",
                       " 0D94",
                       " 1D00",
                       " 1D00",
                       "   NA",
                       "  NaN"))
    })
})


###
### Describe formatters for continuous variables
################################################################################

describe("ModuleConvertNormal", {

    ## Taken from pbc$alk.phos in survival
    eg_min    <-   289.000
    eg_p25    <-   871.500
    eg_median <-  1259.000
    eg_mean   <-  1982.656
    eg_p75    <-  1980.000
    eg_max    <- 13862.400
    eg_sd     <-  2140.389

    ## The function receives a single row matrix of summary statistics
    ## from ModuleContFormatStrata.
    eg_mat <- matrix(c(eg_mean, eg_sd),
                     ncol = 2,
                     byrow = TRUE,
                     dimnames = list(NULL,
                                     c("mean", "sd")))

    it("returns a data frame with two columns", {
        expect_true(is.data.frame(ModuleConvertNormal(eg_mat,
                                                      digits = 1)))
        expect_equal(ncol(ModuleConvertNormal(eg_mat,
                                              digits = 1)),
                     2)
    })

    it("returns a column of numeric mean and a formatted column of character (SD)", {
        ## Column names must be generic (col1, col2).
        ## Both mean (SD) and median [IQR] are stacked up together.
        expect_equal(ModuleConvertNormal(eg_mat,
                                         digits = 0),
                     data.frame(col1 = eg_mean,
                                col2 = " (2140)"))
        expect_equal(ModuleConvertNormal(eg_mat,
                                         digits = 1),
                     data.frame(col1 = eg_mean,
                                col2 = " (2140.4)"))
        expect_equal(ModuleConvertNormal(eg_mat,
                                         digits = 2),
                     data.frame(col1 = eg_mean,
                                col2 = " (2140.39)"))
        expect_equal(ModuleConvertNormal(eg_mat,
                                         digits = 3),
                     data.frame(col1 = eg_mean,
                                col2 = " (2140.389)"))
    })

    it("respects formatOptions", {
        expect_equal(ModuleConvertNormal(eg_mat,
                                         digits = 0,
                                         formatOptions = list(big.mark = ",", decimal.mark = "D")),
                     data.frame(col1 = eg_mean,
                                col2 = " (2,140)"))
        expect_equal(ModuleConvertNormal(eg_mat,
                                         digits = 1,
                                         formatOptions = list(big.mark = ",", decimal.mark = "D")),
                     data.frame(col1 = eg_mean,
                                col2 = " (2,140D4)"))
        expect_equal(ModuleConvertNormal(eg_mat,
                                         digits = 2,
                                         formatOptions = list(big.mark = ",", decimal.mark = "D")),
                     data.frame(col1 = eg_mean,
                                col2 = " (2,140D39)"))
        expect_equal(ModuleConvertNormal(eg_mat,
                                         digits = 3,
                                         formatOptions = list(big.mark = ",", decimal.mark = "D")),
                     data.frame(col1 = eg_mean,
                                col2 = " (2,140D389)"))
    })
})

describe("ModuleConvertNonNormal", {

    ## Taken from pbc$alk.phos in survival
    eg_min    <-   289.000
    eg_p25    <-   871.500
    eg_median <-  1259.000
    eg_mean   <-  1982.656
    eg_p75    <-  1980.000
    eg_max    <- 13862.400
    eg_sd     <-  2140.389

    ## The function receives a single row matrix of summary statistics
    ## from ModuleContFormatStrata.
    eg_mat <- matrix(c(eg_median, eg_p25, eg_p75, eg_min, eg_max),
                     ncol = 5,
                     byrow = TRUE,
                     dimnames = list(NULL,
                                     c("median", "p25", "p75", "min", "max")))

    it("returns a data frame with two columns", {
        expect_true(is.data.frame(ModuleConvertNonNormal(eg_mat,
                                                         digits = 1)))
        expect_equal(ncol(ModuleConvertNonNormal(eg_mat,
                                                 digits = 1)),
                     2)
    })

    it("returns a column of numeric mean and a formatted column of character (SD)", {
        ## Column names must be generic (col1, col2).
        ## Both mean (SD) and median [IQR] are stacked up together.
        expect_equal(ModuleConvertNonNormal(eg_mat,
                                            digits = 0),
                     data.frame(col1 = eg_median,
                                col2 = " [872, 1980]"))
        expect_equal(ModuleConvertNonNormal(eg_mat,
                                            digits = 1),
                     data.frame(col1 = eg_median,
                                col2 = " [871.5, 1980.0]"))
        expect_equal(ModuleConvertNonNormal(eg_mat,
                                            digits = 2),
                     data.frame(col1 = eg_median,
                                col2 = " [871.50, 1980.00]"))
        expect_equal(ModuleConvertNonNormal(eg_mat,
                                            digits = 3),
                     data.frame(col1 = eg_median,
                                col2 = " [871.500, 1980.000]"))
    })

    it("respects formatOptions", {
        expect_equal(ModuleConvertNonNormal(eg_mat,
                                            digits = 0,
                                            formatOptions = list(big.mark = ",", decimal.mark = "D")),
                     data.frame(col1 = eg_median,
                                col2 = " [872, 1,980]"))
        expect_equal(ModuleConvertNonNormal(eg_mat,
                                            digits = 1,
                                            formatOptions = list(big.mark = ",", decimal.mark = "D")),
                     data.frame(col1 = eg_median,
                                col2 = " [871D5, 1,980D0]"))
        expect_equal(ModuleConvertNonNormal(eg_mat,
                                            digits = 2,
                                            formatOptions = list(big.mark = ",", decimal.mark = "D")),
                     data.frame(col1 = eg_median,
                                col2 = " [871D50, 1,980D00]"))
        expect_equal(ModuleConvertNonNormal(eg_mat,
                                            digits = 3,
                                            formatOptions = list(big.mark = ",", decimal.mark = "D")),
                     data.frame(col1 = eg_median,
                                col2 = " [871D500, 1,980D000]"))
    })
})


###
### Describe formatters for categorical variables
################################################################################

describe("ModuleCatFormatVariables", {

    ## It receives a list of data frames named lstVars.
    ## Example below:
    ## lstVars
    ## $status
    ##     n miss p.miss level freq   percent cum.percent
    ## 1 418    0      0     0  232 55.502392    55.50239
    ## 2 418    0      0     1   25  5.980861    61.48325
    ## 3 418    0      0     2  161 38.516746   100.00000
    ##
    ## $trt
    ##     n miss   p.miss level freq  percent cum.percent
    ## 1 418  106 25.35885     1  158 50.64103    50.64103
    ## 2 418  106 25.35885     2  154 49.35897   100.00000

})
