tableone
===============================================================================

An R package to create "Table 1", description of baseline characteristics

For some reasons, there are no good functions to create the "Table 1", i.e., description of baseline characteristics in R although it is essential in every medical research. The package was insipired by descriptive statistics functions in Deducer, Java-based GUI package. This package does not require GUI or Java, and intended for CUI users.

Examples
-------------------------------------------------------------------------------

**Continuous variables**

You can specifiy which variables you  handle as nonnormal variables. The corresponding p-values will be from nonparametric tests.
```
> print(contTable2, nonnormal = nonNormalVars)
                         Stratified by status
                          0                         1                          2                          p     
  n                       232                       25                         161                              
  time (mean (sd))        2333.16 (994.66)          1546.20 (753.07)           1376.93 (1049.23)          <0.001
  age (mean (sd))           49.51 (10.43)             41.69 (6.33)               53.92 (9.81)             <0.001
  bili (median [IQR])        0.90 [0.60, 1.60]         3.10 [1.30, 3.50]          3.20 [1.40, 7.10]       <0.001
  chol (median [IQR])      292.00 [245.25, 360.00]   343.50 [311.50, 445.50]    339.00 [257.50, 454.00]    0.004
  albumin (mean (sd))        3.59 (0.36)               3.49 (0.46)                3.36 (0.47)             <0.001
  copper (median [IQR])     52.00 [33.50, 77.50]     102.00 [69.00, 129.50]     111.00 [63.50, 199.25]    <0.001
  alk.phos (median [IQR]) 1107.50 [794.00, 1641.00] 1345.00 [1028.50, 1777.50] 1664.00 [1029.00, 2468.00] <0.001
  ast (mean (sd))          107.29 (52.79)            130.12 (36.95)             141.93 (58.38)            <0.001
  trig (median [IQR])      104.00 [80.00, 133.50]    124.00 [85.75, 145.75]     122.00 [91.00, 171.00]     0.006
  platelet (mean (sd))     261.16 (88.61)            309.60 (102.65)            242.49 (107.88)            0.011
  protime (median [IQR])    10.40 [9.90, 10.80]       10.30 [10.00, 10.70]       11.00 [10.47, 11.80]     <0.001
```

**Categorical variables**

For categorical variables, levels are handled in decent ways. For two-level variables, only the upper level is shown to avoid redundant information (change factor level to reverse). For multi-category variables, the level names come under the variable name for easy viewing. Frequency and/or percentage can be chosen for format.
```
> print(catTable2, exact = exactVars)
                 Stratified by status
                  0           1          2           p      test 
  n               232         25         161                     
  trt = 2 (%)      85 (50.6)   9 (47.4)   60 (48.0)   0.894      
  sex = f (%)     215 (92.7)  22 (88.0)  137 (85.1)   0.053      
  ascites = 1 (%)   1 ( 0.6)   0 ( 0.0)   23 (18.4)  <0.001 exact
  hepato = 1 (%)   60 (35.7)  12 (63.2)   88 (70.4)  <0.001      
  spiders = 1 (%)  33 (19.6)   5 (26.3)   52 (41.6)  <0.001      
  edema (%)                                          <0.001 exact
     0            216 (93.1)  22 (88.0)  116 (72.0)              
     0.5           15 ( 6.5)   3 (12.0)   26 (16.1)              
     1              1 ( 0.4)   0 ( 0.0)   19 (11.8)              
  stage (%)                                          <0.001      
     1             19 ( 8.3)   0 ( 0.0)    2 ( 1.3)              
     2             64 (27.8)   5 (20.0)   23 (14.6)              
     3             97 (42.2)  10 (40.0)   48 (30.6)              
     4             50 (21.7)  10 (40.0)   84 (53.5)              
```

Installation
-------------------------------------------------------------------------------

This is a piece of software in active development. Please do not rely on this in a production environment.

Your contribution is appreciated. Please report bugs, request feasures, and send improvement suggestions.


The source tar.gz file for the point release is available at:

https://github.com/kaz-yos/tableone/releases

On Mac (and Linux system?), you can then do the following in the Terminal.
```
R CMD INSTALL tableone_ver.tar.gz
```

Alternatively, you can use the devtools to install from github directly. 
```r
## Install devtools (if you do not have it already)
install.packages("devtools")
## Load devtools
library(devtools)
## Install directly from github
install_github(repo = "kaz-yos/tableone")
## Load 
library(tableone)
```
Using devtools require some preparation, please see the following.
http://www.rstudio.com/projects/devtools/
