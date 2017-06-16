## What's new
The following changes are included.

tableone 0.8.0 (2017-06-15)
----------------------------------------------------------------
NEW FEATURES
* The "missing" option for the print methods was implemented. If
  TRUE, a column called "Missing" is added as the rightmost column
  of the formatted table. This represents percentage of missing
  observation in each variable. Please note this is the percentage
  with respect to the unweighted raw observations even in weighted
  tables.
* The "padColnames" option was added the print.TableOne method. If
  TRUE, the column names of the formatted table become space-padded
  to center them.

tableone 0.7.6 (2016-07-12)
----------------------------------------------------------------
BUG FIXES
* The explanation for the "factorVars" argument for the functions
  CreateTableOne and svyCreateTableOne were changed for clarity.
  When factor variables are included in the argument, they are
  releveled to exlude empty levels. This was not clearly documented
  in the previous documentation. Thanks @eribul.
* svyrep.design objects (survey design objects with replicate weights)
  are allowed for the data argument in svyTableOne. This is considered
  experimental. Thanks @przemo.

tableone 0.7.5 (2016-04-10)
----------------------------------------------------------------
BUG FIXES
* ShowRegTable() now correctly supports models fit with geepack,
  nlme, and lme4.

tableone 0.7.4 (2016-03-31)
----------------------------------------------------------------
NEW FEATURE
* Define SMD := 0 when the numerator is 0 even if the denominator
  is also 0. This is more intuitive because a constant compared
  across two groups will give an SMD of 0 rather than NaN (0/0).
  For example, if two groups being compared both only have one
  gender (all female or all male), then SMD for the gender
  variable is defined as 0.

## Test environments
* Local OS X 10.12.5, R 3.4.0
* Ubuntu Linux on Travis-CI (oldrel, release, and devel)
* win-builder (release and devel)

## R CMD check results
* ERRORs: None
* WARNINGs: None
* NOTEs:
 - Author e-mail check

## Downstream dependencies
RcmdrPlugin.EZR GUI frontend: No change was made to APIs.
