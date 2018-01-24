## What's new
The following changes are included.

tableone 0.9.2 (2018-01-24)
----------------------------------------------------------------

BUG FIXES

* Avoid issues with --disable-long-double and alternative BLAS.
* Handling of lme4 models was improved in ShowRegTable.
* Do not reduce dimension of SMD matrix when only one contrast exists.

NEW FEATURES

* The "varLabels" option for the print.TableOne method was added.
  When TRUE, instead of printing the variable names, their
  corresponding variable labels are used. Variable labels must be
  stored in the data frame to be used via labelled::var_label
  function. This option is also available in ExtractSmd function.

* The "dropEqual" option for the print methods was implemented. If
  TRUE, the level description for two-level variables such as " = 1"
  and " = TRUE" are not shown. This can obscure what level is being
  shown depending on the variable naming scheme, thus, should only
  be used after the initial results were checked for correctness.


## Test environments
* Local OS X 10.13.2, R 3.4.3
* Ubuntu Linux on Travis-CI (release and devel)
* win-builder (release and devel)


## R CMD check results
* ERRORs: None
* WARNINGs: None
* NOTEs: None


## Downstream dependencies
Checked RcmdrPlugin.EZR: 0 errors | 0 warnings | 0 notes
Checked rpsftm         : 0 errors | 0 warnings | 0 notes
