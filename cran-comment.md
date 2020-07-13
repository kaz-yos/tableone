## What's new

tableone 0.11.2 (2020-07-13)
----------------------------------------------------------------

BUG FIXES by @ndevln (PR #66)

* Hmisc labels no longer cause continuous vars to be treated as
  factors (Closes Issue #64)

* Setting value labels via labelled no longer result in tableone
  dropping variables.

ADDITIONAL BUG FIXES

* Add rmarkdown to Suggests.


## Test environments
* Local OS X 10.15.5: R 4.0.2
* Ubuntu Linux on Travis-CI (release and devel)
* win-builder (release and devel)


## R CMD check results
* ERRORs: None
* WARNINGs: None
* NOTEs: None


## Downstream dependencies
