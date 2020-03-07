## What's new
The following changes were made to correct issues seen in

Please see the problems shown on
https://cran.r-project.org/web/checks/check_results_tableone.html
Please correct before 2020-03-17 to safely retain your package on CRAN.
The CRAN Team

tableone 0.11.1 (2020-03-07)
----------------------------------------------------------------

BUG FIXES

* Revise tests to avoid failures occuring only on ATLAS, MLK, and
  OpenBLAS.

* Drop methods from Imports as it is no longer needed.


## Test environments
* Local OS X 10.15.3: R 3.6.3 (patched)
* Ubuntu Linux on Travis-CI (release and devel)
* win-builder (release and devel)


## R CMD check results
* ERRORs: None
* WARNINGs: None
* NOTEs: 6 days since last version.
I hope this NOTE is ok as the change is being made following CRAN
maintainers advice.


## Downstream dependencies
── CHECK ───────────────────────────────────────────────────────────────────────────────────────────────── 5 packages ──
✔ CluMP 0.7                              ── E: 0     | W: 0     | N: 0
✔ cvcrand 0.0.2                          ── E: 0     | W: 0     | N: 0
✔ jstable 0.7.9                          ── E: 0     | W: 0     | N: 0
✔ RcmdrPlugin.EZR 1.38                   ── E: 0     | W: 0     | N: 0
✔ rpsftm 1.2.3                           ── E: 0     | W: 0     | N: 0
OK: 5
BROKEN: 0
