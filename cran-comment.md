## What's new
The following changes are included.

tableone 0.10.0 (2019-02-16)
----------------------------------------------------------------

BUG FIXES

* Fix a missing value handling issue (via @chipmanj).
  Closes issue #26.

* mean (sd) was changed to mean (SD) to be consistent with
  median [IQR].

NEW FEATURES

* Add a helper for exporting TableOne objects to Markdown
  (via @malcolmbarrett). Currently, this is very basic. We hope
  to extend its functionalities.

* README.md now lists similar or complementary projects.


## Test environments
* Local OS X 10.14.2, R 3.5.2
* Ubuntu Linux on Travis-CI (release and devel)
* win-builder (release and devel)


## R CMD check results
* ERRORs: None
* WARNINGs: None
* NOTEs: None


## Downstream dependencies
── CHECK ───────────────────────────────────────────────────────────────────────────────────────────────── 5 packages ──
✔ CluMP 0.7                              ── E: 0     | W: 0     | N: 0
✔ cvcrand 0.0.2                          ── E: 0     | W: 0     | N: 0
✔ jstable 0.7.9                          ── E: 0     | W: 0     | N: 0
✔ RcmdrPlugin.EZR 1.38                   ── E: 0     | W: 0     | N: 0
✔ rpsftm 1.2.3                           ── E: 0     | W: 0     | N: 0
OK: 5
BROKEN: 0
