## What's new
The following changes are included.

tableone 0.11.0 (2020-03-01)
----------------------------------------------------------------

NEW FEATURES

* @ndevln contributed addOverall option, which add the overall
  column side-by-side with sratified columns when creating a
  table object.

BUG FIXES

* test-only dependency (suggests) on dummies was dropped as it
  produced warnings with model.matrix().

* In preparation for R 4.0.0, ModuleCreateTableForOneVar and
  svyCatSummaryForOneVar now use stringsAsFactors = FALSE.
  Some internal representation of data changed as a result.

## Test environments
* Local OS X 10.15.3: R 3.6.1, 3.6.2, 3.6.3 (patched), 4.0.0 Under (2020/02/29, r77878))
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
