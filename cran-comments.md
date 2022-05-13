## Test environments
* local R installation, R 4.2.0
* ubuntu 16.04 (on github-actions), R 4.2.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

* Fixes failing tests.

## revdepcheck results

There are expected to be test failures for some of the reverse dependencies (e.g. *statsExpressions*), especially due to the *insight* package update.

The respective maintainers have been informed and will be submitting revised versions of their packages to CRAN soon.
