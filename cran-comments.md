## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 18 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## Other comments

This is a patch release that should (hopefully) fix a failure occurring on macOS
when building vignettes. This only happens on macOS with R 4.3. We tried to
reproduce this locally and in CI with the same setup, but we couldn't. Hence, we
removed all vignettes (except for one "Overview"), they are now only available
on the website.
