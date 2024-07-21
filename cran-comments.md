## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 18 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## Other comments

This is another patch release that should fix a failure when building vignettes.
This only happens on macOS with R 4.3. We tried to reproduce this locally and in
CI with the same setup, but we couldn't. Hence, we removed an optional dependency
that might have been the problem.
