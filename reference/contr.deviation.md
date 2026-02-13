# Deviation Contrast Matrix

Build a deviation contrast matrix, a type of *effects contrast* matrix.

## Usage

``` r
contr.deviation(n, base = 1, contrasts = TRUE, sparse = FALSE)
```

## Arguments

- n:

  a vector of levels for a factor, or the number of levels.

- base:

  an integer specifying which group is considered the baseline group.
  Ignored if `contrasts` is `FALSE`.

- contrasts:

  a logical indicating whether contrasts should be computed.

- sparse:

  logical indicating if the result should be sparse (of class
  [`dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)),
  using package [Matrix](https://CRAN.R-project.org/package=Matrix).

## Details

In effects coding, unlike treatment/dummy coding
([`stats::contr.treatment()`](https://rdrr.io/r/stats/contrast.html)),
each contrast sums to 0. In regressions models, this results in an
intercept that represents the (unweighted) average of the group means.
In ANOVA settings, this also guarantees that lower order effects
represent *main* effects (and not *simple* or *conditional* effects, as
is the case when using R's default
[`stats::contr.treatment()`](https://rdrr.io/r/stats/contrast.html)).\
\
Deviation coding (`contr.deviation`) is a type of effects coding. With
deviation coding, the coefficients for factor variables are interpreted
as the difference of each factor level from the base level (this is the
same interpretation as with treatment/dummy coding). For example, for a
factor `group` with levels "A", "B", and "C", with `contr.devation`, the
intercept represents the overall mean (average of the group means for
the 3 groups), and the coefficients `groupB` and `groupC` represent the
differences between the A group mean and the B and C group means,
respectively.\
\
Sum coding
([`stats::contr.sum()`](https://rdrr.io/r/stats/contrast.html)) is
another type of effects coding. With sum coding, the coefficients for
factor variables are interpreted as the difference of each factor level
from **the grand (across-groups) mean**. For example, for a factor
`group` with levels "A", "B", and "C", with `contr.sum`, the intercept
represents the overall mean (average of the group means for the 3
groups), and the coefficients `group1` and `group2` represent the
differences the **A** and **B** group means from the overall mean,
respectively.

## See also

[`stats::contr.sum()`](https://rdrr.io/r/stats/contrast.html)

## Examples

``` r
if (FALSE) { # !identical(Sys.getenv("IN_PKGDOWN"), "true")
# \donttest{
data("mtcars")

mtcars <- data_modify(mtcars, cyl = factor(cyl))

c.treatment <- cbind(Intercept = 1, contrasts(mtcars$cyl))
solve(c.treatment)
#>            4 6 8
#> Intercept  1 0 0  # mean of the 1st level
#> 6         -1 1 0  # 2nd level - 1st level
#> 8         -1 0 1  # 3rd level - 1st level

contrasts(mtcars$cyl) <- contr.sum
c.sum <- cbind(Intercept = 1, contrasts(mtcars$cyl))
solve(c.sum)
#>                4      6      8
#> Intercept  0.333  0.333  0.333   # overall mean
#>            0.667 -0.333 -0.333   # deviation of 1st from overall mean
#>           -0.333  0.667 -0.333   # deviation of 2nd from overall mean


contrasts(mtcars$cyl) <- contr.deviation
c.deviation <- cbind(Intercept = 1, contrasts(mtcars$cyl))
solve(c.deviation)
#>                4     6     8
#> Intercept  0.333 0.333 0.333   # overall mean
#> 6         -1.000 1.000 0.000   # 2nd level - 1st level
#> 8         -1.000 0.000 1.000   # 3rd level - 1st level

## With Interactions -----------------------------------------
mtcars <- data_modify(mtcars, am = C(am, contr = contr.deviation))
mtcars <- data_arrange(mtcars, select = c("cyl", "am"))

mm <- unique(model.matrix(~ cyl * am, data = mtcars))
rownames(mm) <- c(
  "cyl4.am0", "cyl4.am1", "cyl6.am0",
  "cyl6.am1", "cyl8.am0", "cyl8.am1"
)

solve(mm)
#>             cyl4.am0 cyl4.am1 cyl6.am0 cyl6.am1 cyl8.am0 cyl8.am1
#> (Intercept)    0.167    0.167    0.167    0.167    0.167    0.167  # overall mean
#> cyl6          -0.500   -0.500    0.500    0.500    0.000    0.000  # cyl MAIN eff: 2nd - 1st
#> cyl8          -0.500   -0.500    0.000    0.000    0.500    0.500  # cyl MAIN eff: 2nd - 1st
#> am1           -0.333    0.333   -0.333    0.333   -0.333    0.333  # am MAIN eff
#> cyl6:am1       1.000   -1.000   -1.000    1.000    0.000    0.000
#> cyl8:am1       1.000   -1.000    0.000    0.000   -1.000    1.000
# }
}
```
