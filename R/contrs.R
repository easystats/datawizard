#' Deviation Contrast Matrix
#'
#' Build a deviation contrast matrix, a type of _effects contrast_ matrix.
#'
#' @inheritParams stats::contr.sum
#'
#' @details
#' In effects coding, unlike dummy coding ([stats::contr.treatment()]), each
#' contrasts sums to 0. In regressions models, this results in an intercept that
#' represents the (unweighted) average of the group means. In ANOVA settings,
#' this also guarantees that lower order effects represent _main_ effects (and
#' not _simple_ or _conditional_ effects, as is the case when using
#' [stats::contr.treatment()], which is `R`'s default).
#' \cr\cr
#' `contr.deviation`, unlike [stats::contr.sum()], also has the added benefit
#' that the factor-related coefficients are interpretable. In fact, they
#' represent the same contrasts as those of [stats::contr.treatment()]: the
#' difference of each level from the base level.
#'
#' @seealso [stats::contr.sum()]
#'
#' @examples
#' \dontrun{
#' data("mtcars")
#'
#' mtcars <- data_modify(mtcars, cyl = factor(cyl))
#'
#' c.treatment <- cbind(Intercept = 1, contrasts(mtcars$cyl))
#' solve(c.treatment)
#' #>            4 6 8
#' #> Intercept  1 0 0  # mean of the 1st level
#' #> 6         -1 1 0  # 2nd level - 1st level
#' #> 8         -1 0 1  # 3rd level - 1st level
#'
#' contrasts(mtcars$cyl) <- contr.sum
#' c.sum <- cbind(Intercept = 1, contrasts(mtcars$cyl))
#' solve(c.sum)
#' #>                4      6      8
#' #> Intercept  0.333  0.333  0.333   # overall mean
#' #>            0.667 -0.333 -0.333   # 2/3 * ({2nd, 3rd} - 1st)
#' #>           -0.333  0.667 -0.333   # 2/3 * ({1st, 3rd} - 2nd)
#'
#'
#' contrasts(mtcars$cyl) <- contr.deviation
#' c.deviation <- cbind(Intercept = 1, contrasts(mtcars$cyl))
#' solve(c.deviation)
#' #>                4     6     8
#' #> Intercept  0.333 0.333 0.333   # overall mean
#' #> 6         -1.000 1.000 0.000   # 2nd level - 1st level
#' #> 8         -1.000 0.000 1.000   # 3rd level - 1st level
#'
#' ## With Interactions -----------------------------------------
#' mtcars <- data_modify(mtcars, am = factor(am))
#' mtcars <- data_arrange(mtcars, select = c("cyl", "am"))
#'
#' mm <- unique(model.matrix(~ cyl * am, data = mtcars))
#' rownames(mm) <- c(
#'   "cyl4.am0", "cyl4.am1", "cyl6.am0",
#'   "cyl6.am1", "cyl8.am0", "cyl8.am1"
#' )
#'
#' solve(mm)
#' #>             cyl4.am0 cyl4.am1 cyl6.am0 cyl6.am1 cyl8.am0 cyl8.am1
#' #> (Intercept)    0.167    0.167    0.167    0.167    0.167    0.167  # overall mean
#' #> cyl6          -0.500   -0.500    0.500    0.500    0.000    0.000  # cyl MAIN eff: 2nd - 1st
#' #> cyl8          -0.500   -0.500    0.000    0.000    0.500    0.500  # cyl MAIN eff: 2nd - 1st
#' #> am1           -0.333    0.333   -0.333    0.333   -0.333    0.333  # am MAIN eff
#' #> cyl6:am1       1.000   -1.000   -1.000    1.000    0.000    0.000
#' #> cyl8:am1       1.000   -1.000    0.000    0.000   -1.000    1.000
#' }
#'
#' @export
contr.deviation <- function(n, base = 1,
                            contrasts = TRUE,
                            sparse = FALSE) {
  cont <- stats::contr.treatment(n,
    base = base,
    contrasts = contrasts,
    sparse = sparse
  )
  if (contrasts) {
    n <- nrow(cont)
    cont <- cont - 1 / n
  }
  cont
}
