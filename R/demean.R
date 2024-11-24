#' Compute group-meaned and de-meaned variables
#'
#' @description
#'
#' `demean()` computes group- and de-meaned versions of a variable that can be
#' used in regression analysis to model the between- and within-subject effect.
#' `degroup()` is more generic in terms of the centering-operation. While
#' `demean()` always uses mean-centering, `degroup()` can also use the mode or
#' median for centering.
#'
#' @param x A data frame.
#' @param select Character vector (or formula) with names of variables to select
#'   that should be group- and de-meaned.
#' @param by Character vector (or formula) with the name of the variable that
#'   indicates the group- or cluster-ID. For cross-classified or nested designs,
#'   `by` can also identify two or more variables as group- or cluster-IDs. If
#'   the data is nested and should be treated as such, set `nested = TRUE`. Else,
#'   if `by` defines two or more variables and `nested = FALSE`, a cross-classified
#'   design is assumed. Note that `demean()` and `degroup()` can't handle a mix
#'   of nested and cross-classified designs in one model.
#'
#'   For nested designs, `by` can be:
#'   - a character vector with the name of the variable that indicates the
#'     levels, ordered from *highest* level to *lowest* (e.g.
#'     `by = c("L4", "L3", "L2")`.
#'   - a character vector with variable names in the format `by = "L4/L3/L2"`,
#'     where the levels are separated by `/`.
#'
#'   See also section _De-meaning for cross-classified designs_ and
#'   _De-meaning for nested designs_ below.
#' @param nested Logical, if `TRUE`, the data is treated as nested. If `FALSE`,
#'   the data is treated as cross-classified. Only applies if `by` contains more
#'   than one variable.
#' @param center Method for centering. `demean()` always performs
#'   mean-centering, while `degroup()` can use `center = "median"` or
#'   `center = "mode"` for median- or mode-centering, and also `"min"`
#'   or `"max"`.
#' @param suffix_demean,suffix_groupmean String value, will be appended to the
#'   names of the group-meaned and de-meaned variables of `x`. By default,
#'   de-meaned variables will be suffixed with `"_within"` and
#'   grouped-meaned variables with `"_between"`.
#' @param add_attributes Logical, if `TRUE`, the returned variables gain
#'   attributes to indicate the within- and between-effects. This is only
#'   relevant when printing `model_parameters()` - in such cases, the
#'   within- and between-effects are printed in separated blocks.
#' @inheritParams center
#'
#' @return
#' A data frame with the group-/de-meaned variables, which get the suffix
#' `"_between"` (for the group-meaned variable) and `"_within"` (for the
#' de-meaned variable) by default. For cross-classified or nested designs,
#' the name pattern of the group-meaned variables is the name of the centered
#' variable followed by the name of the variable that indicates the related
#' grouping level, e.g. `predictor_L3_between` and `predictor_L2_between`.
#'
#' @seealso If grand-mean centering (instead of centering within-clusters)
#'   is required, see [`center()`]. See [`performance::check_heterogeneity_bias()`]
#'   to check for heterogeneity bias.
#'
#' @section Heterogeneity Bias:
#'
#' Mixed models include different levels of sources of variability, i.e.
#' error terms at each level. When macro-indicators (or level-2 predictors,
#' or higher-level units, or more general: *group-level predictors that
#' **vary** within and across groups*) are included as fixed effects (i.e.
#' treated as covariate at level-1), the variance that is left unaccounted for
#' this covariate will be absorbed into the error terms of level-1 and level-2
#' (_Bafumi and Gelman 2006; Gelman and Hill 2007, Chapter 12.6._):
#' "Such covariates contain two parts: one that is specific to the higher-level
#' entity that does not vary between occasions, and one that represents the
#' difference between occasions, within higher-level entities" (_Bell et al. 2015_).
#' Hence, the error terms will be correlated with the covariate, which violates
#' one of the assumptions of mixed models (iid, independent and identically
#' distributed error terms). This bias is also called the *heterogeneity bias*
#' (_Bell et al. 2015_). To resolve this problem, level-2 predictors used as
#' (level-1) covariates should be separated into their "within" and "between"
#' effects by "de-meaning" and "group-meaning": After demeaning time-varying
#' predictors, "at the higher level, the mean term is no longer constrained by
#' Level 1 effects, so it is free to account for all the higher-level variance
#' associated with that variable" (_Bell et al. 2015_).
#'
#' @section Panel data and correlating fixed and group effects:
#'
#' `demean()` is intended to create group- and de-meaned variables for panel
#' regression models (fixed effects models), or for complex
#' random-effect-within-between models (see _Bell et al. 2015, 2018_), where
#' group-effects (random effects) and fixed effects correlate (see
#' _Bafumi and Gelman 2006_). This can happen, for instance, when analyzing
#' panel data, which can lead to *Heterogeneity Bias*. To control for correlating
#' predictors and group effects, it is recommended to include the group-meaned
#' and de-meaned version of *time-varying covariates* (and group-meaned version
#' of *time-invariant covariates* that are on a higher level, e.g. level-2
#' predictors) in the model. By this, one can fit complex multilevel models for
#' panel data, including time-varying predictors, time-invariant predictors and
#' random effects.
#'
#' @section Why mixed models are preferred over fixed effects models:
#'
#' A mixed models approach can model the causes of endogeneity explicitly
#' by including the (separated) within- and between-effects of time-varying
#' fixed effects and including time-constant fixed effects. Furthermore,
#' mixed models also include random effects, thus a mixed models approach
#' is superior to classic fixed-effects models, which lack information of
#' variation in the group-effects or between-subject effects. Furthermore,
#' fixed effects regression cannot include random slopes, which means that
#' fixed effects regressions are neglecting "cross-cluster differences in the
#' effects of lower-level controls (which) reduces the precision of estimated
#' context effects, resulting in unnecessarily wide confidence intervals and
#' low statistical power" (_Heisig et al. 2017_).
#'
#' @section Terminology:
#'
#' The group-meaned variable is simply the mean of an independent variable
#' within each group (or id-level or cluster) represented by `by`. It represents
#' the cluster-mean of an independent variable. The regression coefficient of a
#' group-meaned variable is the *between-subject-effect*. The de-meaned variable
#' is then the centered version of the group-meaned variable. De-meaning is
#' sometimes also called person-mean centering or centering within clusters.
#' The regression coefficient of a de-meaned variable represents the
#' *within-subject-effect*.
#'
#' @section De-meaning with continuous predictors:
#'
#' For continuous time-varying predictors, the recommendation is to include
#' both their de-meaned and group-meaned versions as fixed effects, but not
#' the raw (untransformed) time-varying predictors themselves. The de-meaned
#' predictor should also be included as random effect (random slope). In
#' regression models, the coefficient of the de-meaned predictors indicates
#' the within-subject effect, while the coefficient of the group-meaned
#' predictor indicates the between-subject effect.
#'
#' @section De-meaning with binary predictors:
#'
#' For binary time-varying predictors, there are two recommendations. First
#' is to include the raw (untransformed) binary predictor as fixed effect
#' only and the *de-meaned* variable as random effect (random slope).
#' The alternative would be to add the de-meaned version(s) of binary
#' time-varying covariates as additional fixed effect as well (instead of
#' adding it as random slope). Centering time-varying binary variables to
#' obtain within-effects (level 1) isn't necessary. They have a sensible
#' interpretation when left in the typical 0/1 format (_Hoffmann 2015,
#' chapter 8-2.I_). `demean()` will thus coerce categorical time-varying
#' predictors to numeric to compute the de- and group-meaned versions for
#' these variables, where the raw (untransformed) binary predictor and the
#' de-meaned version should be added to the model.
#'
#' @section De-meaning of factors with more than 2 levels:
#'
#' Factors with more than two levels are demeaned in two ways: first, these
#' are also converted to numeric and de-meaned; second, dummy variables
#' are created (binary, with 0/1 coding for each level) and these binary
#' dummy-variables are de-meaned in the same way (as described above).
#' Packages like **panelr** internally convert factors to dummies before
#' demeaning, so this behaviour can be mimicked here.
#'
#' @section De-meaning interaction terms:
#'
#' There are multiple ways to deal with interaction terms of within- and
#' between-effects.
#'
#' - A classical approach is to simply use the product term of the de-meaned
#'   variables (i.e. introducing the de-meaned variables as interaction term
#'   in the model formula, e.g. `y ~ x_within * time_within`). This approach,
#'   however, might be subject to bias (see _Giesselmann & Schmidt-Catran 2020_).
#'
#' - Another option is to first calculate the product term and then apply the
#'   de-meaning to it. This approach produces an estimator "that reflects
#'   unit-level differences of interacted variables whose moderators vary
#'   within units", which is desirable if *no* within interaction of
#'   two time-dependent variables is required. This is what `demean()` does
#'   internally when `select` contains interaction terms.
#'
#' - A third option, when the interaction should result in a genuine within
#'   estimator, is to "double de-mean" the interaction terms
#'   (_Giesselmann & Schmidt-Catran 2018_), however, this is currently
#'   not supported by `demean()`. If this is required, the `wmb()`
#'   function from the **panelr** package should be used.
#'
#' To de-mean interaction terms for within-between models, simply specify
#' the term as interaction for the `select`-argument, e.g. `select = "a*b"`
#' (see 'Examples').
#'
#' @section De-meaning for cross-classified designs:
#'
#' `demean()` can handle cross-classified designs, where the data has two or
#' more groups at the higher (i.e. second) level. In such cases, the
#' `by`-argument can identify two or more variables that represent the
#'  cross-classified group- or cluster-IDs. The de-meaned variables for
#' cross-classified designs are simply subtracting all group means from each
#' individual value, i.e. _fully cluster-mean-centering_ (see _Guo et al. 2024_
#' for details). Note that de-meaning for cross-classified designs is *not*
#' equivalent to de-meaning of nested data structures from models with three or
#' more levels. Set `nested = TRUE` to explicitly assume a nested design. For
#' cross-classified designs, de-meaning is supposed to work for models like
#' `y ~ x + (1|level3) + (1|level2)`, but *not* for models like
#' `y ~ x + (1|level3/level2)`. Note that `demean()` and `degroup()` can't
#' handle a mix of nested and cross-classified designs in one model.
#'
#' @section De-meaning for nested designs:
#'
#' _Brincks et al. (2017)_ have suggested an algorithm to center variables for
#' nested designs, which is implemented in `demean()`. For nested designs, set
#' `nested = TRUE` *and* specify the variables that indicate the different
#' levels in descending order in the `by` argument. E.g.,
#' `by = c("level4", "level3, "level2")` assumes a model like
#' `y ~ x + (1|level4/level3/level2)`. An alternative notation for the
#' `by`-argument would be `by = "level4/level3/level2"`, similar to the
#' formula notation.
#'
#' @section Analysing panel data with mixed models using lme4:
#'
#' A description of how to translate the formulas described in *Bell et al. 2018*
#' into R using `lmer()` from **lme4** can be found in
#' [this vignette](https://easystats.github.io/parameters/articles/demean.html).
#'
#' @references
#'
#'   - Bafumi J, Gelman A. 2006. Fitting Multilevel Models When Predictors
#'     and Group Effects Correlate. In. Philadelphia, PA: Annual meeting of the
#'     American Political Science Association.
#'
#'   - Bell A, Fairbrother M, Jones K. 2019. Fixed and Random Effects
#'     Models: Making an Informed Choice. Quality & Quantity (53); 1051-1074
#'
#'   - Bell A, Jones K. 2015. Explaining Fixed Effects: Random Effects
#'     Modeling of Time-Series Cross-Sectional and Panel Data. Political Science
#'     Research and Methods, 3(1), 133–153.
#'
#'   - Brincks, A. M., Enders, C. K., Llabre, M. M., Bulotsky-Shearer, R. J.,
#'     Prado, G., and Feaster, D. J. (2017). Centering Predictor Variables in
#'     Three-Level Contextual Models. Multivariate Behavioral Research, 52(2),
#'     149–163. https://doi.org/10.1080/00273171.2016.1256753
#'
#'   - Gelman A, Hill J. 2007. Data Analysis Using Regression and
#'     Multilevel/Hierarchical Models. Analytical Methods for Social Research.
#'     Cambridge, New York: Cambridge University Press
#'
#'   - Giesselmann M, Schmidt-Catran, AW. 2020. Interactions in fixed
#'     effects regression models. Sociological Methods & Research, 1–28.
#'     https://doi.org/10.1177/0049124120914934
#'
#'   - Guo Y, Dhaliwal J, Rights JD. 2024. Disaggregating level-specific effects
#'     in cross-classified multilevel models. Behavior Research Methods, 56(4),
#'     3023–3057.
#'
#'   - Heisig JP, Schaeffer M, Giesecke J. 2017. The Costs of Simplicity:
#'     Why Multilevel Models May Benefit from Accounting for Cross-Cluster
#'     Differences in the Effects of Controls. American Sociological Review 82
#'     (4): 796–827.
#'
#'   - Hoffman L. 2015. Longitudinal analysis: modeling within-person
#'     fluctuation and change. New York: Routledge
#'
#' @examples
#'
#' data(iris)
#' iris$ID <- sample(1:4, nrow(iris), replace = TRUE) # fake-ID
#' iris$binary <- as.factor(rbinom(150, 1, .35)) # binary variable
#'
#' x <- demean(iris, select = c("Sepal.Length", "Petal.Length"), by = "ID")
#' head(x)
#'
#' x <- demean(iris, select = c("Sepal.Length", "binary", "Species"), by = "ID")
#' head(x)
#'
#'
#' # demean interaction term x*y
#' dat <- data.frame(
#'   a = c(1, 2, 3, 4, 1, 2, 3, 4),
#'   x = c(4, 3, 3, 4, 1, 2, 1, 2),
#'   y = c(1, 2, 1, 2, 4, 3, 2, 1),
#'   ID = c(1, 2, 3, 1, 2, 3, 1, 2)
#' )
#' demean(dat, select = c("a", "x*y"), by = "ID")
#'
#' # or in formula-notation
#' demean(dat, select = ~ a + x * y, by = ~ID)
#'
#' @export
demean <- function(x,
                   select,
                   by,
                   nested = FALSE,
                   suffix_demean = "_within",
                   suffix_groupmean = "_between",
                   add_attributes = TRUE,
                   verbose = TRUE) {
  degroup(
    x = x,
    select = select,
    by = by,
    nested = nested,
    center = "mean",
    suffix_demean = suffix_demean,
    suffix_groupmean = suffix_groupmean,
    add_attributes = add_attributes,
    verbose = verbose
  )
}


#' @rdname demean
#' @export
degroup <- function(x,
                    select,
                    by,
                    nested = FALSE,
                    center = "mean",
                    suffix_demean = "_within",
                    suffix_groupmean = "_between",
                    add_attributes = TRUE,
                    verbose = TRUE) {
  # ugly tibbles again...
  x <- .coerce_to_dataframe(x)

  center <- match.arg(tolower(center), choices = c("mean", "median", "mode", "min", "max"))

  if (inherits(select, "formula")) {
    # formula to character, remove "~", split at "+". We don't use `all.vars()`
    # here because we want to keep the interaction terms as they are
    select <- trimws(unlist(
      strsplit(gsub("~", "", insight::safe_deparse(select), fixed = TRUE), "+", fixed = TRUE),
      use.names = FALSE
    ))
  }

  # handle different "by" options
  if (inherits(by, "formula")) {
    by <- all.vars(by)
  }

  # we also allow lme4-syntax here: if by = "L4/L3/L2", we assume a nested design
  if (length(by) == 1 && grepl("/", by, fixed = TRUE)) {
    by <- insight::trim_ws(unlist(strsplit(by, "/", fixed = TRUE), use.names = FALSE))
    nested <- TRUE
  }

  # identify interaction terms
  interactions_no <- select[!grepl("(\\*|\\:)", select)]
  interactions_yes <- select[grepl("(\\*|\\:)", select)]

  # if we have interaction terms that should be de-meaned, calculate the product
  # of the terms first, then demean the product
  if (length(interactions_yes)) {
    interaction_terms <- lapply(strsplit(interactions_yes, "*", fixed = TRUE), trimws)
    product <- lapply(interaction_terms, function(i) do.call(`*`, x[, i]))
    new_dat <- as.data.frame(stats::setNames(product, gsub("\\s", "", gsub("*", "_", interactions_yes, fixed = TRUE))))
    x <- cbind(x, new_dat)
    select <- c(interactions_no, colnames(new_dat))
  }

  # check if all variables are present
  not_found <- setdiff(c(select, by), colnames(x))

  if (length(not_found)) {
    insight::format_error(
      paste0(
        "Variable",
        ifelse(length(not_found) > 1, "s ", " "),
        text_concatenate(not_found, enclose = "\""),
        ifelse(length(not_found) > 1, " were", " was"),
        " not found in the dataset."
      ),
      .misspelled_string(colnames(x), not_found, "Possibly misspelled or not yet defined?")
    )
  }

  # get data to demean...
  dat <- x[, c(select, by)]


  # find categorical predictors that are coded as factors
  categorical_predictors <- vapply(dat[select], is.factor, FUN.VALUE = logical(1L))

  # convert binary predictors to numeric
  if (any(categorical_predictors)) {
    # convert categorical to numeric, and then demean
    dat[select[categorical_predictors]] <- lapply(
      dat[select[categorical_predictors]],
      function(i) as.numeric(i) - 1
    )
    # convert categorical to dummy, and demean each binary dummy
    for (i in select[categorical_predictors]) {
      if (nlevels(x[[i]]) > 2) {
        for (j in levels(x[[i]])) {
          # create vector with zeros
          f <- rep(0, nrow(x))
          # for each matching level, set dummy to 1
          f[x[[i]] == j] <- 1
          dummy <- data.frame(f)
          # colnames = variable name + factor level
          # also add new dummy variables to "select"
          colnames(dummy) <- sprintf("%s_%s", i, j)
          select <- c(select, sprintf("%s_%s", i, j))
          # add to data
          dat <- cbind(dat, dummy)
        }
      }
    }
    # tell user...
    if (isTRUE(verbose)) {
      insight::format_alert(
        paste0(
          "Categorical predictors (",
          toString(names(categorical_predictors)[categorical_predictors]),
          ") have been coerced to numeric values to compute de- and group-meaned variables.\n"
        )
      )
    }
  }


  # group variables, then calculate the mean-value
  # for variables within each group (the group means). assign
  # mean values to a vector of same length as the data

  gm_fun <- switch(center,
    mode = function(.gm) distribution_mode(stats::na.omit(.gm)),
    median = function(.gm) stats::median(.gm, na.rm = TRUE),
    min = function(.gm) min(.gm, na.rm = TRUE),
    max = function(.gm) max(.gm, na.rm = TRUE),
    function(.gm) mean(.gm, na.rm = TRUE)
  )

  # we allow disaggregating level-specific effects for cross-classified multilevel
  # models (see Guo et al. 2024). Two levels should work as proposed by the authors,
  # more levels also already work, but need to check the formula from the paper
  # and validate results

  if (length(by) == 1) {
    # simple case: one level
    group_means_list <- lapply(select, function(i) {
      stats::ave(dat[[i]], dat[[by]], FUN = gm_fun)
    })
    names(group_means_list) <- select
    # create de-meaned variables by subtracting the group mean from each individual value
    person_means_list <- lapply(select, function(i) dat[[i]] - group_means_list[[i]])
  } else if (nested) {
    # nested design: by > 1, nested is explicitly set to TRUE
    # We want:
    # L3_between = xbar(k)
    # L2_between = xbar(j,k) - xbar(k)
    # L1_within = x(ijk) - xbar(jk)
    # , where
    # x(ijk) is the individual value / variable that is measured on level 1
    # xbar(k) <- ave(x_ijk, L3, FUN = mean), the group mean of the variable at highest level
    # xbar(jk) <- ave(x_ijk, L3, L2, FUN = mean), the group mean of the variable at second level
    group_means_list <- lapply(select, function(i) {
      out <- lapply(seq_along(by), function(k) {
        dat$higher_levels <- do.call(paste, c(dat[by[1:k]], list(sep = "_")))
        stats::ave(dat[[i]], dat$higher_levels, FUN = gm_fun)
      })
      # subtract mean of higher level from lower level
      for (j in 2:length(by)) {
        out[[j]] <- out[[j]] - out[[j - 1]]
      }
      names(out) <- paste0(select, "_", by)
      out
    })
    # create de-meaned variables by subtracting the group mean from each individual value
    person_means_list <- lapply(
      # seq_along(select),
      # function(i) dat[[select[i]]] - group_means_list[[i]][[length(by)]]
      select,
      function(i) {
        dat$higher_levels <- do.call(paste, c(dat[by], list(sep = "_")))
        dat[[i]] - stats::ave(dat[[i]], dat$higher_levels, FUN = gm_fun)
      }
    )
  } else {
    # cross-classified design: by > 1
    group_means_list <- lapply(by, function(j) {
      out <- lapply(select, function(i) {
        stats::ave(dat[[i]], dat[[j]], FUN = gm_fun)
      })
      names(out) <- paste0(select, "_", j)
      out
    })
    # de-meaned variables for cross-classified design is simply subtracting
    # all group means from each individual value
    person_means_list <- lapply(seq_along(select), function(i) {
      sum_group_means <- do.call(`+`, lapply(group_means_list, function(j) j[[i]]))
      dat[[select[i]]] - sum_group_means
    })
  }

  # preserve names
  names(person_means_list) <- select

  # convert to data frame and add suffix to column names

  group_means <- as.data.frame(group_means_list)
  person_means <- as.data.frame(person_means_list)

  colnames(person_means) <- sprintf("%s%s", colnames(person_means), suffix_demean)
  colnames(group_means) <- sprintf("%s%s", colnames(group_means), suffix_groupmean)

  if (isTRUE(add_attributes)) {
    person_means[] <- lapply(person_means, function(i) {
      attr(i, "within-effect") <- TRUE
      i
    })
    group_means[] <- lapply(group_means, function(i) {
      attr(i, "between-effect") <- TRUE
      i
    })
  }

  cbind(group_means, person_means)
}


#' @rdname demean
#' @export
detrend <- degroup
