#' @title Rescale design weights for multilevel analysis
#' @name rescale_weights
#'
#' @description Most functions to fit multilevel and mixed effects models only
#' allow the user to specify frequency weights, but not design (i.e., sampling
#' or probability) weights, which should be used when analyzing complex samples
#' (e.g., probability samples). `rescale_weights()` implements two algorithms,
#' one proposed by \cite{Asparouhov (2006)} and \cite{Carle (2009)}, to rescale
#' design weights in survey data to account for the grouping structure of
#' multilevel models, and one based on the design effect proposed by
#' \cite{Kish (1965)}, to rescale weights by the design effect to account for
#' additional sampling error introduced by weighting.
#' @param data A data frame.
#' @param by Variable names (as character vector, or as formula), indicating
#' the grouping structure (strata) of the survey data (level-2-cluster
#' variable). It is also possible to create weights for multiple group
#' variables; in such cases, each created weighting variable will be suffixed
#' by the name of the group variable. This argument is required for
#' `method = "carle"`, but optional for `method = "kish"`.
#' @param probability_weights Variable indicating the probability (design or
#' sampling) weights of the survey data (level-1-weight), provided as character
#' string or formula.
#' @param nest Logical, if `TRUE` and `by` indicates at least two group
#' variables, then groups are "nested", i.e. groups are now a combination from
#' each group level of the variables in `by`. This argument is not used when
#' `method = "kish"`.
#' @param method String, indicating which rescale-method is used for rescaling
#' weights. Can be either `"carle"` (default) or `"kish"`. See 'Details'. If
#' `method = "carle"`, the `by` argument is required.
#'
#' @return
#' `data`, including the new weighting variable(s). For `method = "carle"`, new
#' columns `rescaled_weights_a` and `rescaled_weights_b` are returned, and for
#' `method = "kish"`, the returned data contains a column `rescaled_weights`.
#' These represent the rescaled design weights to use in multilevel models (use
#' these variables for the `weights` argument).
#'
#' @details
#' - `method = "carle"`
#'
#'   Rescaling is based on two methods: For `rescaled_weights_a`, the sample
#'   weights `probability_weights` are adjusted by a factor that represents the
#'   proportion of group size divided by the sum of sampling weights within each
#'   group. The adjustment factor for `rescaled_weights_b` is the sum of sample
#'   weights within each group divided by the sum of squared sample weights
#'   within each group (see Carle (2009), Appendix B). In other words,
#'   `rescaled_weights_a` "scales the weights so that the new weights sum to the
#'   cluster sample size" while `rescaled_weights_b` "scales the weights so that
#'   the new weights sum to the effective cluster size".
#'
#'   Regarding the choice between scaling methods A and B, Carle suggests that
#'   "analysts who wish to discuss point estimates should report results based
#'   on weighting method A. For analysts more interested in residual
#'   between-group variance, method B may generally provide the least biased
#'   estimates". In general, it is recommended to fit a non-weighted model and
#'   weighted models with both scaling methods and when comparing the models,
#'   see whether the "inferential decisions converge", to gain confidence in the
#'   results.
#'
#'   Though the bias of scaled weights decreases with increasing group size,
#'   method A is preferred when insufficient or low group size is a concern.
#'
#'   The group ID and probably PSU may be used as random effects (e.g. nested
#'   design, or group and PSU as varying intercepts), depending on the survey
#'   design that should be mimicked.
#'
#' - `method = "kish"`
#'
#'   Rescaling is based on scaling the sample weights so the mean value is 1,
#'   which means the sum of all weights equals the sample size. Next, the design
#'   effect (_Kish 1965_) is calculated, which is the mean of the squared
#'   weights divided by the squared mean of the weights. The scaled sample
#'   weights are then divided by the design effect. This method is most
#'   appropriate when weights are based on additional variables beyond the
#'   grouping variables in the model (e.g., other demographic characteristics),
#'   but may also be useful in other contexts.
#'
#'   Some tests on real-world survey-data suggest that, in comparison to the
#'   Carle-method, the Kish-method comes closer to estimates from a regular
#'   survey-design using the **survey** package. Note that these tests are not
#'   representative and it is recommended to check your results against a
#'   standard survey-design.
#'
#' @references
#'   - Asparouhov T. (2006). General Multi-Level Modeling with Sampling
#'   Weights. Communications in Statistics - Theory and Methods 35: 439-460
#'
#'   - Carle A.C. (2009). Fitting multilevel models in complex survey data
#'   with design weights: Recommendations. BMC Medical Research Methodology
#'   9(49): 1-13
#'
#'   - Kish, L. (1965) Survey Sampling. London: Wiley.
#'
#' @examplesIf all(insight::check_if_installed(c("lme4", "parameters"), quietly = TRUE))
#' data(nhanes_sample)
#' head(rescale_weights(nhanes_sample, "WTINT2YR", "SDMVSTRA"))
#'
#' # also works with multiple group-variables
#' head(rescale_weights(nhanes_sample, "WTINT2YR", c("SDMVSTRA", "SDMVPSU")))
#'
#' # or nested structures.
#' x <- rescale_weights(
#'   data = nhanes_sample,
#'   probability_weights = "WTINT2YR",
#'   by = c("SDMVSTRA", "SDMVPSU"),
#'   nest = TRUE
#' )
#' head(x)
#'
#' \donttest{
#' # compare different methods, using multilevel-Poisson regression
#'
#' d <- rescale_weights(nhanes_sample, "WTINT2YR", "SDMVSTRA")
#' result1 <- lme4::glmer(
#'   total ~ factor(RIAGENDR) + log(age) + factor(RIDRETH1) + (1 | SDMVPSU),
#'   family = poisson(),
#'   data = d,
#'   weights = rescaled_weights_a
#' )
#' result2 <- lme4::glmer(
#'   total ~ factor(RIAGENDR) + log(age) + factor(RIDRETH1) + (1 | SDMVPSU),
#'   family = poisson(),
#'   data = d,
#'   weights = rescaled_weights_b
#' )
#'
#' d <- rescale_weights(
#'   nhanes_sample,
#'   "WTINT2YR",
#'   method = "kish"
#' )
#' result3 <- lme4::glmer(
#'   total ~ factor(RIAGENDR) + log(age) + factor(RIDRETH1) + (1 | SDMVPSU),
#'   family = poisson(),
#'   data = d,
#'   weights = rescaled_weights
#' )
#' d <- rescale_weights(
#'   nhanes_sample,
#'   "WTINT2YR",
#'   "SDMVSTRA",
#'   method = "kish"
#' )
#' result4 <- lme4::glmer(
#'   total ~ factor(RIAGENDR) + log(age) + factor(RIDRETH1) + (1 | SDMVPSU),
#'   family = poisson(),
#'   data = d,
#'   weights = rescaled_weights
#' )
#' parameters::compare_parameters(
#'   list(result1, result2, result3, result4),
#'   exponentiate = TRUE,
#'   column_names = c("Carle (A)", "Carle (B)", "Kish", "Kish (grouped)")
#' )
#' }
#' @export
rescale_weights <- function(data,
                            probability_weights = NULL,
                            by = NULL,
                            nest = FALSE,
                            method = "carle") {
  method <- insight::validate_argument(method, c("carle", "kish"))

  # convert formulas to strings
  if (inherits(by, "formula")) {
    by <- all.vars(by)
  }

  if (inherits(probability_weights, "formula")) {
    probability_weights <- all.vars(probability_weights)
  }

  # check for existing variable names
  if ((method == "carle" && any(c("rescaled_weights_a", "rescaled_weights_b") %in% colnames(data))) ||
    (method == "kish" && "rescaled_weights" %in% colnames(data))) {
    insight::format_warning("The variable name for the rescaled weights already exists in the data. Returned columns will be renamed into unique names.") # nolint
  }

  # need probability_weights
  if (is.null(probability_weights)) {
    insight::format_error("The argument `probability_weights` is missing, but required to rescale weights.")
  }

  # check if weight has missings. we need to remove them first,
  # and add back weights to correct cases later

  weight_missings <- which(is.na(data[[probability_weights]]))
  weight_non_na <- which(!is.na(data[[probability_weights]]))

  if (length(weight_missings) > 0) {
    data_tmp <- data[weight_non_na, ]
  } else {
    data_tmp <- data
  }

  fun_args <- list(
    nest = nest,
    probability_weights = probability_weights,
    data_tmp = data_tmp,
    data = data,
    by = by,
    weight_non_na = weight_non_na
  )

  switch(method,
    carle = do.call(.rescale_weights_carle, fun_args),
    do.call(.rescale_weights_kish, fun_args)
  )
}


# rescale weights, method Kish ----------------------------

.rescale_weights_kish <- function(nest, probability_weights, data_tmp, data, by, weight_non_na) {
  # sort id
  data_tmp$.bamboozled <- seq_len(nrow(data_tmp))

  # `nest` is currently ignored
  if (isTRUE(nest)) {
    insight::format_warning("Argument `nest` is ignored for `method = \"kish\"`.")
  }

  # check by argument
  if (!is.null(by) && !all(by %in% colnames(data_tmp))) {
    dont_exist <- setdiff(by, colnames(data_tmp))
    insight::format_error(
      paste0(
        "The following variable(s) specified in `by` don't exist in the dataset: ",
        text_concatenate(dont_exist), "."
      ),
      .misspelled_string(colnames(data_tmp), dont_exist, "Possibly misspelled?")
    )
  } else if (is.null(by)) {
    # if `by` = NULL, we create a dummy group
    by <- "tmp_kish_by"
    data_tmp[[by]] <- 1
  }

  # split into groups, and calculate weights
  out <- lapply(split(data_tmp, data_tmp[by]), function(group_data) {
    p_weights <- group_data[[probability_weights]]
    # design effect according to Kish
    deff <- mean(p_weights^2) / (mean(p_weights)^2)
    # rescale weights, so their mean is 1
    z_weights <- p_weights * (1 / mean(p_weights))
    # divide weights by design effect
    group_data$rescaled_weights <- z_weights / deff
    group_data
  })

  # bind data
  result <- do.call(rbind, out)

  # restore original order
  result <- result[order(result$.bamboozled), ]

  # add back rescaled weights to original data, but account for missing observations
  data$rescaled_weights <- NA_real_
  data$rescaled_weights[weight_non_na] <- result$rescaled_weights
  # return result
  data
}


# rescale weights, method Carle ----------------------------

.rescale_weights_carle <- function(nest, probability_weights, data_tmp, data, by, weight_non_na) {
  # sort id
  data_tmp$.bamboozled <- seq_len(nrow(data_tmp))

  if (is.null(by)) {
    insight::format_error("Argument `by` must be specified. Please provide one or more variable names in `by` that indicate the grouping structure (strata) of the survey data (level-2-cluster variable).") # nolint
  }

  if (!all(by %in% colnames(data_tmp))) {
    dont_exist <- setdiff(by, colnames(data_tmp))
    insight::format_error(
      paste0(
        "The following variable(s) specified in `by` don't exist in the dataset: ",
        text_concatenate(dont_exist), "."
      ),
      .misspelled_string(colnames(data_tmp), dont_exist, "Possibly misspelled?")
    )
  }

  if (nest && length(by) < 2) {
    insight::format_warning(
      sprintf(
        "Only one group variable selected in `by`, no nested structure possible. Rescaling weights for grout '%s' now.",
        by
      )
    )
    nest <- FALSE
  }

  if (nest) {
    out <- .rescale_weights_nested(data_tmp, group = by, probability_weights, nrow(data), weight_non_na)
  } else {
    out <- lapply(by, function(i) {
      x <- .rescale_weights(data_tmp, i, probability_weights, nrow(data), weight_non_na)
      if (length(by) > 1) {
        colnames(x) <- sprintf(c("pweight_a_%s", "pweight_b_%s"), i)
      }
      x
    })
  }

  make_unique_names <- any(vapply(out, function(i) any(colnames(i) %in% colnames(data)), logical(1)))
  # add weights to data frame
  out <- do.call(cbind, list(data, out))
  # check if we have to rename columns
  if (make_unique_names) {
    colnames(out) <- make.unique(colnames(out), sep = "_")
  }

  out
}


# rescale weights, for one or more group variables ----------------------------

.rescale_weights <- function(x, group, probability_weights, n, weight_non_na) {
  # compute sum of weights per group
  design_weights <- .data_frame(
    group = sort(unique(x[[group]])),
    sum_weights_by_group = tapply(x[[probability_weights]], as.factor(x[[group]]), sum),
    sum_squared_weights_by_group = tapply(x[[probability_weights]]^2, as.factor(x[[group]]), sum),
    n_per_group = as.vector(table(x[[group]]))
  )

  colnames(design_weights)[1] <- group
  x <- merge(x, design_weights, by = group, sort = FALSE)

  # restore original order
  x <- x[order(x$.bamboozled), ]
  x$.bamboozled <- NULL

  # multiply the original weight by the fraction of the
  # sampling unit total population based on Carle 2009

  w_a <- x[[probability_weights]] * x$n_per_group / x$sum_weights_by_group
  w_b <- x[[probability_weights]] * x$sum_weights_by_group / x$sum_squared_weights_by_group

  out <- data.frame(
    rescaled_weights_a = rep(NA_real_, times = n),
    rescaled_weights_b = rep(NA_real_, times = n)
  )

  out$rescaled_weights_a[weight_non_na] <- w_a
  out$rescaled_weights_b[weight_non_na] <- w_b

  out
}


# rescale weights, for nested groups ----------------------------

.rescale_weights_nested <- function(x, group, probability_weights, n, weight_non_na) {
  groups <- expand.grid(lapply(group, function(i) sort(unique(x[[i]]))))
  colnames(groups) <- group

  # compute sum of weights per group
  design_weights <- cbind(
    groups,
    .data_frame(
      sum_weights_by_group = unlist(as.list(tapply(
        x[[probability_weights]], lapply(group, function(i) {
          as.factor(x[[i]])
        }), sum
      )), use.names = FALSE),
      sum_squared_weights_by_group = unlist(as.list(tapply(
        x[[probability_weights]]^2, lapply(group, function(i) {
          as.factor(x[[i]])
        }), sum
      )), use.names = FALSE),
      n_per_group = unlist(as.list(table(x[, group])), use.names = FALSE)
    )
  )

  x <- merge(x, design_weights, by = group, sort = FALSE)

  # restore original order
  x <- x[order(x$.bamboozled), ]
  x$.bamboozled <- NULL

  # multiply the original weight by the fraction of the
  # sampling unit total population based on Carle 2009

  w_a <- x[[probability_weights]] * x$n_per_group / x$sum_weights_by_group
  w_b <- x[[probability_weights]] * x$sum_weights_by_group / x$sum_squared_weights_by_group

  out <- data.frame(
    rescaled_weights_a = rep(NA_real_, times = n),
    rescaled_weights_b = rep(NA_real_, times = n)
  )

  out$rescaled_weights_a[weight_non_na] <- w_a
  out$rescaled_weights_b[weight_non_na] <- w_b

  out
}
