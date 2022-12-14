#' Utility Function for Safe Prediction with `datawizard` transformers
#'
#' This function allows for the use of (some of) `datawizard`'s transformers
#' inside a model formula. See examples below.
#' \cr\cr
#' Currently, [center()], [standardize()], [normalize()], & [rescale()] are
#' supported.
#'
#' @inheritParams stats::makepredictcall
#'
#' @inherit stats::makepredictcall return
#' @importFrom stats makepredictcall
#'
#' @seealso [stats::makepredictcall()]
#' @family datawizard-transformers
#'
#' @examples
#'
#' data("mtcars")
#' train <- mtcars[1:30, ]
#' test <- mtcars[31:32, ]
#'
#' m1 <- lm(mpg ~ center(hp), data = train)
#' predict(m1, newdata = test) # Data is "centered" before the prediction is made,
#' # according to the center of the old data
#'
#' m2 <- lm(mpg ~ standardize(hp), data = train)
#' m3 <- lm(mpg ~ scale(hp), data = train) # same as above
#' predict(m2, newdata = test) # Data is "standardized" before the prediction is made.
#' predict(m3, newdata = test) # Data is "standardized" before the prediction is made.
#'
#'
#' m4 <- lm(mpg ~ normalize(hp), data = mtcars)
#' m5 <- lm(mpg ~ rescale(hp, to = c(-3, 3)), data = mtcars)
#'
#' (newdata <- data.frame(hp = c(range(mtcars$hp), 400))) # 400 is outside original range!
#'
#' model.frame(delete.response(terms(m4)), data = newdata)
#' model.frame(delete.response(terms(m5)), data = newdata)
#'
#' @export
makepredictcall.dw_transformer <- function(var, call) {
  if (is.matrix(var) || is.array(var)) {
    insight::format_error("datawizard scalers in model formulas are not supported for matrices.")
  }

  switch(as.character(call)[1L],
    centre = ,
    center = {
      call$center <- attr(var, "center")
    },
    standardise = ,
    standardize = {
      call$center <- attr(var, "center")
      call$scale <- attr(var, "scale")
    },
    normalize = ,
    normalise = {
      call$min_value <- attr(var, "min_value")
      call$range_difference <- attr(var, "range_difference")
      call$vector_length <- attr(var, "vector_length")
      call$include_bounds <- attr(var, "include_bounds")
      call$flag_bounds <- attr(var, "flag_bounds")
    },
    rescale = {
      call$min_value <- attr(var, "min_value")
      call$max_value <- attr(var, "max_value")
      call$new_min <- attr(var, "new_min")
      call$new_max <- attr(var, "new_max")
    },

    # ELSE:
    {
      return(call)
    }
  )

  call$verbose <- FALSE
  call
}
