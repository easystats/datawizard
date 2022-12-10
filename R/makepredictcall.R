#' Utility Function for Safe Prediction with `datawizard` transformers
#'
#' This function allows for the use of (some of) `datawizard`'s transformers
#' inside a model formula. See examples below.
#' \cr\cr
#' Currently, only [center()] and [standardize()] are supported.
#'
#' @inheritParams stats::makepredictcall
#'
#' @inherit stats::makepredictcall return
#' @importFrom stats makepredictcall
#'
#' @seealso [stats::makepredictcall()], [center()], [standardize()]
#'
#' @examples
#'
#' data("mtcars")
#' train <- mtcars[1:30, ]
#' test <- mtcars[31:32, ]
#'
#' m1 <- lm(mpg ~ center(hp), data = train)
#' predict(m1, newdata = test) # Data is "centered" before the prediction is made,
#'                             # according to the center of the old data
#'
#' m2 <- lm(mpg ~ standardize(hp), data = train)
#' m3 <- lm(mpg ~ scale(hp), data = train) # same as above
#' predict(m2, newdata = test) # Data is "standardized" before the prediction is made.
#' predict(m3, newdata = test) # Data is "standardized" before the prediction is made.
#'
#'
#' @export
makepredictcall.dw_transformer <- function(var, call) {
  if (is.matrix(var) || is.array(var))
    insight::format_error("datawizard scalers in model formulas are not supported for matrices.")

  switch(as.character(call)[1L],

    centre = ,
    center =  {
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
    },

    # ELSE:
    {
      return(call)
    }
  )

  call$verbose <- FALSE
  call
}
