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
#' predict(m1, newdata = test) # Data in "centered" before the prediction is made,
#'                             # according to the center of the old data
#'
#' m2 <- lm(mpg ~ standardize(hp), data = train)
#' m3 <- lm(mpg ~ scale(hp), data = train) # same as
#' predict(m2, newdata = test) # Data is "standardized" before the prediction is made.
#' predict(m3, newdata = test) # Data in "standardized" before the prediction is made.
#'
#'
#' @export
makepredictcall.dw_transformer <- function(var, call) {
  if (is.matrix(var) || is.array(var))
    stop("datawizard scalers in model formulas are not supported for matrices.", call. = FALSE)

  switch (as.character(call)[1L],

    centre = ,
    center =  {
      call$center <- attr(var, "center")
    },

    standardise = ,
    standardize = {
      call$center <- attr(var, "center")
      call$scale <- attr(var, "scale")
    },

    # ELSE:
    {return(call)}
  )

  call$verbose <- FALSE
  call
}
