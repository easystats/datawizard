
#' @export
makepredictcall.dw_scaler <- function(var, call) {
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
    }
  )

  call$verbose <- FALSE
  call
}
