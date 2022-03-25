#' Prepare objects for visualisation
#'
#' @description This function prepares objects for visualisation by returning a list of
#' layers with data and geoms that can be easily plotted using for instance
#' `ggplot2`.
#'
#' If the `see` package is installed, the call to `visualization_recipe()` can be
#' replaced by `plot()`, which will internally call the former and then plot it
#' using `ggplot`. The resulting plot can be customized ad-hoc (by adding
#' ggplot's geoms, theme or specifications), or via some of the arguments
#' of `visualisation_recipe()` that control the aesthetic parameters.
#'
#' See the specific documentation page for your object's class:
#'  - {[modelbased][modelbased::visualisation_recipe.estimate_predicted] (`estimate_means`, `estimate_contrasts`, `estimate_slopes`, `estimate_predicted`, `estimate_grouplevel`)}
#'  - {[correlation][correlation::visualisation_recipe.easycor_test] (`correlation`, `cor_test`)}
#'
#'
#' @param x An `easystats` object.
#' @param ... Other arguments passed to other functions.
#'
#' @export
visualisation_recipe <- function(x, ...) {
  UseMethod("visualisation_recipe")
}


#' @export
print.visualisation_recipe <- function(x, ...) {
  for (i in 1:length(x)) {
    l <- x[[paste0("l", i)]]
    insight::print_color(paste0("Layer ", i, "\n--------\n"), "blue")
    insight::print_color(paste0("Geom type: ", ifelse(is.null(l$geom), "[NULL]", l$geom), "\n"), "yellow")

    elements <- names(l)[!sapply(l, is.null)]

    # Loop through all elements of list
    for (element in elements[elements != "geom"]) {

      # Print element name
      if (element == "aes") {
        cat("aes_string(\n")
      } else {
        cat(paste0(element, " = "))
      }

      # Print element
      if (element == "data") {
        cat(paste0("[", paste0(dim(l$data), collapse = " x "), "]"))
      } else if (element == "aes") {
        for (aes in names(l$aes)) {
          if (!is.null(l$aes[[aes]])) {
            if (is.character(l$aes[[aes]])) {
              cat(paste0("  ", aes, " = '", l$aes[[aes]], "'\n"))
            } else {
              cat(paste0("  ", aes, " = ", l$aes[[aes]], "\n"))
            }
          }
        }
        cat(")")
      } else {
        if (is.character(l[[element]]) || is.numeric(l[[element]]) || is.factor(l[[element]])) {
          if (is.character(l[[element]])) {
            cat(paste0("'", l[[element]], "'"))
          } else {
            if (length(l[[element]]) == 1) {
              cat(l[[element]])
            } else {
              cat(paste0("c(", paste0(l[[element]], collapse = ", "), ")"))
            }
          }
        } else {
          cat(paste0("class: ", class(l[[element]]), collapse = "/"))
        }
      }
      cat("\n")
    }
    cat("\n")
  }
}



#' @export
plot.visualisation_recipe <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}
