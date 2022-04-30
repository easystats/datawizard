#' @title Create a grouped data frame
#' @name data_group
#'
#' @description This function is comparable to `dplyr::group_by()`, but just
#' following the **datawizard** function design. `data_ungroup()` removes the
#' grouping information from a grouped data frame.
#'
#' @param x A data frame
#' @inheritParams find_columns
#'
#' @return A grouped data frame, i.e. a data frame with additional information
#' about the grouping structure saved as attributes.
#'
#' @examples
#' data(efc)
#' if (requireNamespace("poorman")) {
#'   suppressPackageStartupMessages(library(poorman, quietly = TRUE))
#'
#'   # total mean
#'   efc %>%
#'     summarize(mean_hours = mean(c12hour, na.rm = TRUE))
#'
#'   # mean by educational level
#'   efc %>%
#'     data_group(c172code) %>%
#'     summarize(mean_hours = mean(c12hour, na.rm = TRUE))
#' }
#' @export
data_group <- function(x,
                       select = NULL,
                       exclude = NULL,
                       ignore_case = FALSE,
                       verbose = TRUE,
                       ...) {
  # variables for grouping
  select <- .select_nse(select, x, exclude, ignore_case)
  # create grid with combinations of all levels
  grid <- as.data.frame(expand.grid(lapply(x[select], unique)))
  # sort grid
  grid <- grid[do.call(order, grid), , drop = FALSE]

  .rows <- lapply(1:nrow(grid), function(i) {
    as.integer(data_match(
      x,
      to = grid[i, , drop = FALSE],
      match = "and",
      return_indices = TRUE,
      drop_na = FALSE
    ))
  })
  grid[[".rows"]] <- .rows

  # remove data_match attributes
  attr(grid, "out.attrs") <- NULL
  attr(grid, ".drop") <- TRUE

  attr(x, "groups") <- grid
  class(x) <- unique(c("grouped_df", "data.frame"), class(x))

  x
}


#' @rdname data_group
#' @export
data_ungroup <- function(x,
                         verbose = TRUE,
                         ...) {
  attr(x, "groups") <- NULL
  class(x) <- unique(setdiff(class(x), "grouped_df"))

  x
}
