#' @title Peek at values and type of variables in a data frame
#' @name data_peek
#'
#' @description This function creates a table a data frame, showing all
#' column names, variable types and the first values (as many as fit into
#' the screen).
#'
#' @param x A data frame.
#' @param ... not used.
#' @inheritParams find_columns
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @return fds
#'
#' @examples
#' data(efc)
#' data_peek(efc)
#' @export
data_peek <- function(x, ...) {
  UseMethod("data_peek")
}
