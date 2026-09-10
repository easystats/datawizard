#' Calculate chi square test for tabulate
#'
#' @param dwtable    A dwtable object.
#' @param show_table Should the table be shown with the chi sqare
#' @param ...  Other options
#'
#' @export
data_chisq <- function(dwtable, show_table = TRUE, ...) {
  tablelist <- as.table(dwtable)
  chisqlist <- lapply(tablelist, stats::chisq.test)
  if (length(chisqlist) == 1) {
    return(chisqlist[[1]])
  }
  chisqlist
}
