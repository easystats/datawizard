#' @title Create frequency tables of variables
#' @name data_table
#'
#' @description This function creates frequency tables of data frame, including
#' the number of levels/values as well as the distribution of raw, valid and
#' cumulative percentages.
#'
#' @param x A (grouped) data frame, a vector or factor.
#' @param name Optional character string, which includes the name that is used
#' for printing.
#' @param ... not used.
#' @inheritParams find_columns
#'
#' @return A data frame, or a list of data frames, with one frequency table
#' as data frame per variable.
#'
#' @examples
#' set.seed(123)
#' out <- ToothGrowth[sample(1:nrow(ToothGrowth), 50), ]
#' out$supp[sample(1:nrow(out), 9)] <- NA
#' out$dose[sample(1:nrow(out), 9)] <- NA
#' data_table(out[2:3])
#' @export
data_table <- function(x, ...) {
  UseMethod("data_table")
}


#' @rdname data_table
#' @export
data_table.default <- function(x, name = NULL, verbose = TRUE, ...) {
  freq_table <- tryCatch(table(addNA(x)), error = function(e) NULL)

  if (is.null(freq_table)) {
    warning(paste0("Can't compute frequency tables for objects of class '", class(x)[1], "'."), call. = FALSE)
    return(NULL)
  }

  if (is.null(name)) {
    name <- attr(x, "label", exact = TRUE)
    if (is.null(name)) {
      name <- name <- tryCatch(insight::safe_deparse(substitute(x)), error = function(e) NULL)
    }
  }

  out <- data_rename(data.frame(freq_table, stringsAsFactors = FALSE),
                     replacement = c("Value", "N"))

  out$`Raw %` <- 100 * out$N / sum(out$N)
  out$`Valid %` <- c(100 * out$N[1:(nrow(out) - 1)] / sum(out$N[1:(nrow(out) - 1)]), NA)
  out$`Cumulative %` <- cumsum(out$`Valid %`)

  attr(out, "type") <- .variable_type(x)
  attr(out, "name") <- name
  attr(out, "total_n") <- sum(out$N, na.rm = TRUE)
  attr(out, "valid_n") <- sum(out$`Valid %`, na.rm = TRUE)

  class(out) <- c("dw_data_table", "data.frame")

  out
}


#' @rdname data_table
#' @export
data_table.data.frame <- function(x,
                                  select = NULL,
                                  exclude = NULL,
                                  ignore_case = FALSE,
                                  verbose = TRUE,
                                  ...) {
  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)
  out <- lapply(select, function(i) {
    name <- attr(x[[i]], "label", exact = TRUE)
    if (is.null(name)) {
      name <- i
    }
    data_table(x[[i]], name = name, verbose = verbose, ...)
  })

  class(out) <- unique("dw_data_tables", "list")
  out
}


#' @export
data_table.grouped_df <- function(x,
                                  select = NULL,
                                  exclude = NULL,
                                  ignore_case = FALSE,
                                  verbose = TRUE,
                                  ...) {
  info <- attributes(x)

  # dplyr >= 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
    grps <- lapply(grps, function(x) x + 1)
  } else {
    grps <- grps[[".rows"]]
  }

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- data_table(
      x[rows, ],
      select = select,
      exclude = exclude,
      ignore_case = ignore_case,
      verbose = verbose,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}




# methods --------------------

#' @export
print.dw_data_table <- function(x, ...) {
  a <- attributes(x)

  # prepare title
  summary_line <- sprintf("# total N=%g valid N=%g\n\n", a$total_n, a$valid_n)

  ftab <- insight::format_table(as.data.frame(x))
  ftab[] <- lapply(ftab, function(i) {
    i[i == ""] <- "<NA>"
    i
  })
  ftab$N <- gsub("\\.00$", "", ftab$N)

  cat(insight::print_color(a$name, "red"))
  cat(insight::print_color(sprintf(" <%s>\n", a$type), "blue"))
  cat(insight::print_color(summary_line, "blue"))

  cat(insight::export_table(ftab, missing = "<NA>"))
  invisible(x)
}


#' @export
print.dw_data_tables <- function(x, ...) {
  for (i in x) {
    print(i)
    cat("\n")
  }
}




# tools --------------------

.variable_type <- function(x) {
  if (is.ordered(x))
    vt <- "ord"
  else if (is.factor(x))
    vt <- "fct"
  else if (methods::is(x, "Date"))
    vt <- "date"
  else {
    vt <- switch(typeof(x), logical = "lgl", integer = "int",
                 double = "dbl", character = "chr", complex = "cpl",
                 closure = "fn", environment = "env", typeof(x))
  }

  switch(vt, "ord" = "ordinal", "fct" = "categorical", "dbl" = "numeric",
         "int" = "integer", "chr" = "character", "lbl" = "labelled",
         "cpl" = "complex", vt)
}
