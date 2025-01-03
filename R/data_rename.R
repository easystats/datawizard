#' @title Rename columns and variable names
#' @name data_rename
#'
#' @description Safe and intuitive functions to rename variables or rows in
#'   data frames. `data_rename()` will rename column names, i.e. it facilitates
#'   renaming variables. `data_rename_rows()` is a convenient shortcut
#'   to add or rename row names of a data frame, but unlike `row.names()`, its
#'   input and output is a data frame, thus, integrating smoothly into a
#'   possible pipe-workflow.
#'
#' @inheritParams extract_column_names
#' @param data A data frame.
#' @param replacement Character vector. Can be one of the following:
#'   - A character vector that indicates the new names of the columns selected
#'     in `select`. `select` and `replacement` must be of the same length.
#'   - A string (i.e. character vector of length 1) with a "glue" styled
#'     pattern. Currently supported tokens are:
#'     - `{col}` which will be replaced by the column name, i.e. the
#'       corresponding value in `select`.
#'     - `{n}` will be replaced by the number of the variable that is replaced.
#'     - `{letter}` will be replaced by alphabetical letters in sequential
#'       order.
#'       If more than 26 letters are required, letters are repeated, but have
#'       sequential numeric indices (e.g., `a1` to `z1`, followed by `a2` to
#'       `z2`).
#'     - Finally, the name of a user-defined object that is available in the
#'       environment can be used. Note that the object's name is not allowed to
#'       be one of the pre-defined tokens, `"col"`, `"n"` and `"letter"`.
#'
#'     An example for the use of tokens is...
#'     ```r
#'     data_rename(
#'       mtcars,
#'       select = c("am", "vs"),
#'       replacement = "new_name_from_{col}"
#'     )
#'     ```
#'     ... which would return new column names `new_name_from_am` and
#'     `new_name_from_vs`. See 'Examples'.
#'
#' If `select` is a named vector, `replacement` is ignored.
#' @param rows Vector of row names.
#' @param safe Deprecated. Passing unknown column names now always errors.
#' @param pattern Deprecated. Use `select` instead.
#' @param ... Other arguments passed to or from other functions.
#'
#' @details
#' `select` can also be a named character vector. In this case, the names are
#' used to rename the columns in the output data frame. If you have a named
#' list, use `unlist()` to convert it to a named vector. See 'Examples'.
#'
#' @return A modified data frame.
#'
#' @examples
#' # Rename columns
#' head(data_rename(iris, "Sepal.Length", "length"))
#'
#' # Use named vector to rename
#' head(data_rename(iris, c(length = "Sepal.Length", width = "Sepal.Width")))
#'
#' # Change all
#' head(data_rename(iris, replacement = paste0("Var", 1:5)))
#'
#' # Use glue-styled patterns
#' head(data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "formerly_{col}"))
#' head(data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "{col}_is_column_{n}"))
#' head(data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "new_{letter}"))
#'
#' # User-defined glue-styled patterns from objects in environment
#' x <- c("hi", "there", "!")
#' head(data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "col_{x}"))
#' @seealso
#' - Add a prefix or suffix to column names: [data_addprefix()], [data_addsuffix()]
#' - Functions to reorder or remove columns: [data_reorder()], [data_relocate()],
#'   [data_remove()]
#' - Functions to reshape, pivot or rotate data frames: [data_to_long()],
#'   [data_to_wide()], [data_rotate()]
#' - Functions to recode data: [rescale()], [reverse()], [categorize()],
#'   [recode_values()], [slide()]
#' - Functions to standardize, normalize, rank-transform: [center()], [standardize()],
#'   [normalize()], [ranktransform()], [winsorize()]
#' - Split and merge data frames: [data_partition()], [data_merge()]
#' - Functions to find or select columns: [data_select()], [extract_column_names()]
#' - Functions to filter rows: [data_match()], [data_filter()]
#'
#' @export
data_rename <- function(data,
                        select = NULL,
                        replacement = NULL,
                        safe = TRUE,
                        verbose = TRUE,
                        pattern = NULL,
                        ...) {
  # check for valid input
  if (!is.data.frame(data)) {
    insight::format_error("Argument `data` must be a data frame.")
  }
  # If the user does data_rename(iris, pattern = "Sepal.Length", "length"),
  # then "length" is matched to select by position while it's the replacement
  # => do the switch manually
  if (!is.null(pattern)) {
    .is_deprecated("pattern", "select")
    if (!is.null(select)) {
      replacement <- select
    }
    select <- pattern
  }
  if (isFALSE(safe)) {
    insight::format_warning("In `data_rename()`, argument `safe` is no longer used and will be removed in a future release.") # nolint
  }

  # change all names if no pattern specified
  select <- .select_nse(
    select,
    data,
    exclude = NULL,
    ignore_case = NULL,
    regex = NULL,
    allow_rename = TRUE,
    verbose = verbose,
    ifnotfound = "error"
  )

  # Forbid partially named "select",
  # Ex: if select = c("foo" = "Species", "Sepal.Length") then the 2nd name and
  # 2nd value are "Sepal.Length"
  if (!is.null(names(select)) && any(names(select) == select)) {
    insight::format_error("When `select` is a named vector, all elements must be named.")
  }

  # check if `select` has names, and if so, use as "replacement"
  if (!is.null(names(select))) {
    replacement <- names(select)
  }

  # coerce to character
  replacement <- as.character(replacement)

  # check if `replacement` has no empty strings and no NA values
  invalid_replacement <- is.na(replacement) | !nzchar(replacement)
  if (any(invalid_replacement)) {
    if (is.null(names(select))) {
      # when user did not match `select` with `replacement`
      msg <- c(
        "`replacement` is not allowed to have `NA` or empty strings.",
        sprintf(
          "Following values in `select` have no match in `replacement`: %s",
          toString(select[invalid_replacement])
        )
      )
    } else {
      # when user did not name all elements of `select`
      msg <- c(
        "Either name all elements of `select` or use `replacement`.",
        sprintf(
          "Following values in `select` were not named: %s",
          toString(select[invalid_replacement])
        )
      )
    }
    insight::format_error(msg)
  }

  # if duplicated names in replacement, append ".2", ".3", etc. to duplicates
  # ex: c("foo", "foo") -> c("foo", "foo.2")
  if (anyDuplicated(replacement) > 0L) {
    dup <- as.data.frame(table(replacement))
    dup <- dup[dup$Freq > 1, ]
    for (i in dup$replacement) {
      to_replace <- which(replacement == i)[-1]
      new_replacement <- paste0(i, ".", 1 + seq_along(to_replace))
      replacement[to_replace] <- new_replacement
    }
  }

  # check if we have "glue" styled replacement-string
  glue_style <- length(replacement) == 1 && grepl("{", replacement, fixed = TRUE)

  if (length(replacement) > length(select)) {
    insight::format_error("There are more names in `replacement` than in `select`.")
  } else if (length(replacement) < length(select) && !glue_style) {
    insight::format_error("There are more names in `select` than in `replacement`")
  }

  # if we have glue-styled replacement-string, create replacement select now
  if (glue_style) {
    replacement <- .glue_replacement(select, replacement)
  }

  for (i in seq_along(select)) {
    if (!is.na(replacement[i])) {
      data <- .data_rename(data, select[i], replacement[i], safe, verbose)
    }
  }

  data
}

#' @keywords internal
.data_rename <- function(data, pattern, replacement, safe = TRUE, verbose = TRUE) {
  if (!pattern %in% names(data)) {
    if (isTRUE(safe)) {
      # only give message when verbose is TRUE
      if (verbose) {
        insight::format_alert(paste0("Variable `", pattern, "` is not in your data frame :/"))
      }
      # if not safe, always error, no matter what verbose is
    } else {
      insight::format_error(paste0("Variable `", pattern, "` is not in your data frame :/"))
    }
  }

  names(data) <- replace(names(data), names(data) == pattern, replacement)

  data
}


.glue_replacement <- function(pattern, replacement) {
  # this function replaces "glue" tokens into their related
  # real names/values. Currently, following tokens are accepted:
  # - {col}: replacement is the name of the column (indicated in "pattern")
  # - {letter}: replacement is lower-case alphabetically letter, in sequential order
  # - {n}: replacement is the number of the variable out of n, that should be renamed
  out <- rep_len("", length(pattern))

  # for alphabetical letters, we prepare a string if we have more than
  # 26 columns to rename
  if (length(out) > 26) {
    long_letters <- paste0(
      rep.int(letters[1:26], times = ceiling(length(out) / 26)),
      rep(1:ceiling(length(out) / 26), each = 26)
    )
  } else {
    long_letters <- letters[1:26]
  }
  long_letters <- long_letters[seq_len(length(out))]

  for (i in seq_along(out)) {
    # prepare pattern
    column_name <- pattern[i]
    out[i] <- replacement
    # replace first pre-defined token
    out[i] <- gsub(
      "(.*)(\\{col\\})(.*)",
      replacement = paste0("\\1", column_name, "\\3"),
      x = out[i]
    )
    # replace second pre-defined token
    out[i] <- gsub(
      "(.*)(\\{n\\})(.*)",
      replacement = paste0("\\1", i, "\\3"),
      x = out[i]
    )
    # replace third pre-defined token
    out[i] <- gsub(
      "(.*)(\\{letter\\})(.*)",
      replacement = paste0("\\1", long_letters[i], "\\3"),
      x = out[i]
    )
    # extract all non-standard tokens
    matches <- unlist(
      regmatches(out[i], gregexpr("\\{([^}]*)\\}", out[i])),
      use.names = FALSE
    )
    # do we have any additional tokens, i.e. variable names from the environment?
    # users can also specify variable names, where the
    if (length(matches)) {
      # if so, iterate all tokens
      for (token in matches) {
        # evaluate token-object from the environment
        values <- .dynEval(
          str2lang(gsub("\\{(.*)\\}", "\\1", token)),
          ifnotfound = insight::format_error(paste0(
            "The object `", token, "` was not found. Please check if it really exists."
          ))
        )
        # check for correct length
        if (length(values) != length(pattern)) {
          insight::format_error(paste0(
            "The number of values provided in `", token, "` (", length(values),
            " values) do not match the number of columns to rename (",
            length(pattern), " columns)."
          ))
        }
        # replace token with values from the object
        if (length(values)) {
          out[i] <- gsub(token, values[i], out[i], fixed = TRUE)
        }
      }
    }
  }
  out
}


# Row.names ----------------------------------------------------------------

#' @rdname data_rename
#' @export
data_rename_rows <- function(data, rows = NULL) {
  row.names(data) <- rows
  data
}
