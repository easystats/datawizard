## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library(insight)
x <- iris[1:3, c(1, 2, 5)]

# the table as "readable" output
export_table(x)

# see the underlying string
unclass(export_table(x))

## -----------------------------------------------------------------------------
# a simple caption
export_table(x, caption = "Title")

# we use a new object, so "x" has no attributes yet
out <- x
attr(out, "table_caption") <- "Another title"
export_table(out)

## -----------------------------------------------------------------------------
# A red caption
export_table(x, caption = c("# Title", "red"))

# same for attribute
out <- x
attr(out, "table_caption") <- c("*A green title*", "green")
export_table(out)

## ----echo=FALSE, out.width="50%"----------------------------------------------
knitr::include_graphics("export_table.png", dpi = 72)

## -----------------------------------------------------------------------------
# colored caption, simple footer
export_table(
  x,
  caption = c("# Title", "red"),
  footer = "Footer line"
)

# as attribute
out <- x
attr(out, "table_caption") <- c("*A green title*", "green")
attr(out, "table_footer") <- "A simple footer"
export_table(out)

## -----------------------------------------------------------------------------
# colored caption and footer
export_table(
  x,
  caption = c("# Title", "red"),
  footer = c("Footer line in blue", "blue")
)

# as attribute
out <- x
attr(out, "table_caption") <- c("*A green title*", "green")
attr(out, "table_footer") <- c("Footer line in blue", "blue")
export_table(out)

## -----------------------------------------------------------------------------
# colored caption, subtitle and footer
export_table(
  x,
  caption = c("# Title", "red"),
  subtitle = c("\n   A subtitle in yellow", "yellow"),
  footer = c("Footer line in blue", "blue")
)

# as attribute
out <- x
attr(out, "table_caption") <- c("*A green title*", "green")
attr(out, "table_subtitle") <- c("\nA yellow subtitle", "yellow")
attr(out, "table_footer") <- c("Footer line in blue", "blue")
export_table(out)

## -----------------------------------------------------------------------------
x <- list(
  data.frame(iris[1:3, c(1, 2, 5)]),
  data.frame(iris[51:53, c(1, 3, 5)]),
  data.frame(iris[111:113, c(1, 4, 5)])
)

# three different tables
export_table(x)

## -----------------------------------------------------------------------------
# one caption for each table
export_table(x, caption = list("Table 1", "Table 2", "Table 3"))

# add attribute to *each* data frame
out <- x
for (i in seq_along(out)) {
  attr(out[[i]], "table_caption") <- paste("Table", i)
}
export_table(out)

## -----------------------------------------------------------------------------
# add captions and footers for each table
export_table(
  x,
  caption = list("Table 1", "Table 2", "Table 3"),
  footer = list("Footer 1\n\n", "Footer 2\n\n", "Footer 3\n\n")
)

out <- x
for (i in seq_along(out)) {
  attr(out[[i]], "table_caption") <- paste("Table", i)
  attr(out[[i]], "table_footer") <- paste("Footer", i, "\n\n")
}
export_table(out)

## -----------------------------------------------------------------------------
# Colored table captions and multiple footers per table
export_table(
  x,
  caption = list(
    c("Red Table 1", "red"),
    c("Blue Table 2", "bue"),
    c("Green Table 3", "green")
  ),
  footer = list(
    list(c("Footer line 1\n", "green"), c("Second line\n\n", "red")),
    list(c("Footer line A\n", "blue"), c("Second line\n\n", "green")),
    list(c("Footer line I\n", "yellow"), c("Second line\n\n", "blue"))
  )
)

