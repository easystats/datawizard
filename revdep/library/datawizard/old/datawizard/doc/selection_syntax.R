## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

pkgs <- c(
  "datawizard",
  "dplyr",
  "htmltools"
)

if (!all(sapply(pkgs, requireNamespace, quietly = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ----load, echo=FALSE, message=FALSE------------------------------------------
library(datawizard)
library(dplyr)
library(htmltools)

set.seed(123)
iris <- iris[sample(nrow(iris), 10), ]
row.names(iris) <- NULL

row <- function(...) {
  div(
    class = "custom_note",
    ...
  )
}

## -----------------------------------------------------------------------------
data_select(iris, c("Sepal.Length", "Petal.Width"))

## -----------------------------------------------------------------------------
iris %>%
  group_by(Species) %>%
  standardise(Petal.Length) %>%
  ungroup()

## -----------------------------------------------------------------------------
data_select(iris, c(1, 2, 5))

## -----------------------------------------------------------------------------
data_select(iris, is.numeric)

## -----------------------------------------------------------------------------
my_function <- function(i) {
  is.numeric(i) && mean(i, na.rm = TRUE) > 3.5
}

data_select(iris, my_function)

## -----------------------------------------------------------------------------
data_select(iris, starts_with("Sep", "Peta"))

data_select(iris, ends_with("dth", "ies"))

data_select(iris, contains("pal", "ec"))

data_select(iris, regex("^Sep|ies"))

## ----echo=FALSE---------------------------------------------------------------
row("Note: these functions are not exported by `datawizard` but are detected and
applied internally. This means that they won't be detected by autocompletion
when we write them.")

## ----echo=FALSE---------------------------------------------------------------
row("Note #2: because these functions are not exported, they will not create
conflicts with the ones that come from the `tidyverse` and that have the same name.
So we can still use `dplyr` and its friends, it won't change anything for selection
in `datawizard` functions!")

## -----------------------------------------------------------------------------
data_select(iris, -c("Sepal.Length", "Petal.Width"))

data_select(iris, -starts_with("Sep", "Peta"))

data_select(iris, -is.numeric)

## -----------------------------------------------------------------------------
data_select(iris, -(1:2))

## -----------------------------------------------------------------------------
data_select(iris, exclude = c("Sepal.Length", "Petal.Width"))

data_select(iris, exclude = starts_with("Sep", "Peta"))

## -----------------------------------------------------------------------------
my_function <- function(data, selection) {
  find_columns(data, select = selection)
}
my_function(iris, c("Sepal.Length"))
my_function(iris, starts_with("Sep"))

my_function_2 <- function(data, pattern) {
  find_columns(data, select = starts_with(pattern))
}
my_function_2(iris, "Sep")

## -----------------------------------------------------------------------------
new_iris <- iris
for (i in c("Sep", "Pet")) {
  new_iris <- new_iris %>%
    data_relocate(select = starts_with(i), after = -1)
}
new_iris

## -----------------------------------------------------------------------------
data_select(iris, c("sepal.length", "petal.width"), ignore_case = TRUE)

data_select(iris, ~ Sepal.length + petal.Width, ignore_case = TRUE)

data_select(iris, starts_with("sep", "peta"), ignore_case = TRUE)

## -----------------------------------------------------------------------------
data_select(iris, ~ Sepal.Length + Petal.Width)

