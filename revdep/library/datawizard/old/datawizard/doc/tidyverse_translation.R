## ----message=FALSE, warning=FALSE, include=FALSE, eval = TRUE-----------------
library(knitr)
options(knitr.kable.NA = "")
knitr::opts_chunk$set(
  eval = FALSE,
  message = FALSE,
  warning = FALSE,
  dpi = 300
)

pkgs <- c(
  "dplyr",
  "datawizard",
  "tidyr"
)

# since we explicitely put eval = TRUE for some chunks, we can't rely on
# knitr::opts_chunk$set(eval = FALSE) at the beginning of the script. So we make
# a logical that is FALSE only if deps are not installed (cf easystats/easystats#317)
evaluate_chunk <- TRUE

if (!all(sapply(pkgs, requireNamespace, quietly = TRUE))) {
  evaluate_chunk <- FALSE
}

## ---- eval = evaluate_chunk---------------------------------------------------
library(dplyr)
library(tidyr)
library(datawizard)

## ----filter, class.source = "datawizard"--------------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_filter(skin_color == "light" &
#      eye_color == "brown")

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    filter(
#      skin_color == "light",
#      eye_color == "brown"
#    )

## ----filter, eval = evaluate_chunk, echo = FALSE------------------------------
# ---------- datawizard -----------
starwars %>%
  data_filter(skin_color == "light" &
    eye_color == "brown")

## ---- echo = FALSE, eval = evaluate_chunk-------------------------------------
starwars <- head(starwars)

## ----select1, class.source = "datawizard"-------------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_select(select = c("hair_color", "skin_color", "eye_color"))

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    select(hair_color, skin_color, eye_color)

## ----select1, eval = evaluate_chunk, echo = FALSE-----------------------------
# ---------- datawizard -----------
starwars %>%
  data_select(select = c("hair_color", "skin_color", "eye_color"))

## ----select2, class.source = "datawizard"-------------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_select(select = -ends_with("color"))

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    select(-ends_with("color"))

## ----select2, eval = evaluate_chunk, echo = FALSE-----------------------------
# ---------- datawizard -----------
starwars %>%
  data_select(select = -ends_with("color"))

## ----select3, class.source = "datawizard"-------------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_select(select = -hair_color:eye_color)

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    select(!(hair_color:eye_color))

## ----select3, eval = evaluate_chunk, echo = FALSE-----------------------------
# ---------- datawizard -----------
starwars %>%
  data_select(select = -hair_color:eye_color)

## ----select4, class.source = "datawizard"-------------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_select(exclude = regex("color$"))

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    select(-contains("color$"))

## ----select4, eval = evaluate_chunk, echo = FALSE-----------------------------
# ---------- datawizard -----------
starwars %>%
  data_select(exclude = regex("color$"))

## ----select5, class.source = "datawizard"-------------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_select(select = is.numeric)

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    select(where(is.numeric))

## ----select5, eval = evaluate_chunk, echo = FALSE-----------------------------
# ---------- datawizard -----------
starwars %>%
  data_select(select = is.numeric)

## ----arrange1, class.source = "datawizard"------------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_arrange(c("hair_color", "height"))

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    arrange(hair_color, height)

## ----arrange1, eval = evaluate_chunk, echo = FALSE----------------------------
# ---------- datawizard -----------
starwars %>%
  data_arrange(c("hair_color", "height"))

## ----arrange2, class.source = "datawizard"------------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_arrange(c("-hair_color", "-height"))

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    arrange(desc(hair_color), -height)

## ----arrange2, eval = evaluate_chunk, echo = FALSE----------------------------
# ---------- datawizard -----------
starwars %>%
  data_arrange(c("-hair_color", "-height"))

## ----extract1, class.source = "datawizard"------------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_extract(gender)

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    pull(gender)

## ----extract1, eval = evaluate_chunk, echo = FALSE----------------------------
# ---------- datawizard -----------
starwars %>%
  data_extract(gender)

## ----eval = evaluate_chunk----------------------------------------------------
starwars %>%
  data_extract(select = contains("color"))

## ----rename1, class.source = "datawizard"-------------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_rename(
#      pattern = c("sex", "hair_color"),
#      replacement = c("Sex", "Hair Color")
#    )

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    rename(
#      Sex = sex,
#      "Hair Color" = hair_color
#    )

## ----rename1, eval = evaluate_chunk, echo = FALSE-----------------------------
# ---------- datawizard -----------
starwars %>%
  data_rename(
    pattern = c("sex", "hair_color"),
    replacement = c("Sex", "Hair Color")
  )

## ----rename2------------------------------------------------------------------
#  to_rename <- names(starwars)
#  
#  starwars %>%
#    data_rename(
#      pattern = to_rename,
#      replacement = tools::toTitleCase(gsub("_", " ", to_rename))
#    )

## ----rename2, eval = evaluate_chunk, echo = FALSE-----------------------------
to_rename <- names(starwars)

starwars %>%
  data_rename(
    pattern = to_rename,
    replacement = tools::toTitleCase(gsub("_", " ", to_rename))
  )

## ----rename3------------------------------------------------------------------
#  starwars %>%
#    data_addprefix(
#      pattern = "OLD.",
#      select = contains("color")
#    ) %>%
#    data_addsuffix(
#      pattern = ".NEW",
#      select = -contains("color")
#    )

## ----rename3, eval = evaluate_chunk, echo = FALSE-----------------------------
starwars %>%
  data_addprefix(
    pattern = "OLD.",
    select = contains("color")
  ) %>%
  data_addsuffix(
    pattern = ".NEW",
    select = -contains("color")
  )

## ----relocate1, class.source = "datawizard"-----------------------------------
#  # ---------- datawizard -----------
#  starwars %>%
#    data_relocate(sex:homeworld, before = "height")

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  starwars %>%
#    relocate(sex:homeworld, .before = height)

## ----relocate1, eval = evaluate_chunk, echo = FALSE---------------------------
# ---------- datawizard -----------
starwars %>%
  data_relocate(sex:homeworld, before = "height")

## ----eval = evaluate_chunk----------------------------------------------------
# ---------- datawizard -----------
starwars %>%
  data_relocate(sex:homeworld, after = -1)

## ----eval = evaluate_chunk----------------------------------------------------
relig_income

## ----pivot1, class.source = "datawizard"--------------------------------------
#  # ---------- datawizard -----------
#  relig_income %>%
#    data_to_long(
#      -religion,
#      names_to = "income",
#      values_to = "count"
#    )

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  relig_income %>%
#    pivot_longer(
#      !religion,
#      names_to = "income",
#      values_to = "count"
#    )

## ----pivot1, eval = evaluate_chunk, echo = FALSE------------------------------
# ---------- datawizard -----------
relig_income %>%
  data_to_long(
    -religion,
    names_to = "income",
    values_to = "count"
  )

## ----eval = evaluate_chunk----------------------------------------------------
billboard

## ----pivot2, class.source = "datawizard"--------------------------------------
#  # ---------- datawizard -----------
#  billboard %>%
#    data_to_long(
#      cols = starts_with("wk"),
#      names_to = "week",
#      values_to = "rank",
#      values_drop_na = TRUE
#    )

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  billboard %>%
#    pivot_longer(
#      cols = starts_with("wk"),
#      names_to = "week",
#      values_to = "rank",
#      values_drop_na = TRUE
#    )

## ----pivot2, eval = evaluate_chunk, echo = FALSE------------------------------
# ---------- datawizard -----------
billboard %>%
  data_to_long(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )

## ----eval = evaluate_chunk----------------------------------------------------
fish_encounters

## ----pivot3, class.source = "datawizard"--------------------------------------
#  # ---------- datawizard -----------
#  fish_encounters %>%
#    data_to_wide(
#      names_from = "station",
#      values_from = "seen",
#      values_fill = 0
#    )

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  fish_encounters %>%
#    pivot_wider(
#      names_from = station,
#      values_from = seen,
#      values_fill = 0
#    )

## ----pivot3, eval = evaluate_chunk, echo = FALSE------------------------------
# ---------- datawizard -----------
fish_encounters %>%
  data_to_wide(
    names_from = "station",
    values_from = "seen",
    values_fill = 0
  )

## ----eval = evaluate_chunk----------------------------------------------------
band_members

## ----eval = evaluate_chunk----------------------------------------------------
band_instruments

## ----join1, class.source = "datawizard"---------------------------------------
#  # ---------- datawizard -----------
#  band_members %>%
#    data_join(band_instruments, join = "full")

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  band_members %>%
#    full_join(band_instruments)

## ----join1, eval = evaluate_chunk, echo = FALSE-------------------------------
# ---------- datawizard -----------
band_members %>%
  data_join(band_instruments, join = "full")

## ----join2, class.source = "datawizard"---------------------------------------
#  # ---------- datawizard -----------
#  band_members %>%
#    data_join(band_instruments, join = "left")

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  band_members %>%
#    left_join(band_instruments)

## ----join2, eval = evaluate_chunk, echo = FALSE-------------------------------
# ---------- datawizard -----------
band_members %>%
  data_join(band_instruments, join = "left")

## ----join3, class.source = "datawizard"---------------------------------------
#  # ---------- datawizard -----------
#  band_members %>%
#    data_join(band_instruments, join = "right")

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  band_members %>%
#    right_join(band_instruments)

## ----join3, eval = evaluate_chunk, echo = FALSE-------------------------------
# ---------- datawizard -----------
band_members %>%
  data_join(band_instruments, join = "right")

## ----join4, class.source = "datawizard"---------------------------------------
#  # ---------- datawizard -----------
#  band_members %>%
#    data_join(band_instruments, join = "inner")

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  band_members %>%
#    inner_join(band_instruments)

## ----join4, eval = evaluate_chunk, echo = FALSE-------------------------------
# ---------- datawizard -----------
band_members %>%
  data_join(band_instruments, join = "inner")

## ----eval = evaluate_chunk----------------------------------------------------
mtcars <- head(mtcars)
mtcars

mtcars2 <- mtcars %>%
  rownames_as_column(var = "model")

mtcars2

mtcars2 %>%
  column_as_rownames(var = "model")

## ----eval = evaluate_chunk----------------------------------------------------
x <- data.frame(
  X_1 = c(NA, "Title", 1:3),
  X_2 = c(NA, "Title2", 4:6)
)
x
x2 <- x %>%
  row_to_colnames(row = 2)
x2

x2 %>%
  colnames_to_row()

## ----glimpse, class.source = "datawizard"-------------------------------------
#  # ---------- datawizard -----------
#  data_peek(iris)

## ---- class.source = "tidyverse"----------------------------------------------
#  # ---------- tidyverse -----------
#  glimpse(iris)

## ----glimpse, eval = evaluate_chunk, echo = FALSE-----------------------------
# ---------- datawizard -----------
data_peek(iris)

