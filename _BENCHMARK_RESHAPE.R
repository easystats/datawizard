library(tidyr)
library(dplyr)

wide_data <- data.frame(replicate(5, rnorm(10)))
tmp <- list()
for (i in 1:10000) {
  tmp[[i]] <- wide_data
}

tmp <- data.table::rbindlist(tmp) |>
  as_tibble()

ex1 <- bench::mark(
  old = data_to_long(tmp),
  new = new_data_to_long(tmp),
  tidyr = pivot_longer(tmp, cols = everything()),
  iterations = 10
)


ex2 <- bench::mark(
  old = relig_income %>%
    data_to_long(-"religion", names_to = "income", values_to = "count"),
  new = relig_income %>%
    new_data_to_long(-"religion", names_to = "income", values_to = "count"),
  tidyr = relig_income %>%
    pivot_longer(!religion, names_to = "income", values_to = "count"),
  iterations = 100
)

ex3 <- bench::mark(
  old = billboard %>%
    data_to_long(
      cols = starts_with("wk"),
      names_to = "week",
      values_to = "rank"
    ),
  new = billboard %>%
    new_data_to_long(
      cols = starts_with("wk"),
      names_to = "week",
      values_to = "rank"
    ),
  tidyr = billboard %>%
    pivot_longer(
      cols = starts_with("wk"),
      names_to = "week",
      values_to = "rank"
    ),
  iterations = 50
)

ex4 <- bench::mark(
  old = who |>
    data_to_long(
      cols = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_sep = "_",
      values_to = "count"
    ),
  new = who |>
    new_data_to_long(
      cols = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_sep = "_",
      values_to = "count"
    ),
  tidyr = who |>
    pivot_longer(
      cols = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_sep = "_",
      values_to = "count"
    ),
  iterations = 10
)

ex5 <- bench::mark(
  old = who |>
    data_to_long(
      cols = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_pattern = "new_?(.*)_(.)(.*)",
      values_to = "count"
    ),
  new = who |>
    new_data_to_long(
      cols = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_pattern = "new_?(.*)_(.)(.*)",
      values_to = "count"
    ),
  tidyr = who |>
    pivot_longer(
      cols = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_pattern = "new_?(.*)_(.)(.*)",
      values_to = "count"
    ),
  iterations = 10
)


