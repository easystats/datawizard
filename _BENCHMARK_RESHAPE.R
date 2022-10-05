library(tidyr)
library(dplyr)


### DATA_TO_LONG ==========================================

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




### DATA_TO_WIDE ==========================================

ex1 <- bench::mark(
  old = fish_encounters %>%
    data_to_wide(
      names_from = "station",
      values_from = "seen",
      values_fill = 0
    ),
  new = fish_encounters %>%
    new_data_to_wide(
      names_from = "station",
      values_from = "seen",
      values_fill = 0
    ),
  tidyr = fish_encounters %>%
    pivot_wider(
      names_from = "station",
      values_from = "seen",
      values_fill = 0
    ),
  iterations = 100
)




production <- expand_grid(
  product = letters,
  country = paste0(letters, "I"),
  year = 2000:2025
) %>%
  mutate(production = rnorm(nrow(.)))


ex2 <- bench::mark(
  old = production %>%
    data_to_wide(
      names_from = c("product", "country"),
      values_from = "production"
    ),
  new = production %>%
    new_data_to_wide(
      names_from = c("product", "country"),
      values_from = "production"
    ),
  tidyr = production %>%
    pivot_wider(
      names_from = c(product, country),
      values_from = production
    ),
  iterations = 10
)


ex3 <- bench::mark(
  old = production %>%
    data_to_wide(
      names_from = c("product", "country"),
      values_from = "production",
      names_glue = "prod_{product}_{country}"
    ),
  new = production %>%
    new_data_to_wide(
      names_from = c("product", "country"),
      values_from = "production",
      names_glue = "prod_{product}_{country}"
    ),
  tidyr = production %>%
    pivot_wider(
      names_from = c(product, country),
      values_from = production,
      names_glue = "prod_{product}_{country}"
    ),
  iterations = 10
)


tmp <- list()
for (i in 1:1000) {
  tmp[[i]] <- us_rent_income
}

tmp <- data.table::rbindlist(tmp) |>
  as_tibble()
tmp$GEOID <- rep(1:52000, each = 2)
tmp$NAME <- as.character(rep(1:52000, each = 2))

ex4 <- bench::mark(
  old = tmp %>%
    data_to_wide(
      names_from = "variable",
      values_from = c("estimate", "moe")
    ),
  new = tmp %>%
    new_data_to_wide(
      names_from = "variable",
      values_from = c("estimate", "moe")
    ),
  tidyr = tmp %>%
    pivot_wider(
      names_from = "variable",
      values_from = c("estimate", "moe")
    ),
  iterations = 10
)

# SLOW ============

set.seed(123)
contacts <- tibble(
  id = rep(1:500000, each = 2),
  field = rep(c("a", "b"), 500000),
  value = sample(letters, 1000000, replace = TRUE)
)
# 1M rows

ex5 <- bench::mark(
  old = contacts %>%
    data_to_wide(names_from = "field", values_from = "value"),
  new = contacts %>%
    new_data_to_wide(names_from = "field", values_from = "value"),
  tidyr = contacts %>%
    tidyr::pivot_wider(names_from = field, values_from = value),
  iterations = 10
)



