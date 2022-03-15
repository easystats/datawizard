data <- tidyr::who

cols = 5:60
names_to = c("diagnosis", "gender", "age")
names_pattern = "new_?(.*)_(.)(.*)"
values_to = "count"

# target
tidyr::who |>
  tidyr::pivot_longer(cols = 5:60,
                      names_to = c("diagnosis", "gender", "age"),
                      names_pattern = "new_?(.*)_(.)(.*)",
                      values_to = "count")


tidyr::who |>
  reshape_longer(cols = 5:60,
                             names_to = c("diagnosis", "gender", "age"),
                             names_pattern = "new_?(.*)_(.)(.*)",
                             values_to = "count")


anscombe |>
  reshape_longer(cols = colnames(anscombe),
                             names_to = c(".value", "set"),
                             names_pattern = "(.)(.)"
  )
