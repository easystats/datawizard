# skewness works with data frames

    Code
      skewness(iris[, 1:4])
    Output
      Parameter    | Skewness |    SE
      -------------------------------
      Sepal.Length |    0.315 | 0.196
      Sepal.Width  |    0.319 | 0.196
      Petal.Length |   -0.275 | 0.196
      Petal.Width  |   -0.103 | 0.196

---

    Code
      skewness(iris[, 1:4], iterations = 100)
    Output
      Parameter    | Skewness |    SE
      -------------------------------
      Sepal.Length |    0.315 | 0.126
      Sepal.Width  |    0.319 | 0.175
      Petal.Length |   -0.275 | 0.137
      Petal.Width  |   -0.103 | 0.134

# kurtosis works with data frames

    Code
      kurtosis(iris[, 1:4])
    Output
      Parameter    | Kurtosis |    SE
      -------------------------------
      Sepal.Length |   -0.552 | 0.381
      Sepal.Width  |    0.228 | 0.381
      Petal.Length |   -1.402 | 0.381
      Petal.Width  |   -1.341 | 0.381

---

    Code
      kurtosis(iris[, 1:4], iterations = 100)
    Output
      Parameter    | Kurtosis |    SE
      -------------------------------
      Sepal.Length |   -0.552 | 0.188
      Sepal.Width  |    0.228 | 0.351
      Petal.Length |   -1.402 | 0.167
      Petal.Width  |   -1.341 | 0.115

# skewness works with matrices

    Code
      skewness(as.matrix(iris[, 1:4]))
    Output
      Parameter    | Skewness |    SE
      -------------------------------
      Sepal.Length |    0.315 | 0.196
      Sepal.Width  |    0.319 | 0.196
      Petal.Length |   -0.275 | 0.196
      Petal.Width  |   -0.103 | 0.196

---

    Code
      skewness(as.matrix(iris[, 1:4]), iterations = 100)
    Output
      Parameter    | Skewness |    SE
      -------------------------------
      Sepal.Length |    0.315 | 0.126
      Sepal.Width  |    0.319 | 0.175
      Petal.Length |   -0.275 | 0.137
      Petal.Width  |   -0.103 | 0.134

# kurtosis works with matrices

    Code
      kurtosis(as.matrix(iris[, 1:4]))
    Output
      Parameter    | Kurtosis |    SE
      -------------------------------
      Sepal.Length |   -0.552 | 0.381
      Sepal.Width  |    0.228 | 0.381
      Petal.Length |   -1.402 | 0.381
      Petal.Width  |   -1.341 | 0.381

---

    Code
      kurtosis(as.matrix(iris[, 1:4]), iterations = 100)
    Output
      Parameter    | Kurtosis |    SE
      -------------------------------
      Sepal.Length |   -0.552 | 0.188
      Sepal.Width  |    0.228 | 0.351
      Petal.Length |   -1.402 | 0.167
      Petal.Width  |   -1.341 | 0.115

