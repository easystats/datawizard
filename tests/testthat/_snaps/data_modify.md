# data_modify message about recycling values

    Code
      head(data_modify(iris, Sepal.Width = 1))
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1          5.1           1          1.4         0.2  setosa
      2          4.9           1          1.4         0.2  setosa
      3          4.7           1          1.3         0.2  setosa
      4          4.6           1          1.5         0.2  setosa
      5          5.0           1          1.4         0.2  setosa
      6          5.4           1          1.7         0.4  setosa

---

    Code
      head(data_modify(iris, Sepal.Width = 1:2))
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1          5.1           1          1.4         0.2  setosa
      2          4.9           2          1.4         0.2  setosa
      3          4.7           1          1.3         0.2  setosa
      4          4.6           2          1.5         0.2  setosa
      5          5.0           1          1.4         0.2  setosa
      6          5.4           2          1.7         0.4  setosa

---

    Code
      head(data_modify(iris, Petal.Length = 1, Sepal.Width = 1))
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1          5.1           1            1         0.2  setosa
      2          4.9           1            1         0.2  setosa
      3          4.7           1            1         0.2  setosa
      4          4.6           1            1         0.2  setosa
      5          5.0           1            1         0.2  setosa
      6          5.4           1            1         0.4  setosa

---

    Code
      head(data_modify(iris, Petal.Length = 1, Sepal.Width = 1:2))
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1          5.1           1            1         0.2  setosa
      2          4.9           2            1         0.2  setosa
      3          4.7           1            1         0.2  setosa
      4          4.6           2            1         0.2  setosa
      5          5.0           1            1         0.2  setosa
      6          5.4           2            1         0.4  setosa

---

    Code
      head(data_modify(iris, Petal.Length = 2, Sepal.Width = 2))
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1          5.1           2            2         0.2  setosa
      2          4.9           2            2         0.2  setosa
      3          4.7           2            2         0.2  setosa
      4          4.6           2            2         0.2  setosa
      5          5.0           2            2         0.2  setosa
      6          5.4           2            2         0.4  setosa

# data_modify message about modified variables

    Code
      head(data_modify(iris, Sepal.Width = 2 * Sepal.Width))
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1          5.1         7.0          1.4         0.2  setosa
      2          4.9         6.0          1.4         0.2  setosa
      3          4.7         6.4          1.3         0.2  setosa
      4          4.6         6.2          1.5         0.2  setosa
      5          5.0         7.2          1.4         0.2  setosa
      6          5.4         7.8          1.7         0.4  setosa

---

    Code
      head(data_modify(iris, Petal.Length = Sepal.Length, Sepal.Width = Petal.Width))
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1          5.1         0.2          5.1         0.2  setosa
      2          4.9         0.2          4.9         0.2  setosa
      3          4.7         0.2          4.7         0.2  setosa
      4          4.6         0.2          4.6         0.2  setosa
      5          5.0         0.2          5.0         0.2  setosa
      6          5.4         0.4          5.4         0.4  setosa

