# rescale works as expected

    Code
      head(rescale(iris, to = c(0, 1)))
    Message <simpleMessage>
      Variables of class 'factor' can't be rescaled and remain unchanged.
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1   0.22222222   0.6250000   0.06779661  0.04166667  setosa
      2   0.16666667   0.4166667   0.06779661  0.04166667  setosa
      3   0.11111111   0.5000000   0.05084746  0.04166667  setosa
      4   0.08333333   0.4583333   0.08474576  0.04166667  setosa
      5   0.19444444   0.6666667   0.06779661  0.04166667  setosa
      6   0.30555556   0.7916667   0.11864407  0.12500000  setosa

---

    Code
      head(rescale(iris, to = c(0, 1), select = "Sepal.Length"))
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1   0.22222222         3.5          1.4         0.2  setosa
      2   0.16666667         3.0          1.4         0.2  setosa
      3   0.11111111         3.2          1.3         0.2  setosa
      4   0.08333333         3.1          1.5         0.2  setosa
      5   0.19444444         3.6          1.4         0.2  setosa
      6   0.30555556         3.9          1.7         0.4  setosa

---

    Code
      head(rescale(iris, to = list(Sepal.Length = c(0, 1), Petal.Length = c(-1, 0))))
    Message <simpleMessage>
      Variables of class 'factor' can't be rescaled and remain unchanged.
    Output
        Sepal.Length Sepal.Width Petal.Length Petal.Width Species
      1   0.22222222         3.5   -0.9322034         0.2  setosa
      2   0.16666667         3.0   -0.9322034         0.2  setosa
      3   0.11111111         3.2   -0.9491525         0.2  setosa
      4   0.08333333         3.1   -0.9152542         0.2  setosa
      5   0.19444444         3.6   -0.9322034         0.2  setosa
      6   0.30555556         3.9   -0.8813559         0.4  setosa

