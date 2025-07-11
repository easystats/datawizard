# describe_distribution - factor

    Code
      describe_distribution(factor(substring("statistics", 1:10, 1:10)))
    Output
      Mean | SD |  Range | Skewness | Kurtosis |  n | n_Missing
      ---------------------------------------------------------
           |    | [a, t] |    -0.77 |    -0.13 | 10 |         0

# describe_distribution - character

    Code
      describe_distribution(as.character(ToothGrowth$supp))
    Output
      Mean | SD |    Range | Skewness | Kurtosis |  n | n_Missing
      -----------------------------------------------------------
           |    | [VC, OJ] |        0 |    -2.07 | 60 |         0

# describe_distribution - grouped df

    Code
      out
    Output
      Species    |     Variable | Mean |   SD |  IQR |        Range | Skewness
      ------------------------------------------------------------------------
      setosa     | Petal.Length | 1.46 | 0.17 | 0.20 | [1.00, 1.90] |     0.11
      setosa     |  Petal.Width | 0.25 | 0.11 | 0.10 | [0.10, 0.60] |     1.25
      versicolor | Petal.Length | 4.26 | 0.47 | 0.60 | [3.00, 5.10] |    -0.61
      versicolor |  Petal.Width | 1.33 | 0.20 | 0.30 | [1.00, 1.80] |    -0.03
      virginica  | Petal.Length | 5.55 | 0.55 | 0.80 | [4.50, 6.90] |     0.55
      virginica  |  Petal.Width | 2.03 | 0.27 | 0.50 | [1.40, 2.50] |    -0.13
      
      Species    | Kurtosis |  n | n_Missing
      --------------------------------------
      setosa     |     1.02 | 50 |         0
      setosa     |     1.72 | 50 |         0
      versicolor |     0.05 | 50 |         0
      versicolor |    -0.41 | 50 |         0
      virginica  |    -0.15 | 50 |         0
      virginica  |    -0.60 | 50 |         0

# describe_distribution - grouped df and multiple groups

    Code
      describe_distribution(x)
    Output
      grp1 | grp2 | Variable |  Mean |    SD |   IQR |          Range | Skewness
      --------------------------------------------------------------------------
      a    |    a |   values | 10.00 |  6.48 | 12.00 |  [1.00, 19.00] |     0.00
      b    |    a |   values | 13.86 | 10.92 | 21.00 |  [1.00, 28.00] |     0.23
      c    |    a |   values | 20.50 |  5.61 | 10.50 | [13.00, 28.00] |     0.00
      a    |    b |   values | 11.00 |  6.48 | 12.00 |  [2.00, 20.00] |     0.00
      b    |    b |   values | 15.50 | 11.81 | 22.50 |  [2.00, 29.00] |     0.00
      c    |    b |   values | 20.00 |  6.48 | 12.00 | [11.00, 29.00] |     0.00
      a    |    c |   values | 10.50 |  5.61 | 10.50 |  [3.00, 18.00] |     0.00
      b    |    c |   values | 17.14 | 10.92 | 21.00 |  [3.00, 30.00] |    -0.23
      c    |    c |   values | 21.00 |  6.48 | 12.00 | [12.00, 30.00] |     0.00
      
      grp1 | Kurtosis | n | n_Missing
      -------------------------------
      a    |    -1.20 | 7 |         0
      b    |    -2.14 | 7 |         0
      c    |    -1.20 | 6 |         0
      a    |    -1.20 | 7 |         0
      b    |    -2.76 | 6 |         0
      c    |    -1.20 | 7 |         0
      a    |    -1.20 | 6 |         0
      b    |    -2.14 | 7 |         0
      c    |    -1.20 | 7 |         0

# describe_distribution formatting

    Code
      format(x)
    Output
      Mean |   SD |  IQR |        Range |  Quartiles | Skewness | Kurtosis |   n | n_Missing
      --------------------------------------------------------------------------------------
      3.06 | 0.44 | 0.52 | [2.00, 4.40] | 2.80, 3.30 |     0.32 |     0.23 | 150 |         0

# (multiple) centralities with CIs

    Code
      print(out, table_width = Inf)
    Output
      Median | 95% CI (Median) |  MAD | Mean | 95% CI (Mean) |   SD |  MAP | 95% CI (MAP) |  IQR |        Range | Skewness | Kurtosis |   n | n_Missing
      -------------------------------------------------------------------------------------------------------------------------------------------------
           3 |    [3.00, 3.10] | 0.44 | 3.06 |  [3.00, 3.13] | 0.44 | 3.00 | [2.94, 3.07] | 0.52 | [2.00, 4.40] |     0.32 |     0.23 | 150 |         0

---

    Code
      print(out, table_width = Inf)
    Output
      Mean | 95% CI (Mean) |   SD |  IQR |        Range | Skewness | Kurtosis |   n | n_Missing
      -----------------------------------------------------------------------------------------
      3.06 |  [3.00, 3.13] | 0.44 | 0.52 | [2.00, 4.40] |     0.32 |     0.23 | 150 |         0

---

    Code
      print(out, table_width = Inf)
    Output
      Median | 95% CI (Median) |  MAD |  MAP | 95% CI (MAP) |  IQR |        Range | Skewness | Kurtosis |   n | n_Missing
      -------------------------------------------------------------------------------------------------------------------
           3 |    [3.00, 3.10] | 0.44 | 3.00 | [2.91, 3.09] | 0.52 | [2.00, 4.40] |     0.32 |     0.23 | 150 |         0

# display() method exports to markdown

    Code
      display(out)
    Output
      
      
      |Variable     | Mean |   SD |  IQR |        Range | Skewness | Kurtosis |   n | n_Missing |
      |:------------|:----:|:----:|:----:|:------------:|:--------:|:--------:|:---:|:---------:|
      |Sepal.Length | 5.84 | 0.83 | 1.30 | (4.30, 7.90) |     0.31 |    -0.55 | 150 |         0 |
      |Sepal.Width  | 3.06 | 0.44 | 0.52 | (2.00, 4.40) |     0.32 |     0.23 | 150 |         0 |
      |Petal.Length | 3.76 | 1.77 | 3.52 | (1.00, 6.90) |    -0.27 |    -1.40 | 150 |         0 |
      |Petal.Width  | 1.20 | 0.76 | 1.50 | (0.10, 2.50) |    -0.10 |    -1.34 | 150 |         0 |

