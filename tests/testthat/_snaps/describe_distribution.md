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

