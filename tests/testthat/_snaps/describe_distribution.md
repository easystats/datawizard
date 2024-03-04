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

# describe_distribution formatting

    Code
      format(x)
    Output
      Mean |   SD |  IQR |        Range |  Quartiles | Skewness | Kurtosis |   n | n_Missing
      --------------------------------------------------------------------------------------
      3.06 | 0.44 | 0.52 | [2.00, 4.40] | 2.80, 3.30 |     0.32 |     0.23 | 150 |         0

---

    Code
      format(x)
    Output
      Variable |   Mean |     SD |    IQR |           90% CI |           Range | Skewness | Kurtosis |  n | n_Missing
      ---------------------------------------------------------------------------------------------------------------
      mpg      |  20.09 |   6.03 |   7.53 | [ 18.52,  21.40] | [10.40,  33.90] |     0.67 |    -0.02 | 32 |         0
      cyl      |   6.19 |   1.79 |   4.00 | [  5.75,   6.62] | [ 4.00,   8.00] |    -0.19 |    -1.76 | 32 |         0
      disp     | 230.72 | 123.94 | 221.53 | [195.47, 257.75] | [71.10, 472.00] |     0.42 |    -1.07 | 32 |         0
      hp       | 146.69 |  68.56 |  84.50 | [127.28, 168.31] | [52.00, 335.00] |     0.80 |     0.28 | 32 |         0

---

    Code
      format(x)
    Output
      Variable | Trimmed (10%) |     SD |    IQR |           90% CI |           Range | Skewness | Kurtosis |  n | n_Missing
      ----------------------------------------------------------------------------------------------------------------------
      mpg      |         19.70 |   6.03 |   7.53 | [ 17.92,  21.35] | [10.40,  33.90] |     0.67 |    -0.02 | 32 |         0
      cyl      |          6.23 |   1.79 |   4.00 | [  5.61,   6.77] | [ 4.00,   8.00] |    -0.19 |    -1.76 | 32 |         0
      disp     |        222.52 | 123.94 | 221.53 | [181.61, 258.52] | [71.10, 472.00] |     0.42 |    -1.07 | 32 |         0
      hp       |        141.19 |  68.56 |  84.50 | [125.27, 160.73] | [52.00, 335.00] |     0.80 |     0.28 | 32 |         0

