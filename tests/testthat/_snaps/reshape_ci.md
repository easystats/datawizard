# reshape_ci with single CI level

    Code
      df_reshape
    Output
        Parameter CI_low CI_high  CI
      1    Term 1    0.2     0.5 0.8

# reshape_ci with multiple CI levels

    Code
      reshape_ci(x)
    Output
        Parameter CI_low_0.8 CI_high_0.8 CI_low_0.9 CI_high_0.9
      1    Term 1        0.2         0.5       0.10        0.80
      2    Term 2        0.3         0.6       0.15        0.85

---

    Code
      reshape_ci(reshape_ci(x))
    Output
        Parameter  CI CI_low CI_high
      1    Term 1 0.8   0.20    0.50
      2    Term 1 0.9   0.10    0.80
      3    Term 2 0.8   0.30    0.60
      4    Term 2 0.9   0.15    0.85

