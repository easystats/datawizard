# describe_missing

    Code
      describe_missing(airquality)
    Output
        variable n_columns n_missing n_cells missing_percent complete_percent
      1    Ozone         1        37     153           24.18            75.82
      2  Solar.R         1         7     153            4.58            95.42
      3     Wind         1         0     153            0.00           100.00
      4     Temp         1         0     153            0.00           100.00
      5    Month         1         0     153            0.00           100.00
      6      Day         1         0     153            0.00           100.00
      7    Total         6        44     918            4.79            95.21
        missing_max missing_max_percent all_missing
      1           1              100.00          37
      2           1              100.00           7
      3           0                0.00           0
      4           0                0.00           0
      5           0                0.00           0
      6           0                0.00           0
      7           2               33.33           0

---

    Code
      describe_missing(airquality, vars = list(c("Ozone", "Solar.R", "Wind"), c(
        "Temp", "Month", "Day")))
    Output
        variable n_columns n_missing n_cells missing_percent complete_percent
      1    Ozone         1        37     153           24.18            75.82
      2  Solar.R         1         7     153            4.58            95.42
      3     Wind         1         0     153            0.00           100.00
      4     Temp         1         0     153            0.00           100.00
      5    Month         1         0     153            0.00           100.00
      6      Day         1         0     153            0.00           100.00
      7    Total         6        44     918            4.79            95.21
        missing_max missing_max_percent all_missing
      1           1              100.00          37
      2           1              100.00           7
      3           0                0.00           0
      4           0                0.00           0
      5           0                0.00           0
      6           0                0.00           0
      7           2               33.33           0

---

    Code
      describe_missing(df, scales = c("ID", "scale1", "scale2", "scale3"))
    Output
                   variable n_columns n_missing n_cells missing_percent
      1                  ID         1         7      14           50.00
      2 scale1_Q1:scale1_Q3         3        11      42           26.19
      3 scale2_Q1:scale2_Q3         3        17      42           40.48
      4 scale3_Q1:scale3_Q3         3        10      42           23.81
      5               Total        10        45     140           32.14
        complete_percent missing_max missing_max_percent all_missing
      1            50.00           1                 100           7
      2            73.81           3                 100           3
      3            59.52           3                 100           3
      4            76.19           3                 100           3
      5            67.86          10                 100           2

