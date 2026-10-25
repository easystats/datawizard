# describe_missing

    Code
      describe_missing(airquality2)
    Output
        variable n_missing missing_percent complete_percent
      1  Solar.R         7            4.58            95.42
      2     Wind         0            0.00           100.00
      3     Temp         0            0.00           100.00
      4    Month         0            0.00           100.00
      5      Day         0            0.00           100.00
      6    Ozone        37           24.18            75.82
      7    Total        44            4.79            95.21

---

    Code
      describe_missing(airquality2, sort = TRUE)
    Output
         variable n_missing missing_percent complete_percent
      6     Ozone        37           24.18            75.82
      1   Solar.R         7            4.58            95.42
      2      Wind         0            0.00           100.00
      3      Temp         0            0.00           100.00
      4     Month         0            0.00           100.00
      5       Day         0            0.00           100.00
      11    Total        44            4.79            95.21

---

    Code
      describe_missing(airquality2, select = "Ozone:Temp")
    Output
        variable n_missing missing_percent complete_percent
      1    Ozone        37           24.18            75.82
      2      Day         0            0.00           100.00
      3    Month         0            0.00           100.00
      4     Temp         0            0.00           100.00
      5    Total        37            6.05            93.95

---

    Code
      describe_missing(airquality2, exclude = "Ozone:Temp")
    Output
        variable n_missing missing_percent complete_percent
      1  Solar.R         7            4.58            95.42
      2     Wind         0            0.00           100.00
      3    Total         7            2.29            97.71

---

    Code
      describe_missing(df_long, select = -c(1, 3), by = "dimension")
    Output
             variable n_missing missing_percent complete_percent
      1 agreeableness        10           23.81            76.19
      2  extroversion        17           40.48            59.52
      3      openness        11           26.19            73.81
      4         Total        38           15.08            84.92

