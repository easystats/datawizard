# describe_missing

    Code
      describe_missing(airquality)
    Output
              var items na cells na_percent na_max na_max_percent all_na
      1 Ozone:Day     6 44   918       4.79      2          33.33      0

---

    Code
      describe_missing(airquality, vars = list(c("Ozone", "Solar.R", "Wind"), c(
        "Temp", "Month", "Day")))
    Output
               var items na cells na_percent na_max na_max_percent all_na
      1 Ozone:Wind     3 44   459       9.59      2          66.67      0
      2   Temp:Day     3  0   459       0.00      0           0.00      0
      3      Total     6 44   918       4.79      2          33.33      0

---

    Code
      df <- data.frame(ID = c("idz", NA), scale1_Q1 = fun(), scale1_Q2 = fun(),
      scale1_Q3 = fun(), scale2_Q1 = fun(), scale2_Q2 = fun(), scale2_Q3 = fun(),
      scale3_Q1 = fun(), scale3_Q2 = fun(), scale3_Q3 = fun())

---

    Code
      describe_missing(df, scales = c("ID", "scale1", "scale2", "scale3"))
    Output
                        var items na cells na_percent na_max na_max_percent all_na
      1               ID:ID     1  7    14      50.00      1            100      7
      2 scale1_Q1:scale1_Q3     3 11    42      26.19      3            100      3
      3 scale2_Q1:scale2_Q3     3 17    42      40.48      3            100      3
      4 scale3_Q1:scale3_Q3     3 10    42      23.81      3            100      3
      5               Total    10 45   140      32.14     10            100      2

