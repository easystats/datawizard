# convert dataframe to numeric

    Code
      convert_data_to_numeric(head(ToothGrowth))
    Output
         len supp.OJ supp.VC dose
      1  4.2       0       1  0.5
      2 11.5       0       1  0.5
      3  7.3       0       1  0.5
      4  5.8       0       1  0.5
      5  6.4       0       1  0.5
      6 10.0       0       1  0.5

---

    Code
      convert_data_to_numeric(head(ToothGrowth), dummy_factors = FALSE)
    Output
         len supp dose
      1  4.2    2  0.5
      2 11.5    2  0.5
      3  7.3    2  0.5
      4  5.8    2  0.5
      5  6.4    2  0.5
      6 10.0    2  0.5

# convert factor to numeric

    Code
      convert_data_to_numeric(f)
    Output
         a c i s t
      1  0 0 0 1 0
      2  0 0 0 0 1
      3  1 0 0 0 0
      4  0 0 0 0 1
      5  0 0 1 0 0
      6  0 0 0 1 0
      7  0 0 0 0 1
      8  0 0 1 0 0
      9  0 1 0 0 0
      10 0 0 0 1 0

