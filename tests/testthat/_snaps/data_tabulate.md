# data_tabulate print

    Code
      data_tabulate(efc$e42dep)
    Output
      elder's dependency (efc$e42dep) <categorical>
      # total N=100 valid N=97
      
      Value |  N | Raw % | Valid % | Cumulative %
      ------+----+-------+---------+-------------
      1     |  2 |  2.00 |    2.06 |         2.06
      2     |  4 |  4.00 |    4.12 |         6.19
      3     | 28 | 28.00 |   28.87 |        35.05
      4     | 63 | 63.00 |   64.95 |       100.00
      <NA>  |  3 |  3.00 |    <NA> |         <NA>

# data_tabulate print multiple

    Code
      data_tabulate(efc, c("c172code", "e16sex"))
    Output
      carer's level of education (c172code) <numeric>
      # total N=100 valid N=90
      
      Value |  N | Raw % | Valid % | Cumulative %
      ------+----+-------+---------+-------------
      1     |  8 |  8.00 |    8.89 |         8.89
      2     | 66 | 66.00 |   73.33 |        82.22
      3     | 16 | 16.00 |   17.78 |       100.00
      <NA>  | 10 | 10.00 |    <NA> |         <NA>
      
      elder's gender (e16sex) <numeric>
      # total N=100 valid N=100
      
      Value |  N | Raw % | Valid % | Cumulative %
      ------+----+-------+---------+-------------
      1     | 46 | 46.00 |   46.00 |        46.00
      2     | 54 | 54.00 |   54.00 |       100.00
      <NA>  |  0 |  0.00 |    <NA> |         <NA>

# data_tabulate big numbers

    Code
      data_tabulate(x)
    Output
      x <integer>
      # total N=10,000,000 valid N=10,000,000
      
      Value |         N | Raw % | Valid % | Cumulative %
      ------+-----------+-------+---------+-------------
      1     | 1,998,318 | 19.98 |   19.98 |        19.98
      2     | 1,998,338 | 19.98 |   19.98 |        39.97
      3     | 2,001,814 | 20.02 |   20.02 |        59.98
      4     | 1,999,423 | 19.99 |   19.99 |        79.98
      5     | 2,002,107 | 20.02 |   20.02 |       100.00
      <NA>  |         0 |  0.00 |    <NA> |         <NA>

---

    Code
      print(data_tabulate(x), big_mark = "-")
    Output
      x <integer>
      # total N=10-000-000 valid N=10-000-000
      
      Value |         N | Raw % | Valid % | Cumulative %
      ------+-----------+-------+---------+-------------
      1     | 1-998-318 | 19.98 |   19.98 |        19.98
      2     | 1-998-338 | 19.98 |   19.98 |        39.97
      3     | 2-001-814 | 20.02 |   20.02 |        59.98
      4     | 1-999-423 | 19.99 |   19.99 |        79.98
      5     | 2-002-107 | 20.02 |   20.02 |       100.00
      <NA>  |         0 |  0.00 |    <NA> |         <NA>

# data_tabulate print multiple, collapse

    Code
      data_tabulate(efc, c("c172code", "e16sex"), collapse = TRUE)
    Output
      # Frequency Table
      
      Variable | Value |  N | Raw % | Valid % | Cumulative %
      ---------+-------+----+-------+---------+-------------
      c172code |     1 |  8 |  8.00 |    8.89 |         8.89
               |     2 | 66 | 66.00 |   73.33 |        82.22
               |     3 | 16 | 16.00 |   17.78 |       100.00
               |  <NA> | 10 | 10.00 |    <NA> |         <NA>
      ---------+-------+----+-------+---------+-------------
      e16sex   |     1 | 46 | 46.00 |   46.00 |        46.00
               |     2 | 54 | 54.00 |   54.00 |       100.00
               |  <NA> |  0 |  0.00 |    <NA> |         <NA>
      ------------------------------------------------------

# data_tabulate print grouped data

    Code
      data_tabulate(poorman::group_by(efc, e16sex), "c172code")
    Output
      carer's level of education (c172code) <numeric>
      Grouped by e16sex (1)
      # total N=46 valid N=41
      
      Value |  N | Raw % | Valid % | Cumulative %
      ------+----+-------+---------+-------------
      1     |  5 | 10.87 |   12.20 |        12.20
      2     | 32 | 69.57 |   78.05 |        90.24
      3     |  4 |  8.70 |    9.76 |       100.00
      <NA>  |  5 | 10.87 |    <NA> |         <NA>
      
      carer's level of education (c172code) <numeric>
      Grouped by e16sex (2)
      # total N=54 valid N=49
      
      Value |  N | Raw % | Valid % | Cumulative %
      ------+----+-------+---------+-------------
      1     |  3 |  5.56 |    6.12 |         6.12
      2     | 34 | 62.96 |   69.39 |        75.51
      3     | 12 | 22.22 |   24.49 |       100.00
      <NA>  |  5 |  9.26 |    <NA> |         <NA>

# data_tabulate print, collapse groups

    Code
      data_tabulate(poorman::group_by(efc, e16sex), "c172code", collapse = TRUE)
    Output
      # Frequency Table
      
      Variable |      Group | Value |  N | Raw % | Valid % | Cumulative %
      ---------+------------+-------+----+-------+---------+-------------
      c172code | e16sex (1) |     1 |  5 | 10.87 |   12.20 |        12.20
               |            |     2 | 32 | 69.57 |   78.05 |        90.24
               |            |     3 |  4 |  8.70 |    9.76 |       100.00
               |            |  <NA> |  5 | 10.87 |    <NA> |         <NA>
      ---------+------------+-------+----+-------+---------+-------------
      c172code | e16sex (2) |     1 |  3 |  5.56 |    6.12 |         6.12
               |            |     2 | 34 | 62.96 |   69.39 |        75.51
               |            |     3 | 12 | 22.22 |   24.49 |       100.00
               |            |  <NA> |  5 |  9.26 |    <NA> |         <NA>
      -------------------------------------------------------------------

# data_tabulate print, collapse groups, drop levels

    Code
      data_tabulate(poorman::group_by(efc, e16sex), "e42dep", collapse = TRUE,
      drop_levels = TRUE)
    Output
      # Frequency Table
      
      Variable |      Group | Value |  N | Raw % | Valid % | Cumulative %
      ---------+------------+-------+----+-------+---------+-------------
      e42dep   | e16sex (1) |     1 |  2 |  4.35 |    4.44 |         4.44
               |            |     2 |  2 |  4.35 |    4.44 |         8.89
               |            |     3 |  8 | 17.39 |   17.78 |        26.67
               |            |     4 | 33 | 71.74 |   73.33 |       100.00
               |            |  <NA> |  1 |  2.17 |    <NA> |         <NA>
      ---------+------------+-------+----+-------+---------+-------------
      e42dep   | e16sex (2) |     2 |  2 |  3.70 |    3.85 |         3.85
               |            |     3 | 20 | 37.04 |   38.46 |        42.31
               |            |     4 | 30 | 55.56 |   57.69 |       100.00
               |            |  <NA> |  2 |  3.70 |    <NA> |         <NA>
      -------------------------------------------------------------------

