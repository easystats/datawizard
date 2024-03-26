# data_tabulate, weights

    Code
      print(data_tabulate(efc$e42dep, weights = efc$weights))
    Output
      elder's dependency (efc$e42dep) <categorical>
      # total N=105 valid N=100 (weighted)
      
      Value |  N | Raw % | Valid % | Cumulative %
      ------+----+-------+---------+-------------
      1     |  3 |  2.86 |    3.00 |         3.00
      2     |  4 |  3.81 |    4.00 |         7.00
      3     | 26 | 24.76 |   26.00 |        33.00
      4     | 67 | 63.81 |   67.00 |       100.00
      <NA>  |  5 |  4.76 |    <NA> |         <NA>

---

    Code
      print_md(data_tabulate(efc$e42dep, weights = efc$weights))
    Output
       [1] "Table: elder's dependency (efc$e42dep) (categorical)"
       [2] ""                                                    
       [3] "|Value |  N| Raw %| Valid %| Cumulative %|"          
       [4] "|:-----|--:|-----:|-------:|------------:|"          
       [5] "|1     |  3|  2.86|    3.00|         3.00|"          
       [6] "|2     |  4|  3.81|    4.00|         7.00|"          
       [7] "|3     | 26| 24.76|   26.00|        33.00|"          
       [8] "|4     | 67| 63.81|   67.00|       100.00|"          
       [9] "|(NA)  |  5|  4.76|    (NA)|         (NA)|"          
      [10] "total N=105 valid N=100 (weighted)\n\n"              
      attr(,"format")
      [1] "pipe"
      attr(,"class")
      [1] "knitr_kable" "character"  

---

    Code
      print(data_tabulate(efc, c("e42dep", "e16sex"), collapse = TRUE, weights = efc$
        weights))
    Output
      # Frequency Table (weighted)
      
      Variable | Value |  N | Raw % | Valid % | Cumulative %
      ---------+-------+----+-------+---------+-------------
      e42dep   |     1 |  3 |  2.86 |    3.00 |         3.00
               |     2 |  4 |  3.81 |    4.00 |         7.00
               |     3 | 26 | 24.76 |   26.00 |        33.00
               |     4 | 67 | 63.81 |   67.00 |       100.00
               |  <NA> |  5 |  4.76 |    <NA> |         <NA>
      ---------+-------+----+-------+---------+-------------
      e16sex   |     1 | 50 | 47.62 |   47.62 |        47.62
               |     2 | 55 | 52.38 |   52.38 |       100.00
               |  <NA> |  0 |  0.00 |    <NA> |         <NA>
      ------------------------------------------------------

---

    Code
      print_md(data_tabulate(efc, c("e42dep", "e16sex"), weights = efc$weights))
    Output
       [1] "Table: Frequency Table (weighted)"                   
       [2] ""                                                    
       [3] "|Variable | Value|  N| Raw %| Valid %| Cumulative %|"
       [4] "|:--------|-----:|--:|-----:|-------:|------------:|"
       [5] "|e42dep   |     1|  3|  2.86|    3.00|         3.00|"
       [6] "|         |     2|  4|  3.81|    4.00|         7.00|"
       [7] "|         |     3| 26| 24.76|   26.00|        33.00|"
       [8] "|         |     4| 67| 63.81|   67.00|       100.00|"
       [9] "|         |  (NA)|  5|  4.76|    (NA)|         (NA)|"
      [10] "|         |      |   |      |        |             |"
      [11] "|e16sex   |     1| 50| 47.62|   47.62|        47.62|"
      [12] "|         |     2| 55| 52.38|   52.38|       100.00|"
      [13] "|         |  (NA)|  0|  0.00|    (NA)|         (NA)|"
      [14] "|         |      |   |      |        |             |"
      attr(,"format")
      [1] "pipe"
      attr(,"class")
      [1] "knitr_kable" "character"  

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

# data_tabulate, cross tables

    Code
      print(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full"))
    Output
      efc$c172code |       male |     female |     <NA> | Total
      -------------+------------+------------+----------+------
      1            |  5  (5.0%) |  2  (2.0%) | 1 (1.0%) |     8
      2            | 31 (31.0%) | 33 (33.0%) | 2 (2.0%) |    66
      3            |  4  (4.0%) | 11 (11.0%) | 1 (1.0%) |    16
      <NA>         |  5  (5.0%) |  4  (4.0%) | 1 (1.0%) |    10
      -------------+------------+------------+----------+------
      Total        |         45 |         50 |        5 |   100

---

    Code
      print(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full",
      include_na = FALSE))
    Output
      efc$c172code |       male |     female | Total
      -------------+------------+------------+------
      1            |  5  (5.8%) |  2  (2.3%) |     7
      2            | 31 (36.0%) | 33 (38.4%) |    64
      3            |  4  (4.7%) | 11 (12.8%) |    15
      -------------+------------+------------+------
      Total        |         40 |         46 |    86

---

    Code
      print(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full",
      weights = efc$weights))
    Output
      efc$c172code |       male |     female |     <NA> | Total
      -------------+------------+------------+----------+------
      1            |  5  (4.8%) |  3  (2.9%) | 2 (1.9%) |    10
      2            | 32 (30.5%) | 32 (30.5%) | 3 (2.9%) |    67
      3            |  3  (2.9%) | 11 (10.5%) | 1 (1.0%) |    15
      <NA>         |  8  (7.6%) |  5  (4.8%) | 1 (1.0%) |    14
      -------------+------------+------------+----------+------
      Total        |         48 |         51 |        7 |   105

---

    Code
      print(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full",
      include_na = FALSE, weights = efc$weights))
    Output
      efc$c172code |       male |     female | Total
      -------------+------------+------------+------
      1            |  5  (5.8%) |  3  (3.5%) |     8
      2            | 32 (37.2%) | 32 (37.2%) |    64
      3            |  3  (3.5%) | 11 (12.8%) |    14
      -------------+------------+------------+------
      Total        |         40 |         46 |    86

---

    Code
      print(data_tabulate(efc, "c172code", by = efc$e16sex, proportions = "row"))
    Output
      c172code |       male |     female |      <NA> | Total
      ---------+------------+------------+-----------+------
      1        |  5 (62.5%) |  2 (25.0%) | 1 (12.5%) |     8
      2        | 31 (47.0%) | 33 (50.0%) | 2  (3.0%) |    66
      3        |  4 (25.0%) | 11 (68.8%) | 1  (6.2%) |    16
      <NA>     |  5 (50.0%) |  4 (40.0%) | 1 (10.0%) |    10
      ---------+------------+------------+-----------+------
      Total    |         45 |         50 |         5 |   100
      

---

    Code
      print(data_tabulate(efc, "c172code", by = efc$e16sex, proportions = "row",
      include_na = FALSE))
    Output
      c172code |       male |     female | Total
      ---------+------------+------------+------
      1        |  5 (71.4%) |  2 (28.6%) |     7
      2        | 31 (48.4%) | 33 (51.6%) |    64
      3        |  4 (26.7%) | 11 (73.3%) |    15
      ---------+------------+------------+------
      Total    |         40 |         46 |    86
      

---

    Code
      print(data_tabulate(efc, "c172code", by = efc$e16sex, proportions = "row",
      weights = efc$weights))
    Output
      c172code |       male |     female |      <NA> | Total
      ---------+------------+------------+-----------+------
      1        |  5 (50.0%) |  3 (30.0%) | 2 (20.0%) |    10
      2        | 32 (47.8%) | 32 (47.8%) | 3  (4.5%) |    67
      3        |  3 (20.0%) | 11 (73.3%) | 1  (6.7%) |    15
      <NA>     |  8 (57.1%) |  5 (35.7%) | 1  (7.1%) |    14
      ---------+------------+------------+-----------+------
      Total    |         48 |         51 |         7 |   105
      

---

    Code
      print(data_tabulate(efc, "c172code", by = efc$e16sex, proportions = "row",
      include_na = FALSE, weights = efc$weights))
    Output
      c172code |       male |     female | Total
      ---------+------------+------------+------
      1        |  5 (62.5%) |  3 (37.5%) |     8
      2        | 32 (50.0%) | 32 (50.0%) |    64
      3        |  3 (21.4%) | 11 (78.6%) |    14
      ---------+------------+------------+------
      Total    |         40 |         46 |    86
      

---

    Code
      print(data_tabulate(efc, "c172code", by = "e16sex", proportions = "column"))
    Output
      c172code |       male |     female |      <NA> | Total
      ---------+------------+------------+-----------+------
      1        |  5 (11.1%) |  2  (4.0%) | 1 (20.0%) |     8
      2        | 31 (68.9%) | 33 (66.0%) | 2 (40.0%) |    66
      3        |  4  (8.9%) | 11 (22.0%) | 1 (20.0%) |    16
      <NA>     |  5 (11.1%) |  4  (8.0%) | 1 (20.0%) |    10
      ---------+------------+------------+-----------+------
      Total    |         45 |         50 |         5 |   100
      

---

    Code
      print(data_tabulate(efc, "c172code", by = "e16sex", proportions = "column",
        include_na = FALSE))
    Output
      c172code |       male |     female | Total
      ---------+------------+------------+------
      1        |  5 (12.5%) |  2  (4.3%) |     7
      2        | 31 (77.5%) | 33 (71.7%) |    64
      3        |  4 (10.0%) | 11 (23.9%) |    15
      ---------+------------+------------+------
      Total    |         40 |         46 |    86
      

---

    Code
      print(data_tabulate(efc, "c172code", by = "e16sex", proportions = "column",
        weights = "weights"))
    Output
      c172code |       male |     female |      <NA> | Total
      ---------+------------+------------+-----------+------
      1        |  5 (10.4%) |  3  (5.9%) | 2 (28.6%) |    10
      2        | 32 (66.7%) | 32 (62.7%) | 3 (42.9%) |    67
      3        |  3  (6.2%) | 11 (21.6%) | 1 (14.3%) |    15
      <NA>     |  8 (16.7%) |  5  (9.8%) | 1 (14.3%) |    14
      ---------+------------+------------+-----------+------
      Total    |         48 |         51 |         7 |   105
      

---

    Code
      print(data_tabulate(efc, "c172code", by = "e16sex", proportions = "column",
        include_na = FALSE, weights = "weights"))
    Output
      c172code |       male |     female | Total
      ---------+------------+------------+------
      1        |  5 (12.5%) |  3  (6.5%) |     8
      2        | 32 (80.0%) | 32 (69.6%) |    64
      3        |  3  (7.5%) | 11 (23.9%) |    14
      ---------+------------+------------+------
      Total    |         40 |         46 |    86
      

# data_tabulate, cross tables, grouped df

    Code
      print(data_tabulate(grp, "c172code", by = "e16sex", proportions = "row"))
    Output
      Grouped by e42dep (1)
      
      c172code |       male |       <NA> | Total
      ---------+------------+------------+------
      2        | 2 (100.0%) | 0   (0.0%) |     2
      <NA>     |     0 (0%) |     0 (0%) |     0
      ---------+------------+------------+------
      Total    |          2 |          0 |     2
      
      Grouped by e42dep (2)
      
      c172code |      male |    female |      <NA> | Total
      ---------+-----------+-----------+-----------+------
      2        | 2 (50.0%) | 2 (50.0%) | 0  (0.0%) |     4
      <NA>     |    0 (0%) |    0 (0%) |    0 (0%) |     0
      ---------+-----------+-----------+-----------+------
      Total    |         2 |         2 |         0 |     4
      
      Grouped by e42dep (3)
      
      c172code |      male |     female |      <NA> | Total
      ---------+-----------+------------+-----------+------
      1        | 2 (50.0%) |  2 (50.0%) | 0  (0.0%) |     4
      2        | 4 (25.0%) | 11 (68.8%) | 1  (6.2%) |    16
      3        | 1 (16.7%) |  5 (83.3%) | 0  (0.0%) |     6
      <NA>     | 1 (50.0%) |  0  (0.0%) | 1 (50.0%) |     2
      ---------+-----------+------------+-----------+------
      Total    |         8 |         18 |         2 |    28
      
      Grouped by e42dep (4)
      
      c172code |       male |     female |      <NA> | Total
      ---------+------------+------------+-----------+------
      1        |  3 (75.0%) |  0  (0.0%) | 1 (25.0%) |     4
      2        | 23 (54.8%) | 18 (42.9%) | 1  (2.4%) |    42
      3        |  3 (30.0%) |  6 (60.0%) | 1 (10.0%) |    10
      <NA>     |  3 (42.9%) |  4 (57.1%) | 0  (0.0%) |     7
      ---------+------------+------------+-----------+------
      Total    |         32 |         28 |         3 |    63
      
      Grouped by e42dep (NA)
      
      c172code |       male |     female |       <NA> | Total
      ---------+------------+------------+------------+------
      2        | 0   (0.0%) | 2 (100.0%) | 0   (0.0%) |     2
      <NA>     | 1 (100.0%) | 0   (0.0%) | 0   (0.0%) |     1
      ---------+------------+------------+------------+------
      Total    |          1 |          2 |          0 |     3
      

# data_tabulate, cross tables, markdown

    Code
      print_md(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full"))
    Output
      [1] "|efc$c172code |       male|     female|    (NA) | Total|"
      [2] "|:------------|----------:|----------:|:--------|-----:|"
      [3] "|1            |  5  (5.0%)|  2  (2.0%)|1 (1.0%) |     8|"
      [4] "|2            | 31 (31.0%)| 33 (33.0%)|2 (2.0%) |    66|"
      [5] "|3            |  4  (4.0%)| 11 (11.0%)|1 (1.0%) |    16|"
      [6] "|<NA>         |  5  (5.0%)|  4  (4.0%)|1 (1.0%) |    10|"
      [7] "|             |           |           |         |      |"
      [8] "|Total        |         45|         50|       5 |   100|"
      attr(,"format")
      [1] "pipe"
      attr(,"class")
      [1] "knitr_kable" "character"  

---

    Code
      print_md(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full",
      include_na = FALSE))
    Output
      [1] "|efc$c172code |       male|     female| Total|"
      [2] "|:------------|----------:|----------:|-----:|"
      [3] "|1            |  5  (5.8%)|  2  (2.3%)|     7|"
      [4] "|2            | 31 (36.0%)| 33 (38.4%)|    64|"
      [5] "|3            |  4  (4.7%)| 11 (12.8%)|    15|"
      [6] "|             |           |           |      |"
      [7] "|Total        |         40|         46|    86|"
      attr(,"format")
      [1] "pipe"
      attr(,"class")
      [1] "knitr_kable" "character"  

---

    Code
      print_md(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full",
      weights = efc$weights))
    Output
      [1] "|efc$c172code |       male|     female|    (NA) | Total|"
      [2] "|:------------|----------:|----------:|:--------|-----:|"
      [3] "|1            |  5  (4.8%)|  3  (2.9%)|2 (1.9%) |    10|"
      [4] "|2            | 32 (30.5%)| 32 (30.5%)|3 (2.9%) |    67|"
      [5] "|3            |  3  (2.9%)| 11 (10.5%)|1 (1.0%) |    15|"
      [6] "|<NA>         |  8  (7.6%)|  5  (4.8%)|1 (1.0%) |    14|"
      [7] "|             |           |           |         |      |"
      [8] "|Total        |         48|         51|       7 |   105|"
      attr(,"format")
      [1] "pipe"
      attr(,"class")
      [1] "knitr_kable" "character"  

---

    Code
      print_md(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full",
      include_na = FALSE, weights = efc$weights))
    Output
      [1] "|efc$c172code |       male|     female| Total|"
      [2] "|:------------|----------:|----------:|-----:|"
      [3] "|1            |  5  (5.8%)|  3  (3.5%)|     8|"
      [4] "|2            | 32 (37.2%)| 32 (37.2%)|    64|"
      [5] "|3            |  3  (3.5%)| 11 (12.8%)|    14|"
      [6] "|             |           |           |      |"
      [7] "|Total        |         40|         46|    86|"
      attr(,"format")
      [1] "pipe"
      attr(,"class")
      [1] "knitr_kable" "character"  

# data_tabulate, correct 0% for proportions

    Code
      print(out[[1]])
    Output
      c172code |       male |     female |   <NA> | Total
      ---------+------------+------------+--------+------
      1        |  5 (10.9%) |  3  (5.6%) | 0 (0%) |     8
      2        | 32 (69.6%) | 34 (63.0%) | 0 (0%) |    66
      3        |  4  (8.7%) | 12 (22.2%) | 0 (0%) |    16
      <NA>     |  5 (10.9%) |  5  (9.3%) | 0 (0%) |    10
      ---------+------------+------------+--------+------
      Total    |         46 |         54 |      0 |   100

