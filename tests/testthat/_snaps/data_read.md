# data_read, convert many labels correctly

    Code
      data_tabulate(d$selv1)
    Output
      d$selv1 <categorical>
      # total N=2413 valid N=2413
      
      Value                                              |   N | Raw % | Valid % | Cumulative %
      ---------------------------------------------------+-----+-------+---------+-------------
      Vignette 1 weiblich (Gülsen E. Reinigungskraft B)  | 150 |  6.22 |    6.22 |         6.22
      Vignette 2 weiblich (Gülsen E. Anwältin B)         | 150 |  6.22 |    6.22 |        12.43
      Vignette 3 weiblich (Monika E. Reinigungskraft B)  | 150 |  6.22 |    6.22 |        18.65
      Vignette 4 weiblich (Monika E. Anwältin B)         | 151 |  6.26 |    6.26 |        24.91
      Vignette 5 männlich (Hasan E. Reinigungskraft B)   | 151 |  6.26 |    6.26 |        31.16
      Vignette 6 männlich (Hasan E. Anwalt B)            | 153 |  6.34 |    6.34 |        37.51
      Vignette 7 männlich (Martin E. Reinigungskraft B)  | 150 |  6.22 |    6.22 |        43.72
      Vignette 8 männlich (Martin E. Anwalt B)           | 150 |  6.22 |    6.22 |        49.94
      Vignette 9 weiblich (Gülsen E. Reinigungskraft E)  | 151 |  6.26 |    6.26 |        56.20
      Vignette 10 weiblich (Gülsen E. Anwältin E)        | 150 |  6.22 |    6.22 |        62.41
      Vignette 11 weiblich (Monika E. Reinigungskraft E) | 150 |  6.22 |    6.22 |        68.63
      Vignette 12 weiblich (Monika E. Anwältin E)        | 151 |  6.26 |    6.26 |        74.89
      Vignette 13 männlich (Hasan E. Reinigungskraft E)  | 155 |  6.42 |    6.42 |        81.31
      Vignette 14 männlich (Hasan E. Anwalt E)           | 150 |  6.22 |    6.22 |        87.53
      Vignette 15 männlich (Martin E. Reinigungskraft E) | 150 |  6.22 |    6.22 |        93.74
      Vignette 16 männlich (Martin E. Anwalt E)          | 151 |  6.26 |    6.26 |       100.00
      <NA>                                               |   0 |  0.00 |    <NA> |         <NA>

---

    Code
      data_tabulate(d$c12)
    Output
      Sind oder waren Sie schon einmal selbst von solchen Beschwerden betroffen? (d$c12) <categorical>
      # total N=2413 valid N=2413
      
      Value        |    N | Raw % | Valid % | Cumulative %
      -------------+------+-------+---------+-------------
      ja           |  786 | 32.57 |   32.57 |        32.57
      nein         | 1616 | 66.97 |   66.97 |        99.54
      keine Angabe |   11 |  0.46 |    0.46 |       100.00
      <NA>         |    0 |  0.00 |    <NA> |         <NA>

---

    Code
      data_tabulate(d$c12a)
    Output
      Haben Sie deswegen Behandlung(en) in Anspruch genommen? (d$c12a) <categorical>
      # total N=2413 valid N=2413
      
      Value        |    N | Raw % | Valid % | Cumulative %
      -------------+------+-------+---------+-------------
      Filter       | 1627 | 67.43 |   67.43 |        67.43
      ja           |  500 | 20.72 |   20.72 |        88.15
      nein         |  285 | 11.81 |   11.81 |        99.96
      keine Angabe |    1 |  0.04 |    0.04 |       100.00
      <NA>         |    0 |  0.00 |    <NA> |         <NA>

---

    Code
      data_tabulate(d$c12c)
    Output
      Wie sehr haben diese Behandlung(en) Ihre Beeinträchtigung durch die Beschwerden verbessert? (d$c12c) <categorical>
      # total N=2413 valid N=2413
      
      Value                     |    N | Raw % | Valid % | Cumulative %
      --------------------------+------+-------+---------+-------------
      Filter                    | 1913 | 79.28 |   79.28 |        79.28
      0 = keine                 |   34 |  1.41 |    1.41 |        80.69
      1                         |    2 |  0.08 |    0.08 |        80.77
      2                         |   11 |  0.46 |    0.46 |        81.23
      3                         |   14 |  0.58 |    0.58 |        81.81
      4                         |   19 |  0.79 |    0.79 |        82.59
      5                         |   61 |  2.53 |    2.53 |        85.12
      6                         |   42 |  1.74 |    1.74 |        86.86
      7                         |   63 |  2.61 |    2.61 |        89.47
      8                         |   97 |  4.02 |    4.02 |        93.49
      9                         |   53 |  2.20 |    2.20 |        95.69
      10 = sehr starke          |   99 |  4.10 |    4.10 |        99.79
      weiß nicht / keine Angabe |    5 |  0.21 |    0.21 |       100.00
      <NA>                      |    0 |  0.00 |    <NA> |         <NA>

---

    Code
      table(d$selv1)
    Output
      
        1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16 
      150 150 150 151 151 153 150 150 151 150 150 151 155 150 150 151 

---

    Code
      table(d$c12)
    Output
      
         1    2   99 
       786 1616   11 

---

    Code
      table(d$c12a)
    Output
      
        -2    1    2   99 
      1627  500  285    1 

---

    Code
      table(d$c12c)
    Output
      
        -2    0    1    2    3    4    5    6    7    8    9   10   99 
      1913   34    2   11   14   19   61   42   63   97   53   99    5 

