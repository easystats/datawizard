# data_separate: multiple columns

    Code
      out
    Output
        x_1 x_2  x_3  y_1  y_2  y_3
      1   1   a    6    m    n   99
      2   2   b    7   77    f    g
      3   3   c    8   44    9 <NA>
      4   5   j <NA> <NA> <NA> <NA>

---

    Code
      out
    Output
        x_1 x_2  x_3  y_1  y_2  y_3
      1   1   a    6    m    n   99
      2   2   b  7 d   77    f    g
      3   3   c    8   44    9 <NA>
      4   5   j <NA> <NA> <NA> <NA>

---

    Code
      out
    Output
        x_A x_B  x_C  y_A  y_B  y_C
      1   1   a    6    m    n   99
      2   2   b  7 d   77    f    g
      3   3   c    8   44    9 <NA>
      4   5   j <NA> <NA> <NA> <NA>

---

    Code
      out
    Output
              x      y x_A x_B  x_C  y_A  y_B  y_C
      1   1.a.6 m.n.99   1   a    6    m    n   99
      2 2.b.7.d 77.f.g   2   b  7 d   77    f    g
      3   3.c.8   44.9   3   c    8   44    9 <NA>
      4     5.j   <NA>   5   j <NA> <NA> <NA> <NA>

---

    Code
      out
    Output
        x_1 x_2  x_3  y_1  y_2  y_3
      1   1   a    6    m    n   99
      2   b   7    d   77    f    g
      3   3   c    8   44    9 <NA>
      4   5   j <NA> <NA> <NA> <NA>

---

    Code
      out
    Output
              x      y x_A x_B x_C  y_A  y_B  y_C
      1   1.a.6 m.n.99   1   a   6    m    n   99
      2 2.b.7.d 77.f.g   2   b 7 d   77    f    g
      3   3.c.8   44.9   3   c   8   44    9    9
      4     5.j   <NA>   5   j   j <NA> <NA> <NA>

---

    Code
      out
    Output
              x      y   A   B   C
      1   1.a.6 m.n.99  1m  an 699
      2 2.b.7.d 77.f.g 277  bf 7dg
      3   3.c.8   44.9 344  c9  89
      4     5.j   <NA> 5NA jNA jNA

---

    Code
      out
    Output
              x      y   A   B    C
      1   1.a.6 m.n.99  1m  an  699
      2 2.b.7.d 77.f.g 277  bf   7g
      3   3.c.8   44.9 344  c9  8NA
      4     5.j   <NA> 5NA jNA NANA

---

    Code
      out
    Output
        x_1 x_2 x_3  y_1  y_2  y_3
      1   1   a   6    m    n   99
      2   2   b   7   77    f    g
      3   3   c   8   44   44    9
      4   5   5   j <NA> <NA> <NA>

# data_separate: multiple columns, different lengths

    Code
      out
    Output
        A B    C   EE   FF   GG
      1 1 a    6    m    n   99
      2 2 b    7   77    f    g
      3 3 c    8   44    9 <NA>
      4 5 j <NA> <NA> <NA> <NA>

---

    Code
      out
    Output
        A B    C   EE   FF   GG   HH
      1 1 a    6    m    n   99   22
      2 2 b    7   77    f    g   34
      3 3 c    8   44    9 <NA> <NA>
      4 5 j <NA> <NA> <NA> <NA> <NA>

# data_separate: fail if invalid column selected

    Code
      data_separate(d_sep, guess_columns = "mode", select = NULL)
    Message <simpleMessage>
      Column `x` had different number of values after splitting. Variable was
        split into 3 columns.
      `x` returned more columns than expected after splitting. Right-most
        columns have been dropped.
      `x`returned fewer columns than expected after splitting. Right-most
        columns were filled with `NA`.
      Column `y` had different number of values after splitting. Variable was
        split into 3 columns.
      `y`returned fewer columns than expected after splitting. Right-most
        columns were filled with `NA`.
    Output
        x_1 x_2  x_3  y_1  y_2  y_3
      1   1   a    6    m    n   99
      2   2   b    7   77    f    g
      3   3   c    8   44    9 <NA>
      4   5   j <NA> <NA> <NA> <NA>

# data_separate: numeric column

    Code
      out
    Output
              y x_1 x_2 x_3 x_4
      V1 m.n.99  15 435 352   3
      V2 77.f.g  53 554 353   2
      V3   44.9  12 342 422    
      V4   <NA>  15 454 334 535

