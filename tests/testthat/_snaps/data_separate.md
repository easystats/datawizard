# data_separate: multiple columns

    Code
      print(out)
    Output
        split_1 split_2 split_3 split_1.1 split_2.1 split_3.1
      1       1       a       6         m         n        99
      2       2       b       7        77         f         g
      3       3       c       8        44         9      <NA>
      4       5       j    <NA>      <NA>      <NA>      <NA>

---

    Code
      print(out)
    Output
        split_1 split_2 split_3 split_1.1 split_2.1 split_3.1
      1       1       a       6         m         n        99
      2       2       b     7 d        77         f         g
      3       3       c       8        44         9      <NA>
      4       5       j    <NA>      <NA>      <NA>      <NA>

---

    Code
      print(out)
    Output
        A B    C  A.1  B.1  C.1
      1 1 a    6    m    n   99
      2 2 b  7 d   77    f    g
      3 3 c    8   44    9 <NA>
      4 5 j <NA> <NA> <NA> <NA>

---

    Code
      print(out)
    Output
              x      y A B    C  A.1  B.1  C.1
      1   1.a.6 m.n.99 1 a    6    m    n   99
      2 2.b.7.d 77.f.g 2 b  7 d   77    f    g
      3   3.c.8   44.9 3 c    8   44    9 <NA>
      4     5.j   <NA> 5 j <NA> <NA> <NA> <NA>

---

    Code
      print(out)
    Output
              x      y A B   C  A.1  B.1  C.1
      1   1.a.6 m.n.99 1 a   6    m    n   99
      2 2.b.7.d 77.f.g 2 b 7 d   77    f    g
      3   3.c.8   44.9 3 c   8   44    9    9
      4     5.j   <NA> 5 j   j <NA> <NA> <NA>

---

    Code
      print(out)
    Output
              x      y   A   B    C
      1   1.a.6 m.n.99  1m  an  699
      2 2.b.7.d 77.f.g 277  bf 7 dg
      3   3.c.8   44.9 344  c9   89
      4     5.j   <NA> 5NA jNA  jNA

---

    Code
      print(out)
    Output
              x      y   A   B    C
      1   1.a.6 m.n.99  1m  an  699
      2 2.b.7.d 77.f.g 277  bf   7g
      3   3.c.8   44.9 344  c9  8NA
      4     5.j   <NA> 5NA jNA NANA

