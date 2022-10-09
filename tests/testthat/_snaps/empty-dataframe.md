# remove empty with character

    Code
      remove_empty_columns(tmp)
    Output
         a  b  d
      1  1  1  1
      2  2 NA NA
      3  3  3  3
      4 NA NA NA
      5  5  5  5

---

    Code
      remove_empty_rows(tmp)
    Output
        a  b  c  d
      1 1  1 NA  1
      2 2 NA NA NA
      3 3  3 NA  3
      5 5  5 NA  5

---

    Code
      remove_empty(tmp)
    Output
        a  b  d
      1 1  1  1
      2 2 NA NA
      3 3  3  3
      5 5  5  5

