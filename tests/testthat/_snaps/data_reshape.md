# data_reshape works as expected - wide to long

    Code
      data_to_long(wide_data)
    Output
         Name       Value
      1    X1 -0.56047565
      2    X2  1.71506499
      3    X3  1.22408180
      4    X1 -0.23017749
      5    X2  0.46091621
      6    X3  0.35981383
      7    X1  1.55870831
      8    X2 -1.26506123
      9    X3  0.40077145
      10   X1  0.07050839
      11   X2 -0.68685285
      12   X3  0.11068272
      13   X1  0.12928774
      14   X2 -0.44566197
      15   X3 -0.55584113

---

    Code
      data_to_long(wide_data, cols = c(1, 2), colnames_to = "Column", values_to = "Numbers",
      rows_to = "Row")
    Output
                 X3 Row Column     Numbers
      1   1.2240818   1     X1 -0.56047565
      2   1.2240818   1     X2  1.71506499
      3   0.3598138   2     X1 -0.23017749
      4   0.3598138   2     X2  0.46091621
      5   0.4007715   3     X1  1.55870831
      6   0.4007715   3     X2 -1.26506123
      7   0.1106827   4     X1  0.07050839
      8   0.1106827   4     X2 -0.68685285
      9  -0.5558411   5     X1  0.12928774
      10 -0.5558411   5     X2 -0.44566197

# data_reshape works as expected - long to wide

    Code
      data_to_wide(long_data, colnames_from = "Name", values_from = "Value",
        rows_from = "Row_ID")
    Output
        Row_ID    Value_X1   Value_X2   Value_X3
      1      1 -0.56047565  1.7150650  1.2240818
      2      2 -0.23017749  0.4609162  0.3598138
      3      3  1.55870831 -1.2650612  0.4007715
      4      4  0.07050839 -0.6868529  0.1106827
      5      5  0.12928774 -0.4456620 -0.5558411

# data_reshape works as expected - complex dataset

    Code
      str(long)
    Output
      'data.frame':	70000 obs. of  6 variables:
       $ gender     : int  1 1 1 1 1 1 1 1 1 1 ...
       $ education  : int  NA NA NA NA NA NA NA NA NA NA ...
       $ age        : int  16 16 16 16 16 16 16 16 16 16 ...
       $ Participant: num  61617 61617 61617 61617 61617 ...
       $ Item       : chr  "A1" "A2" "A3" "A4" ...
       $ Score      : int  2 4 3 4 4 2 3 3 4 4 ...

---

    Code
      str(wide)
    Output
      'data.frame':	14000 obs. of  10 variables:
       $ gender     : int  1 1 1 1 1 2 2 2 2 2 ...
       $ education  : int  NA NA NA NA NA NA NA NA NA NA ...
       $ age        : int  16 16 16 16 16 18 18 18 18 18 ...
       $ Participant: num  61617 61617 61617 61617 61617 ...
       $ Facet      : chr  "A" "C" "E" "N" ...
       $ Score_I1   : int  2 2 3 3 3 2 5 1 3 4 ...
       $ Score_I2   : int  4 3 3 4 6 4 4 1 3 2 ...
       $ Score_I3   : int  3 3 3 2 3 5 4 6 3 4 ...
       $ Score_I4   : int  4 4 4 2 4 2 3 4 5 3 ...
       $ Score_I5   : int  4 4 4 3 3 5 4 3 5 3 ...

