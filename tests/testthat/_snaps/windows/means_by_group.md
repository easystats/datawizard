# mean_by_group, weighted

    Code
      means_by_group(efc, "c12hour", "e42dep", weights = "weight")
    Output
      # Mean of average number of hours of care per week by elder's dependency
      
      Category             |   Mean |  N |    SD |           95% CI |      p
      ----------------------------------------------------------------------
      independent          |  16.92 |  3 | 11.31 | [-60.82,  94.66] | 0.486 
      slightly dependent   |  33.56 |  4 | 29.75 | [-26.93,  94.05] | 0.593 
      moderately dependent |  52.74 | 26 | 54.44 | [ 28.71,  76.76] | 0.996 
      severely dependent   | 108.08 | 67 | 65.40 | [ 93.01, 123.16] | < .001
      Total                |  88.11 | 97 | 67.01 |                  |       
      
      Anova: R2=0.191; adj.R2=0.165; F=7.329; p<.001

---

    Code
      means_by_group(efc, "c12hour", "e42dep", weights = "weight", ci = NA)
    Output
      # Mean of average number of hours of care per week by elder's dependency
      
      Category             |   Mean |  N |    SD |      p
      ---------------------------------------------------
      independent          |  16.92 |  3 | 11.31 | 0.486 
      slightly dependent   |  33.56 |  4 | 29.75 | 0.593 
      moderately dependent |  52.74 | 26 | 54.44 | 0.996 
      severely dependent   | 108.08 | 67 | 65.40 | < .001
      Total                |  88.11 | 97 | 67.01 |       
      
      Anova: R2=0.191; adj.R2=0.165; F=7.329; p<.001

