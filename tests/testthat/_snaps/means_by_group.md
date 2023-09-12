# mean_by_group

    Code
      means_by_group(efc, "c12hour", "e42dep")
    Output
      # Mean of average number of hours of care per week by elder's dependency
      
      Category             |   Mean |  N |    SD |           95% CI |      p
      ----------------------------------------------------------------------
      independent          |  17.00 |  2 | 11.31 | [-68.46, 102.46] | 0.573 
      slightly dependent   |  34.25 |  4 | 29.97 | [-26.18,  94.68] | 0.626 
      moderately dependent |  52.75 | 28 | 51.83 | [ 29.91,  75.59] | > .999
      severely dependent   | 106.97 | 63 | 65.88 | [ 91.74, 122.19] | 0.001 
      Total                |  86.46 | 97 | 66.40 |                  |       
      
      Anova: R2=0.186; adj.R2=0.160; F=7.098; p<.001

---

    Code
      means_by_group(efc, "c12hour", "e42dep", ci = 0.99)
    Output
      # Mean of average number of hours of care per week by elder's dependency
      
      Category             |   Mean |  N |    SD |           99% CI |      p
      ----------------------------------------------------------------------
      independent          |  17.00 |  2 | 11.31 | [-96.17, 130.17] | 0.573 
      slightly dependent   |  34.25 |  4 | 29.97 | [-45.77, 114.27] | 0.626 
      moderately dependent |  52.75 | 28 | 51.83 | [ 22.50,  83.00] | > .999
      severely dependent   | 106.97 | 63 | 65.88 | [ 86.80, 127.13] | 0.001 
      Total                |  86.46 | 97 | 66.40 |                  |       
      
      Anova: R2=0.186; adj.R2=0.160; F=7.098; p<.001

---

    Code
      means_by_group(efc, "c12hour", "e42dep", ci = NA)
    Output
      # Mean of average number of hours of care per week by elder's dependency
      
      Category             |   Mean |  N |    SD |      p
      ---------------------------------------------------
      independent          |  17.00 |  2 | 11.31 | 0.573 
      slightly dependent   |  34.25 |  4 | 29.97 | 0.626 
      moderately dependent |  52.75 | 28 | 51.83 | > .999
      severely dependent   | 106.97 | 63 | 65.88 | 0.001 
      Total                |  86.46 | 97 | 66.40 |       
      
      Anova: R2=0.186; adj.R2=0.160; F=7.098; p<.001

---

    Code
      means_by_group(efc, c("neg_c_7", "c12hour"), "e42dep")
    Output
      # Mean of Negative impact with 7 items by elder's dependency
      
      Category             |  Mean |  N |   SD |         95% CI |     p
      -----------------------------------------------------------------
      independent          | 11.00 |  2 | 0.00 | [ 5.00, 17.00] | 0.567
      slightly dependent   | 10.00 |  4 | 3.16 | [ 5.76, 14.24] | 0.296
      moderately dependent | 13.71 | 28 | 3.14 | [12.11, 15.32] | 0.296
      severely dependent   | 14.67 | 60 | 4.78 | [13.57, 15.76] | 0.108
      Total                | 14.11 | 94 | 4.34 |                |      
      
      Anova: R2=0.063; adj.R2=0.032; F=2.009; p=0.118
      
      # Mean of average number of hours of care per week by elder's dependency
      
      Category             |   Mean |  N |    SD |           95% CI |      p
      ----------------------------------------------------------------------
      independent          |  17.00 |  2 | 11.31 | [-68.46, 102.46] | 0.573 
      slightly dependent   |  34.25 |  4 | 29.97 | [-26.18,  94.68] | 0.626 
      moderately dependent |  52.75 | 28 | 51.83 | [ 29.91,  75.59] | > .999
      severely dependent   | 106.97 | 63 | 65.88 | [ 91.74, 122.19] | 0.001 
      Total                |  86.46 | 97 | 66.40 |                  |       
      
      Anova: R2=0.186; adj.R2=0.160; F=7.098; p<.001

---

    Code
      means_by_group(efc, c("neg_c_7", "c12hour"), "e42dep", ci = NA)
    Output
      # Mean of Negative impact with 7 items by elder's dependency
      
      Category             |  Mean |  N |   SD |     p
      ------------------------------------------------
      independent          | 11.00 |  2 | 0.00 | 0.567
      slightly dependent   | 10.00 |  4 | 3.16 | 0.296
      moderately dependent | 13.71 | 28 | 3.14 | 0.296
      severely dependent   | 14.67 | 60 | 4.78 | 0.108
      Total                | 14.11 | 94 | 4.34 |      
      
      Anova: R2=0.063; adj.R2=0.032; F=2.009; p=0.118
      
      # Mean of average number of hours of care per week by elder's dependency
      
      Category             |   Mean |  N |    SD |      p
      ---------------------------------------------------
      independent          |  17.00 |  2 | 11.31 | 0.573 
      slightly dependent   |  34.25 |  4 | 29.97 | 0.626 
      moderately dependent |  52.75 | 28 | 51.83 | > .999
      severely dependent   | 106.97 | 63 | 65.88 | 0.001 
      Total                |  86.46 | 97 | 66.40 |       
      
      Anova: R2=0.186; adj.R2=0.160; F=7.098; p<.001

---

    Code
      means_by_group(efc, c("neg_c_7", "c12hour"), "e42dep", ci = 0.99)
    Output
      # Mean of Negative impact with 7 items by elder's dependency
      
      Category             |  Mean |  N |   SD |         99% CI |     p
      -----------------------------------------------------------------
      independent          | 11.00 |  2 | 0.00 | [ 3.05, 18.95] | 0.567
      slightly dependent   | 10.00 |  4 | 3.16 | [ 4.38, 15.62] | 0.296
      moderately dependent | 13.71 | 28 | 3.14 | [11.59, 15.84] | 0.296
      severely dependent   | 14.67 | 60 | 4.78 | [13.22, 16.12] | 0.108
      Total                | 14.11 | 94 | 4.34 |                |      
      
      Anova: R2=0.063; adj.R2=0.032; F=2.009; p=0.118
      
      # Mean of average number of hours of care per week by elder's dependency
      
      Category             |   Mean |  N |    SD |           99% CI |      p
      ----------------------------------------------------------------------
      independent          |  17.00 |  2 | 11.31 | [-96.17, 130.17] | 0.573 
      slightly dependent   |  34.25 |  4 | 29.97 | [-45.77, 114.27] | 0.626 
      moderately dependent |  52.75 | 28 | 51.83 | [ 22.50,  83.00] | > .999
      severely dependent   | 106.97 | 63 | 65.88 | [ 86.80, 127.13] | 0.001 
      Total                |  86.46 | 97 | 66.40 |                  |       
      
      Anova: R2=0.186; adj.R2=0.160; F=7.098; p<.001

---

    Code
      means_by_group(efc$c12hour, efc$e42dep)
    Output
      # Mean of average number of hours of care per week by elder's dependency
      
      Category             |   Mean |  N |    SD |           95% CI |      p
      ----------------------------------------------------------------------
      independent          |  17.00 |  2 | 11.31 | [-68.46, 102.46] | 0.573 
      slightly dependent   |  34.25 |  4 | 29.97 | [-26.18,  94.68] | 0.626 
      moderately dependent |  52.75 | 28 | 51.83 | [ 29.91,  75.59] | > .999
      severely dependent   | 106.97 | 63 | 65.88 | [ 91.74, 122.19] | 0.001 
      Total                |  86.46 | 97 | 66.40 |                  |       
      
      Anova: R2=0.186; adj.R2=0.160; F=7.098; p<.001

---

    Code
      means_by_group(efc$c12hour, efc$e42dep, ci = NA)
    Output
      # Mean of average number of hours of care per week by elder's dependency
      
      Category             |   Mean |  N |    SD |      p
      ---------------------------------------------------
      independent          |  17.00 |  2 | 11.31 | 0.573 
      slightly dependent   |  34.25 |  4 | 29.97 | 0.626 
      moderately dependent |  52.75 | 28 | 51.83 | > .999
      severely dependent   | 106.97 | 63 | 65.88 | 0.001 
      Total                |  86.46 | 97 | 66.40 |       
      
      Anova: R2=0.186; adj.R2=0.160; F=7.098; p<.001

