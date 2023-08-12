# meany_by_group

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

