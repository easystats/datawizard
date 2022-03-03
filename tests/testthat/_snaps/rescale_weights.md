# rescale_weights works as expected

    Code
      head(rescale_weights(nhanes_sample, "SDMVSTRA", "WTINT2YR"))
    Output
        total  age RIAGENDR RIDRETH1 SDMVPSU SDMVSTRA WTINT2YR pweights_a pweights_b
      1     1 2.20        1        3       2       31 97593.68  1.5733612  1.2005159
      2     7 2.08        2        3       1       29 39599.36  0.6231745  0.5246593
      3     3 1.48        2        1       2       42 26619.83  0.8976966  0.5439111
      4     4 1.32        2        4       2       33 34998.53  0.7083628  0.5498944
      5     1 2.00        2        1       1       41 14746.45  0.4217782  0.3119698
      6     6 2.20        2        4       1       38 28232.10  0.6877550  0.5155503

---

    Code
      head(rescale_weights(nhanes_sample, c("SDMVSTRA", "SDMVPSU"), "WTINT2YR"))
    Output
        total  age RIAGENDR RIDRETH1 SDMVPSU SDMVSTRA WTINT2YR pweight_a_SDMVSTRA
      1     1 2.20        1        3       2       31 97593.68          1.5733612
      2     7 2.08        2        3       1       29 39599.36          0.6231745
      3     3 1.48        2        1       2       42 26619.83          0.8976966
      4     4 1.32        2        4       2       33 34998.53          0.7083628
      5     1 2.00        2        1       1       41 14746.45          0.4217782
      6     6 2.20        2        4       1       38 28232.10          0.6877550
        pweight_b_SDMVSTRA pweight_a_SDMVPSU pweight_b_SDMVPSU
      1          1.2005159         1.8458164         1.3699952
      2          0.5246593         0.8217570         0.5780808
      3          0.5439111         0.5034683         0.3736824
      4          0.5498944         0.6619369         0.4913004
      5          0.3119698         0.3060151         0.2152722
      6          0.5155503         0.5858662         0.4121388

