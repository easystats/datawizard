---
title: "datawizard: An R Package for Easy Data Preparation and Statistical Transformations"
tags:
  - R
  - easystats
authors:
- affiliation: 1
  name: Indrajeet Patil
  orcid: 0000-0003-1995-6531
- affiliation: 2
  name: Dominique Makowski
  orcid: 0000-0001-5375-9967
- affiliation: 3
  name: Mattan S. Ben-Shachar
  orcid: 0000-0002-4287-4801
- affiliation: 4
  name: Brenton M. Wiernik^[Brenton Wiernik is currently an independent researcher and Research Scientist at Meta, Demography and Survey Science. The current work was done in an independent capacity.]
  orcid: 0000-0001-9560-6336
- affiliation: 5
  name: Etienne Bacher
  orcid: 0000-0002-9271-5075 
- affiliation: 6
  name: Daniel Lüdecke
  orcid: 0000-0002-8895-3206
  
affiliations:
- index: 1
  name: cynkra Analytics GmbH, Germany
- index: 2
  name: Nanyang Technological University, Singapore
- index: 3
  name: Ben-Gurion University of the Negev, Israel
- index: 4
  name: Independent Researcher
- index: 5
  name: Luxembourg Institute of Socio-Economic Research (LISER), Luxembourg
- index: 6
  name: University Medical Center Hamburg-Eppendorf, Germany
    
date: "2022-10-04"
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
link-citations: yes
---



# Summary

The `{datawizard}` package for the R programming language [@base2021] provides a lightweight toolbox to assist in key steps involved in any data analysis workflow: (1) wrangling the raw data to get it in the needed form, (2) applying preprocessing steps and statistical transformations, and (3) compute statistical summaries of data properties and distributions. Therefore, it can be a valuable tool for R users and developers looking for a lightweight option for data preparation.

# Statement of Need

The `{datawizard}` package is part of `{easystats}`, a collection of R packages designed to make statistical analysis easier (@Ben-Shachar2020, @Lüdecke2020parameters, @Lüdecke2020performance, @Lüdecke2021see, @Lüdecke2019, @Makowski2019, @Makowski2020). As this ecosystem follows a "0-external-hard-dependency" policy, a data manipulation package that relies only on base R needed to be created. In effect, `{datawizard}` provides a data processing backend for this entire ecosystem. 
In addition to its usefulness to the `{easystats}` ecosystem, it also provides *an* option for R users and package developers if they wish to keep their (recursive) dependency weight to a minimum (for other options, see @Dowle2021, @Eastwood2021).

Because `{datawizard}` is also meant to be used and adopted easily by a wide range of users, its workflow and syntax are designed to be similar to `{tidyverse}` [@Wickham2019], a widely used ecosystem of R packages. Thus, users familiar with the `{tidyverse}` can easily translate their knowledge and make full use of `{datawizard}`.

In addition to being a lightweight solution to clean messy data, `{datawizard}` also provides helpers for the other important step of data analysis: applying statistical transformations to the cleaned data while setting up statistical models. This includes various types of data standardization, normalization, rank-transformation, and adjustment. These transformations, although widely used, are not currently collectively implemented in a package in the R ecosystem, so `{datawizard}` can help new R users in finding the transformation they need.

Lastly, `{datawizard}` also provides a toolbox to create detailed summaries of data properties and distributions (e.g., tables of descriptive statistics for each variable). This is a common step in data analysis, but it is not available in base R or many modeling packages, so its inclusion makes `{datawizard}` a one-stop-shop for data preparation tasks.

# Features

## Data Preparation

The raw data is rarely in a state that it can be directly fed into a statistical model. It often needs to be modified in various ways. For example, columns need to be renamed or reshaped, certain portions of the data need to be filtered out, data scattered across multiple tables needs to be joined, etc. 

`{datawizard}` provides various functions for cleaning and preparing data (see Table 1).

| Function         | Operation                             |
| :--------------- | :------------------------------------ |
| `data_filter()`  | to select only certain *observations* |
| `data_select()`  | to select only certain *variables*    |
| `data_extract()` | to extract a single *variable*        |
| `data_rename()`  | to rename variables                   |
| `data_to_long()` | to convert data from wide to long     |
| `data_to_wide()` | to convert data from long to wide     |
| `data_join()`    | to join two data frames               |
| ...              | ...                                   |

Table: The table below lists a few key functions offered by `{datawizard}` for data wrangling. To see the full list, see the package website: <https://easystats.github.io/datawizard/>

We will look at one example function that converts data in wide format to tidy/long format:


```r
stocks <- data.frame(
  time = as.Date("2009-01-01") + 0:4,
  X = rnorm(5, 0, 1),
  Y = rnorm(5, 0, 2)
)

stocks
#>         time           X          Y
#> 1 2009-01-01 -0.91474184 -0.5654808
#> 2 2009-01-02  1.00124785 -1.5270177
#> 3 2009-01-03 -0.05642291 -1.3700199
#> 4 2009-01-04  0.29664516  0.7341479
#> 5 2009-01-05 -2.79147086  0.3659937

data_to_long(
  stocks,
  select = -c("time"),
  names_to = "stock",
  values_to = "price"
)
#>          time stock       price
#> 1  2009-01-01     X -0.91474184
#> 2  2009-01-01     Y -0.56548082
#> 3  2009-01-02     X  1.00124785
#> 4  2009-01-02     Y -1.52701766
#> 5  2009-01-03     X -0.05642291
#> 6  2009-01-03     Y -1.37001987
#> 7  2009-01-04     X  0.29664516
#> 8  2009-01-04     Y  0.73414790
#> 9  2009-01-05     X -2.79147086
#> 10 2009-01-05     Y  0.36599370
```

## Statistical Transformations

Even after getting the raw data in the needed format, we may need to transform certain variables further to meet requirements imposed by a statistical test.

`{datawizard}` provides a rich collection of such functions for transforming variables (see Table 2).

| Function          | Operation                                    |
| :---------------- | :------------------------------------------- |
| `standardize()`   | to center and scale data                     |
| `normalize()`     | to scale variables to 0-1 range              |
| `adjust()`        | to adjust data for effect of other variables |
| `slide()`         | to shift numeric value range                 |
| `ranktransform()` | to convert numeric values to integer ranks   |
| ...               | ...                                          |

Table: The table below lists a few key functions offered by `{datawizard}` for data transformations. To see the full list, see the package website: <https://easystats.github.io/datawizard/>

We will look at one example function that standardizes (i.e. centers and scales) data so that it can be expressed in terms of standard deviation:


```r
d <- data.frame(
  a = c(-2, -1, 0, 1, 2),
  b = c(3, 4, 5, 6, 7)
)

standardize(d, center = c(3, 4), scale = c(2, 4))
#>      a     b
#> 1 -2.5 -0.25
#> 2 -2.0  0.00
#> 3 -1.5  0.25
#> 4 -1.0  0.50
#> 5 -0.5  0.75
```

## Summaries of Data Properties and Distributions

The workhorse function to get a comprehensive summary of data properties is `describe_distribution()`, which combines a set of indices (e.g., measures of centrality, dispersion, range, skewness, kurtosis, etc.) computed by other functions in `{datawizard}`.


```r
describe_distribution(mtcars)
```


\begin{tabular}[t]{lrrrrrrrrr}
\toprule
Variable & Mean & SD & IQR & Min & Max & Skewness & Kurtosis & n & n\_Missing\\
\midrule
mpg & 20.09 & 6.03 & 7.53 & 10.4 & 33.9 & 0.67 & -0.02 & 32 & 0\\
cyl & 6.19 & 1.79 & 4.00 & 4.0 & 8.0 & -0.19 & -1.76 & 32 & 0\\
disp & 230.72 & 123.94 & 221.52 & 71.1 & 472.0 & 0.42 & -1.07 & 32 & 0\\
hp & 146.69 & 68.56 & 84.50 & 52.0 & 335.0 & 0.80 & 0.28 & 32 & 0\\
drat & 3.60 & 0.53 & 0.84 & 2.8 & 4.9 & 0.29 & -0.45 & 32 & 0\\
wt & 3.22 & 0.98 & 1.19 & 1.5 & 5.4 & 0.47 & 0.42 & 32 & 0\\
qsec & 17.85 & 1.79 & 2.02 & 14.5 & 22.9 & 0.41 & 0.86 & 32 & 0\\
vs & 0.44 & 0.50 & 1.00 & 0.0 & 1.0 & 0.26 & -2.06 & 32 & 0\\
am & 0.41 & 0.50 & 1.00 & 0.0 & 1.0 & 0.40 & -1.97 & 32 & 0\\
gear & 3.69 & 0.74 & 1.00 & 3.0 & 5.0 & 0.58 & -0.90 & 32 & 0\\
carb & 2.81 & 1.62 & 2.00 & 1.0 & 8.0 & 1.16 & 2.02 & 32 & 0\\
\bottomrule
\end{tabular}

# Licensing and Availability

`{datawizard}` is licensed under the GNU General Public License (v3.0), with all source code openly developed and stored on GitHub (<https://github.com/easystats/datawizard>), along with a corresponding issue tracker for bug reporting and feature enhancements. In the spirit of honest and open science, we encourage requests, tips for fixes, feature updates, as well as general questions and concerns via direct interaction with contributors and developers.

# Acknowledgments

`{datawizard}` is part of the collaborative [*easystats*](https://easystats.github.io/easystats/) ecosystem. Thus, we thank the [members of easystats](https://github.com/orgs/easystats/people) as well as the users.

# References
