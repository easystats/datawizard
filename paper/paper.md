---
title: "datawizard: An R Package for Easy Data Wrangling and Transformations"
tags:
  - R
  - easystats
authors:
- affiliation: 1
  name: Daniel Lüdecke
  orcid: 0000-0002-8895-3206
- affiliation: 2
  name: Dominique Makowski
  orcid: 0000-0001-5375-9967
- affiliation: 3
  name: Mattan S. Ben-Shachar
  orcid: 0000-0002-4287-4801
- affiliation: 4
  name: Brenton M. Wiernik
  orcid: 0000-0001-9560-6336
- affiliation: 5
  name: Etienne Bacher
  # orcid: 
- affiliation: 6
  name: Indrajeet Patil
  orcid: 0000-0003-1995-6531
  
affiliations:
- index: 1
  name:  University Medical Center Hamburg-Eppendorf, Germany
- index: 2
  name: Nanyang Technological University, Singapore
- index: 3
  name: Ben-Gurion University of the Negev, Israel
- index: 4
  name: Facebook
- index: 5
  name: Luxembourg Institute of Socio-Economic Research, Luxembourg
- index: 6
  name: esqLABS GmbH
  
date: "2022-07-04"
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
link-citations: yes
---



# Summary

The `{datawizard}` package in the R programming language [@base2021] provides a

# Statement of Need

The `{datawizard}` package makes basic data wrangling easier than with base R. Its workflow and syntax are designed to be similar to `{tidyverse}` (@Wickham2019), which is a widely used ecosystem of packages for data analysis, and, therefore, users familiar with this ecosystem can easily translate their knowledge. Naturally, one might wonder why recreate data wrangling functionality already present in `{tidyverse}`.

The `{easystats}` (@Ben-Shachar2020, @Lüdecke2020parameters, @Lüdecke2020performance, @Lüdecke2021see, @Lüdecke2019, @Makowski2019, @Makowski2020) is an ecosystem of packages designed to make statistical analysis easier in R. Importantly, in order to be lightweight, it follows a "0-external-hard-dependency" policy. Thus, while building this ecosystem, a new data wrangling package that relies only on base R needed to be created.
In effect, this package provides the data processing backend for this entire ecosystem.
In addition to its usefulness to the `{easystats}` ecosystem, it also provides *an* option for R users and package developers if they wish to keep their (recursive) dependency weight to a minimum (for other options, see @Dowle2021, @Eastwood2021, etc.).

In addition to providing functions to clean messy data, `{datawizard}` also provides helpers for the other important step of data analysis: transforming the cleaned data further for setting up statistical models. For example, one may need to standardize certain variables, normalize range of some variables, adjust the data for effect of some variables, etc.

Lastly, `{datawizard}` also provides a toolbox to create a detailed profile of data properties.

# Features

## Data wrangling

Function           | Operation                             |
------------------ | --------------------------------------|
`data_filter()`    | to select only certain *observations* |
`data_select()`    | to select only a few *attributes*     |
`data_extract()`   | to extract a single *attribute*       |
`data_rename()`    | to rename attributes                  |
`reshape_longer()` | to convert data from wide to long     |
`reshape_wider()`  | to convert data from long to wide     |
`data_join()`      | to join two data frames               |
    ...            |        ...                            |

Table: The table below lists a few key functions offered by *datawizard* for data wrangling. To see the full list, see the package website: <https://easystats.github.io/datawizard/>

## Data transformations

Function           | Operation                                     |
------------------ | ----------------------------------------------|
`standardize()`    | to center and scale data                      |
`normalize()`      | to scale variables to 0-1 range               |
`adjust()`         | to adjust data for effect of other variables  |
`data_shift()`     | to shift numeric value range                  |
`ranktransform()`  | to convert numeric values to integer ranks    |
    ...            |        ...                                    |

Table: The table below lists a few key functions offered by *datawizard* for data transformations. To see the full list, see the package website: <https://easystats.github.io/datawizard/>

## Data properties

The workhorse function to get a comprehensive summary of data properties is `describe_distribution()`, which combines a set of indices (e.g., measures of centrality, dispersion, range, skewness, kurtosis, etc.) computed by other functions in `{datawizard}`.


```r
describe_distribution(mtcars$wt)
#> Mean |   SD |  IQR |        Range | Skewness | Kurtosis |  n | n_Missing
#> ------------------------------------------------------------------------
#> 3.22 | 0.98 | 1.19 | [1.51, 5.42] |     0.47 |     0.42 | 32 |         0
```


# Licensing and Availability

*datawizard* is licensed under the GNU General Public License (v3.0), with all source code openly developed and stored at GitHub (<https://github.com/easystats/datawizard>), along with a corresponding issue tracker for bug reporting and feature enhancements. In the spirit of honest and open science, we encourage requests, tips for fixes, feature updates, as well as general questions and concerns via direct interaction with contributors and developers.

# Acknowledgments

*datawizard* is part of the collaborative [*easystats*](https://github.com/easystats/easystats) ecosystem. Thus, we thank the [members of easystats](https://github.com/orgs/easystats/people) as well as the users.

# References
