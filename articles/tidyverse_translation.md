# Coming from 'tidyverse'

This vignette can be referred to by citing the following:

Patil et al., (2022). datawizard: An R Package for Easy Data Preparation
and Statistical Transformations. *Journal of Open Source Software*,
*7*(78), 4684, <https://doi.org/10.21105/joss.04684>

## Introduction

[datawizard](https://easystats.github.io/datawizard/) package aims to
make basic data wrangling easier than with base R. The data wrangling
workflow it supports is similar to the one supported by the tidyverse
package combination of [dplyr](https://dplyr.tidyverse.org) and
[tidyr](https://tidyr.tidyverse.org). However, one of its main features
is that it has a very few dependencies: `{stats}` and `{utils}`
(included in base R) and
[insight](https://easystats.github.io/insight/), which is the core
package of the *easystats* ecosystem. This package grew organically to
simultaneously satisfy the “0 non-base hard dependency” principle of
*easystats* and the data wrangling needs of the constituent packages in
this ecosystem. It is also important to note that
[datawizard](https://easystats.github.io/datawizard/) was designed to
avoid namespace collisions with
[tidyverse](https://tidyverse.tidyverse.org) packages.

In this article, we will see how to go through basic data wrangling
steps with [datawizard](https://easystats.github.io/datawizard/). We
will also compare it to the [tidyverse](https://tidyverse.tidyverse.org)
syntax for achieving the same. This way, if you decide to make the
switch, you can easily find the translations here. This vignette is
largely inspired from [dplyr](https://dplyr.tidyverse.org)’s [Getting
started vignette](https://dplyr.tidyverse.org/articles/dplyr.html).

Note: In this vignette, we use the native pipe-operator, `|>`, which was
introduced in R 4.1. Users of R version 3.6 or 4.0 should replace the
native pipe by magrittr’s one (`%>%`) so that examples work.

``` r

library(dplyr)
library(tidyr)
library(datawizard)

data(efc)
efc <- head(efc)
```

## Workhorses

Before we look at their *tidyverse* equivalents, we can first have a
look at [datawizard](https://easystats.github.io/datawizard/)’s key
functions for data wrangling:

| Function | Operation |
|:---|:---|
| [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md) | [to select only certain observations](#filtering) |
| [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md) | [to select only a few variables](#selecting) |
| [`data_modify()`](https://easystats.github.io/datawizard/reference/data_modify.md) | [to create variables or modify existing ones](#modifying) |
| [`data_arrange()`](https://easystats.github.io/datawizard/reference/data_arrange.md) | [to sort observations](#sorting) |
| [`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md) | [to extract a single variable](#extracting) |
| [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md) | [to rename variables](#renaming) |
| [`data_relocate()`](https://easystats.github.io/datawizard/reference/data_relocate.md) | [to reorder a data frame](#relocating) |
| [`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md) | [to convert data from wide to long](#reshaping) |
| [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md) | [to convert data from long to wide](#reshaping) |
| [`data_join()`](https://easystats.github.io/datawizard/reference/data_merge.md) | [to join two data frames](#joining) |
| [`data_unite()`](https://easystats.github.io/datawizard/reference/data_unite.md) | [to concatenate several columns into a single one](#uniting) |
| [`data_separate()`](https://easystats.github.io/datawizard/reference/data_separate.md) | [to separate a single column into multiple columns](#separating) |

Note that there are a few functions in
[datawizard](https://easystats.github.io/datawizard/) that have no
strict equivalent in [dplyr](https://dplyr.tidyverse.org) or
[tidyr](https://tidyr.tidyverse.org) (e.g
[`data_rotate()`](https://easystats.github.io/datawizard/reference/data_rotate.md)),
and so we won’t discuss them in the next section.

## Equivalence with `{dplyr}` / `{tidyr}`

Before we look at them individually, let’s first have a look at the
summary table of this equivalence.

| Function | Tidyverse equivalent(s) |
|:---|:---|
| [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md) | [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html), [`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html) |
| [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md) | [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html) |
| [`data_modify()`](https://easystats.github.io/datawizard/reference/data_modify.md) | [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) |
| [`data_arrange()`](https://easystats.github.io/datawizard/reference/data_arrange.md) | [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) |
| [`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md) | [`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html) |
| [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md) | [`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html) |
| [`data_relocate()`](https://easystats.github.io/datawizard/reference/data_relocate.md) | [`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html) |
| [`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md) | [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html) |
| [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md) | [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html) |
| [`data_join()`](https://easystats.github.io/datawizard/reference/data_merge.md) | [`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html), [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html), [`dplyr::right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html), |
|  | [`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html), [`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html), [`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html) |
| [`data_peek()`](https://easystats.github.io/datawizard/reference/data_peek.md) | [`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) |
| [`data_unite()`](https://easystats.github.io/datawizard/reference/data_unite.md) | [`tidyr::unite()`](https://tidyr.tidyverse.org/reference/unite.html) |
| [`data_separate()`](https://easystats.github.io/datawizard/reference/data_separate.md) | [`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html) |

### Filtering

[`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)
is a wrapper around [`subset()`](https://rdrr.io/r/base/subset.html).
However, if you want to have several filtering conditions, you can
either use `&` (as in [`subset()`](https://rdrr.io/r/base/subset.html))
or `,` (as in
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)).

``` r

# ---------- datawizard -----------
starwars |>
  data_filter(
    skin_color == "light",
    eye_color == "brown"
  )

# or
starwars |>
  data_filter(
    skin_color == "light" &
      eye_color == "brown"
  )
```

``` r

# ---------- tidyverse -----------
starwars |>
  filter(
    skin_color == "light",
    eye_color == "brown"
  )
```

    ## # A tibble: 7 × 14
    ##   name      height  mass hair_color skin_color eye_color birth_year sex   gender
    ## * <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
    ## 1 Leia Org…    150    49 brown      light      brown             19 fema… femin…
    ## 2 Biggs Da…    183    84 black      light      brown             24 male  mascu…
    ## 3 Padmé Am…    185    45 brown      light      brown             46 fema… femin…
    ## 4 Cordé        157    NA brown      light      brown             NA NA    NA    
    ## 5 Dormé        165    NA brown      light      brown             NA fema… femin…
    ## 6 Raymus A…    188    79 brown      light      brown             NA male  mascu…
    ## 7 Poe Dame…     NA    NA brown      light      brown             NA male  mascu…
    ## # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
    ## #   vehicles <list>, starships <list>

    ## # A tibble: 7 × 14
    ##   name      height  mass hair_color skin_color eye_color birth_year sex   gender
    ## * <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
    ## 1 Leia Org…    150    49 brown      light      brown             19 fema… femin…
    ## 2 Biggs Da…    183    84 black      light      brown             24 male  mascu…
    ## 3 Padmé Am…    185    45 brown      light      brown             46 fema… femin…
    ## 4 Cordé        157    NA brown      light      brown             NA NA    NA    
    ## 5 Dormé        165    NA brown      light      brown             NA fema… femin…
    ## 6 Raymus A…    188    79 brown      light      brown             NA male  mascu…
    ## 7 Poe Dame…     NA    NA brown      light      brown             NA male  mascu…
    ## # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
    ## #   vehicles <list>, starships <list>

### Selecting

[`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
is the equivalent of
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).
The main difference between these two functions is that
[`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
uses two arguments (`select` and `exclude`) and requires quoted column
names if we want to select several variables, while
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
accepts any unquoted column names.

``` r

# ---------- datawizard -----------
starwars |>
  data_select(select = c("hair_color", "skin_color", "eye_color"))
```

``` r

# ---------- tidyverse -----------
starwars |>
  select(hair_color, skin_color, eye_color)
```

    ## # A tibble: 6 × 3
    ##   hair_color  skin_color  eye_color
    ##   <chr>       <chr>       <chr>    
    ## 1 blond       fair        blue     
    ## 2 NA          gold        yellow   
    ## 3 NA          white, blue red      
    ## 4 none        white       yellow   
    ## 5 brown       light       brown    
    ## 6 brown, grey light       blue

``` r

# ---------- datawizard -----------
starwars |>
  data_select(select = -ends_with("color"))
```

``` r

# ---------- tidyverse -----------
starwars |>
  select(-ends_with("color"))
```

    ## # A tibble: 6 × 11
    ##   name     height  mass birth_year sex   gender homeworld species films vehicles
    ##   <chr>     <int> <dbl>      <dbl> <chr> <chr>  <chr>     <chr>   <lis> <list>  
    ## 1 Luke Sk…    172    77       19   male  mascu… Tatooine  Human   <chr> <chr>   
    ## 2 C-3PO       167    75      112   none  mascu… Tatooine  Droid   <chr> <chr>   
    ## 3 R2-D2        96    32       33   none  mascu… Naboo     Droid   <chr> <chr>   
    ## 4 Darth V…    202   136       41.9 male  mascu… Tatooine  Human   <chr> <chr>   
    ## 5 Leia Or…    150    49       19   fema… femin… Alderaan  Human   <chr> <chr>   
    ## 6 Owen La…    178   120       52   male  mascu… Tatooine  Human   <chr> <chr>   
    ## # ℹ 1 more variable: starships <list>

``` r

# ---------- datawizard -----------
starwars |>
  data_select(select = -(hair_color:eye_color))
```

``` r

# ---------- tidyverse -----------
starwars |>
  select(!(hair_color:eye_color))
```

    ## # A tibble: 6 × 11
    ##   name     height  mass birth_year sex   gender homeworld species films vehicles
    ##   <chr>     <int> <dbl>      <dbl> <chr> <chr>  <chr>     <chr>   <lis> <list>  
    ## 1 Luke Sk…    172    77       19   male  mascu… Tatooine  Human   <chr> <chr>   
    ## 2 C-3PO       167    75      112   none  mascu… Tatooine  Droid   <chr> <chr>   
    ## 3 R2-D2        96    32       33   none  mascu… Naboo     Droid   <chr> <chr>   
    ## 4 Darth V…    202   136       41.9 male  mascu… Tatooine  Human   <chr> <chr>   
    ## 5 Leia Or…    150    49       19   fema… femin… Alderaan  Human   <chr> <chr>   
    ## 6 Owen La…    178   120       52   male  mascu… Tatooine  Human   <chr> <chr>   
    ## # ℹ 1 more variable: starships <list>

``` r

# ---------- datawizard -----------
starwars |>
  data_select(exclude = regex("color$"))
```

``` r

# ---------- tidyverse -----------
starwars |>
  select(-contains("color$"))
```

    ## # A tibble: 6 × 11
    ##   name     height  mass birth_year sex   gender homeworld species films vehicles
    ##   <chr>     <int> <dbl>      <dbl> <chr> <chr>  <chr>     <chr>   <lis> <list>  
    ## 1 Luke Sk…    172    77       19   male  mascu… Tatooine  Human   <chr> <chr>   
    ## 2 C-3PO       167    75      112   none  mascu… Tatooine  Droid   <chr> <chr>   
    ## 3 R2-D2        96    32       33   none  mascu… Naboo     Droid   <chr> <chr>   
    ## 4 Darth V…    202   136       41.9 male  mascu… Tatooine  Human   <chr> <chr>   
    ## 5 Leia Or…    150    49       19   fema… femin… Alderaan  Human   <chr> <chr>   
    ## 6 Owen La…    178   120       52   male  mascu… Tatooine  Human   <chr> <chr>   
    ## # ℹ 1 more variable: starships <list>

``` r

# ---------- datawizard -----------
starwars |>
  data_select(select = is.numeric)
```

``` r

# ---------- tidyverse -----------
starwars |>
  select(where(is.numeric))
```

    ## # A tibble: 6 × 3
    ##   height  mass birth_year
    ##    <int> <dbl>      <dbl>
    ## 1    172    77       19  
    ## 2    167    75      112  
    ## 3     96    32       33  
    ## 4    202   136       41.9
    ## 5    150    49       19  
    ## 6    178   120       52

You can find a list of all the select helpers with
[`?data_select`](https://easystats.github.io/datawizard/reference/extract_column_names.md).

### Modifying

[`data_modify()`](https://easystats.github.io/datawizard/reference/data_modify.md)
is a wrapper around
[`base::transform()`](https://rdrr.io/r/base/transform.html) but has
several additional benefits:

- it allows us to use newly created variables in the following
  expressions;
- it works with grouped data;
- it preserves variable attributes such as labels;
- it accepts expressions as character vectors so that it is easy to
  program with it

This last point is also the main difference between
[`data_modify()`](https://easystats.github.io/datawizard/reference/data_modify.md)
and
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

``` r

# ---------- datawizard -----------
efc |>
  data_modify(
    c12hour_c = center(c12hour),
    c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE),
    c12hour_z2 = standardize(c12hour)
  )
```

``` r

# ---------- tidyverse -----------
efc |>
  mutate(
    c12hour_c = center(c12hour),
    c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE),
    c12hour_z2 = standardize(c12hour)
  )
```

    ##   c12hour e16sex e42dep c172code neg_c_7 c12hour_c  c12hour_z c12hour_z2
    ## 1      16      2      3        2      12     -67.6 -0.9420928 -0.9420928
    ## 2     148      2      3        2      20      64.4  0.8974967  0.8974967
    ## 3      70      2      3        1      11     -13.6 -0.1895335 -0.1895335
    ## 4      NA      2   <NA>        2      10        NA         NA         NA
    ## 5     168      2      4        2      12      84.4  1.1762224  1.1762224
    ## 6      16      2      4        2      19     -67.6 -0.9420928 -0.9420928

[`data_modify()`](https://easystats.github.io/datawizard/reference/data_modify.md)
supports expressions as strings via its `as_expr()` helper function.

``` r

new_exp <- c(
  "c12hour_c = center(c12hour)",
  "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)"
)
data_modify(efc, as_expr(new_exp))
```

    ##   c12hour e16sex e42dep c172code neg_c_7 c12hour_c  c12hour_z
    ## 1      16      2      3        2      12     -67.6 -0.9420928
    ## 2     148      2      3        2      20      64.4  0.8974967
    ## 3      70      2      3        1      11     -13.6 -0.1895335
    ## 4      NA      2   <NA>        2      10        NA         NA
    ## 5     168      2      4        2      12      84.4  1.1762224
    ## 6      16      2      4        2      19     -67.6 -0.9420928

This makes it easy to use it in custom functions:

``` r

miles_to_km <- function(data, var) {
  data_modify(
    data,
    as_expr(paste0("km = ", var, "* 1.609344"))
  )
}

distance <- data.frame(miles = c(1, 8, 233, 88, 9))
distance
```

    ##   miles
    ## 1     1
    ## 2     8
    ## 3   233
    ## 4    88
    ## 5     9

``` r

miles_to_km(distance, "miles")
```

    ##   miles         km
    ## 1     1   1.609344
    ## 2     8  12.874752
    ## 3   233 374.977152
    ## 4    88 141.622272
    ## 5     9  14.484096

### Sorting

[`data_arrange()`](https://easystats.github.io/datawizard/reference/data_arrange.md)
is the equivalent of
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html).
It takes two arguments: a data frame, and a vector of column names used
to sort the rows. Note that contrary to most other functions in
[datawizard](https://easystats.github.io/datawizard/), it is not
possible to use select helpers such as
[`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html)
in
[`data_arrange()`](https://easystats.github.io/datawizard/reference/data_arrange.md).

``` r

# ---------- datawizard -----------
starwars |>
  data_arrange(c("hair_color", "height"))
```

``` r

# ---------- tidyverse -----------
starwars |>
  arrange(hair_color, height)
```

    ##             name height mass  hair_color  skin_color eye_color birth_year
    ## 1 Luke Skywalker    172   77       blond        fair      blue       19.0
    ## 2    Leia Organa    150   49       brown       light     brown       19.0
    ## 3      Owen Lars    178  120 brown, grey       light      blue       52.0
    ## 4    Darth Vader    202  136        none       white    yellow       41.9
    ## 5          R2-D2     96   32        <NA> white, blue       red       33.0
    ## 6          C-3PO    167   75        <NA>        gold    yellow      112.0
    ##      sex    gender homeworld species
    ## 1   male masculine  Tatooine   Human
    ## 2 female  feminine  Alderaan   Human
    ## 3   male masculine  Tatooine   Human
    ## 4   male masculine  Tatooine   Human
    ## 5   none masculine     Naboo   Droid
    ## 6   none masculine  Tatooine   Droid
    ##                                                                                                                                       films
    ## 1                                           A New Hope, The Empire Strikes Back, Return of the Jedi, Revenge of the Sith, The Force Awakens
    ## 2                                           A New Hope, The Empire Strikes Back, Return of the Jedi, Revenge of the Sith, The Force Awakens
    ## 3                                                                                     A New Hope, Attack of the Clones, Revenge of the Sith
    ## 4                                                              A New Hope, The Empire Strikes Back, Return of the Jedi, Revenge of the Sith
    ## 5 A New Hope, The Empire Strikes Back, Return of the Jedi, The Phantom Menace, Attack of the Clones, Revenge of the Sith, The Force Awakens
    ## 6                    A New Hope, The Empire Strikes Back, Return of the Jedi, The Phantom Menace, Attack of the Clones, Revenge of the Sith
    ##                             vehicles                starships
    ## 1 Snowspeeder, Imperial Speeder Bike X-wing, Imperial shuttle
    ## 2              Imperial Speeder Bike                         
    ## 3                                                            
    ## 4                                             TIE Advanced x1
    ## 5                                                            
    ## 6

You can also sort variables in descending order by putting a `"-"` in
front of their name, like below:

``` r

# ---------- datawizard -----------
starwars |>
  data_arrange(c("-hair_color", "-height"))
```

``` r

# ---------- tidyverse -----------
starwars |>
  arrange(desc(hair_color), -height)
```

    ##             name height mass  hair_color  skin_color eye_color birth_year
    ## 1    Darth Vader    202  136        none       white    yellow       41.9
    ## 2      Owen Lars    178  120 brown, grey       light      blue       52.0
    ## 3    Leia Organa    150   49       brown       light     brown       19.0
    ## 4 Luke Skywalker    172   77       blond        fair      blue       19.0
    ## 5          C-3PO    167   75        <NA>        gold    yellow      112.0
    ## 6          R2-D2     96   32        <NA> white, blue       red       33.0
    ##      sex    gender homeworld species
    ## 1   male masculine  Tatooine   Human
    ## 2   male masculine  Tatooine   Human
    ## 3 female  feminine  Alderaan   Human
    ## 4   male masculine  Tatooine   Human
    ## 5   none masculine  Tatooine   Droid
    ## 6   none masculine     Naboo   Droid
    ##                                                                                                                                       films
    ## 1                                                              A New Hope, The Empire Strikes Back, Return of the Jedi, Revenge of the Sith
    ## 2                                                                                     A New Hope, Attack of the Clones, Revenge of the Sith
    ## 3                                           A New Hope, The Empire Strikes Back, Return of the Jedi, Revenge of the Sith, The Force Awakens
    ## 4                                           A New Hope, The Empire Strikes Back, Return of the Jedi, Revenge of the Sith, The Force Awakens
    ## 5                    A New Hope, The Empire Strikes Back, Return of the Jedi, The Phantom Menace, Attack of the Clones, Revenge of the Sith
    ## 6 A New Hope, The Empire Strikes Back, Return of the Jedi, The Phantom Menace, Attack of the Clones, Revenge of the Sith, The Force Awakens
    ##                             vehicles                starships
    ## 1                                             TIE Advanced x1
    ## 2                                                            
    ## 3              Imperial Speeder Bike                         
    ## 4 Snowspeeder, Imperial Speeder Bike X-wing, Imperial shuttle
    ## 5                                                            
    ## 6

### Extracting

Although we mostly work on data frames, it is sometimes useful to
extract a single column as a vector. This can be done with
[`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md),
which reproduces the behavior of
[`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html):

``` r

# ---------- datawizard -----------
starwars |>
  data_extract(gender)
```

``` r

# ---------- tidyverse -----------
starwars |>
  pull(gender)
```

    ## [1] "masculine" "masculine" "masculine" "masculine" "feminine"  "masculine"

We can also specify several variables in `select`. In this case,
[`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md)
is equivalent to
[`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md):

``` r

starwars |>
  data_extract(select = contains("color"))
```

    ## # A tibble: 6 × 3
    ##   hair_color  skin_color  eye_color
    ##   <chr>       <chr>       <chr>    
    ## 1 blond       fair        blue     
    ## 2 NA          gold        yellow   
    ## 3 NA          white, blue red      
    ## 4 none        white       yellow   
    ## 5 brown       light       brown    
    ## 6 brown, grey light       blue

### Renaming

[`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
is the equivalent of
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
but the syntax between the two is different. While
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
takes new-old pairs of column names,
[`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
requires a vector of column names to rename, and then a vector of new
names for these columns that must be of the same length.

``` r

# ---------- datawizard -----------
starwars |>
  data_rename(
    select = c("sex", "hair_color"),
    replacement = c("Sex", "Hair Color")
  )
```

``` r

# ---------- tidyverse -----------
starwars |>
  rename(
    Sex = sex,
    "Hair Color" = hair_color
  )
```

    ## # A tibble: 6 × 14
    ##   name    height  mass `Hair Color` skin_color eye_color birth_year Sex   gender
    ##   <chr>    <int> <dbl> <chr>        <chr>      <chr>          <dbl> <chr> <chr> 
    ## 1 Luke S…    172    77 blond        fair       blue            19   male  mascu…
    ## 2 C-3PO      167    75 NA           gold       yellow         112   none  mascu…
    ## 3 R2-D2       96    32 NA           white, bl… red             33   none  mascu…
    ## 4 Darth …    202   136 none         white      yellow          41.9 male  mascu…
    ## 5 Leia O…    150    49 brown        light      brown           19   fema… femin…
    ## 6 Owen L…    178   120 brown, grey  light      blue            52   male  mascu…
    ## # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
    ## #   vehicles <list>, starships <list>

The way
[`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
is designed makes it easy to apply the same modifications to a vector of
column names. For example, we can remove underscores and use TitleCase
with the following code:

``` r

to_rename <- names(starwars)

starwars |>
  data_rename(
    select = to_rename,
    replacement = tools::toTitleCase(gsub("_", " ", to_rename, fixed = TRUE))
  )
```

    ## # A tibble: 6 × 14
    ##   Name     Height  Mass `Hair Color` `Skin Color` `Eye Color` `Birth Year` Sex  
    ##   <chr>     <int> <dbl> <chr>        <chr>        <chr>              <dbl> <chr>
    ## 1 Luke Sk…    172    77 blond        fair         blue                19   male 
    ## 2 C-3PO       167    75 NA           gold         yellow             112   none 
    ## 3 R2-D2        96    32 NA           white, blue  red                 33   none 
    ## 4 Darth V…    202   136 none         white        yellow              41.9 male 
    ## 5 Leia Or…    150    49 brown        light        brown               19   fema…
    ## 6 Owen La…    178   120 brown, grey  light        blue                52   male 
    ## # ℹ 6 more variables: Gender <chr>, Homeworld <chr>, Species <chr>,
    ## #   Films <list>, Vehicles <list>, Starships <list>

It is also possible to add a prefix or a suffix to all or a subset of
variables with
[`data_addprefix()`](https://easystats.github.io/datawizard/reference/data_prefix_suffix.md)
and
[`data_addsuffix()`](https://easystats.github.io/datawizard/reference/data_prefix_suffix.md).
The argument `select` accepts all select helpers that we saw above with
[`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md):

``` r

starwars |>
  data_addprefix(
    pattern = "OLD.",
    select = contains("color")
  ) |>
  data_addsuffix(
    pattern = ".NEW",
    select = -contains("color")
  )
```

    ## # A tibble: 6 × 14
    ##   name.NEW       height.NEW mass.NEW OLD.hair_color OLD.skin_color OLD.eye_color
    ##   <chr>               <int>    <dbl> <chr>          <chr>          <chr>        
    ## 1 Luke Skywalker        172       77 blond          fair           blue         
    ## 2 C-3PO                 167       75 NA             gold           yellow       
    ## 3 R2-D2                  96       32 NA             white, blue    red          
    ## 4 Darth Vader           202      136 none           white          yellow       
    ## 5 Leia Organa           150       49 brown          light          brown        
    ## 6 Owen Lars             178      120 brown, grey    light          blue         
    ## # ℹ 8 more variables: birth_year.NEW <dbl>, sex.NEW <chr>, gender.NEW <chr>,
    ## #   homeworld.NEW <chr>, species.NEW <chr>, films.NEW <list>,
    ## #   vehicles.NEW <list>, starships.NEW <list>

### Relocating

Sometimes, we want to relocate one or a small subset of columns in the
dataset. Rather than typing many names in
[`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md),
we can use
[`data_relocate()`](https://easystats.github.io/datawizard/reference/data_relocate.md),
which is the equivalent of
[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html).
Just like
[`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md),
we can specify a list of variables we want to relocate with `select` and
`exclude`. Then, the arguments `before` and `after`[^1] specify where
the selected columns should be relocated:

``` r

# ---------- datawizard -----------
starwars |>
  data_relocate(sex:homeworld, before = "height")
```

``` r

# ---------- tidyverse -----------
starwars |>
  relocate(sex:homeworld, .before = height)
```

    ## # A tibble: 6 × 14
    ##   name       sex   gender homeworld height  mass hair_color skin_color eye_color
    ##   <chr>      <chr> <chr>  <chr>      <int> <dbl> <chr>      <chr>      <chr>    
    ## 1 Luke Skyw… male  mascu… Tatooine     172    77 blond      fair       blue     
    ## 2 C-3PO      none  mascu… Tatooine     167    75 NA         gold       yellow   
    ## 3 R2-D2      none  mascu… Naboo         96    32 NA         white, bl… red      
    ## 4 Darth Vad… male  mascu… Tatooine     202   136 none       white      yellow   
    ## 5 Leia Orga… fema… femin… Alderaan     150    49 brown      light      brown    
    ## 6 Owen Lars  male  mascu… Tatooine     178   120 brown, gr… light      blue     
    ## # ℹ 5 more variables: birth_year <dbl>, species <chr>, films <list>,
    ## #   vehicles <list>, starships <list>

In addition to column names, `before` and `after` accept column indices.
Finally, one can use `before = -1` to relocate the selected columns just
before the last column, or `after = -1` to relocate them after the last
column.

``` r

# ---------- datawizard -----------
starwars |>
  data_relocate(sex:homeworld, after = -1)
```

    ## # A tibble: 6 × 14
    ##   name     height  mass hair_color skin_color eye_color birth_year species films
    ##   <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>   <lis>
    ## 1 Luke Sk…    172    77 blond      fair       blue            19   Human   <chr>
    ## 2 C-3PO       167    75 NA         gold       yellow         112   Droid   <chr>
    ## 3 R2-D2        96    32 NA         white, bl… red             33   Droid   <chr>
    ## 4 Darth V…    202   136 none       white      yellow          41.9 Human   <chr>
    ## 5 Leia Or…    150    49 brown      light      brown           19   Human   <chr>
    ## 6 Owen La…    178   120 brown, gr… light      blue            52   Human   <chr>
    ## # ℹ 5 more variables: vehicles <list>, starships <list>, sex <chr>,
    ## #   gender <chr>, homeworld <chr>

### Reshaping

#### Longer

Reshaping data from wide to long or from long to wide format can be done
with
[`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md)
and
[`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md).
These functions were designed to match
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
and
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
arguments, so that the only thing to do is to change the function name.
However, not all of
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
and
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
features are available yet.

We will use the `relig_income` dataset, as in the [`{tidyr}`
vignette](https://tidyr.tidyverse.org/articles/pivot.html).

``` r

relig_income
```

    ## # A tibble: 18 × 11
    ##    religion `<$10k` `$10-20k` `$20-30k` `$30-40k` `$40-50k` `$50-75k` `$75-100k`
    ##    <chr>      <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>      <dbl>
    ##  1 Agnostic      27        34        60        81        76       137        122
    ##  2 Atheist       12        27        37        52        35        70         73
    ##  3 Buddhist      27        21        30        34        33        58         62
    ##  4 Catholic     418       617       732       670       638      1116        949
    ##  5 Don’t k…      15        14        15        11        10        35         21
    ##  6 Evangel…     575       869      1064       982       881      1486        949
    ##  7 Hindu          1         9         7         9        11        34         47
    ##  8 Histori…     228       244       236       238       197       223        131
    ##  9 Jehovah…      20        27        24        24        21        30         15
    ## 10 Jewish        19        19        25        25        30        95         69
    ## 11 Mainlin…     289       495       619       655       651      1107        939
    ## 12 Mormon        29        40        48        51        56       112         85
    ## 13 Muslim         6         7         9        10         9        23         16
    ## 14 Orthodox      13        17        23        32        32        47         38
    ## 15 Other C…       9         7        11        13        13        14         18
    ## 16 Other F…      20        33        40        46        49        63         46
    ## 17 Other W…       5         2         3         4         2         7          3
    ## 18 Unaffil…     217       299       374       365       341       528        407
    ## # ℹ 3 more variables: `$100-150k` <dbl>, `>150k` <dbl>,
    ## #   `Don't know/refused` <dbl>

We would like to reshape this dataset to have 3 columns: religion,
count, and income. The column “religion” doesn’t need to change, so we
exclude it with `-religion`. Then, each remaining column corresponds to
an income category. Therefore, we want to move all these column names to
a single column called “income”. Finally, the values corresponding to
each of these columns will be reshaped to be in a single new column,
called “count”.

``` r

# ---------- datawizard -----------
relig_income |>
  data_to_long(
    -religion,
    names_to = "income",
    values_to = "count"
  )
```

``` r

# ---------- tidyverse -----------
relig_income |>
  pivot_longer(
    !religion,
    names_to = "income",
    values_to = "count"
  )
```

    ## # A tibble: 180 × 3
    ##    religion income             count
    ##    <chr>    <chr>              <dbl>
    ##  1 Agnostic <$10k                 27
    ##  2 Agnostic $10-20k               34
    ##  3 Agnostic $20-30k               60
    ##  4 Agnostic $30-40k               81
    ##  5 Agnostic $40-50k               76
    ##  6 Agnostic $50-75k              137
    ##  7 Agnostic $75-100k             122
    ##  8 Agnostic $100-150k            109
    ##  9 Agnostic >150k                 84
    ## 10 Agnostic Don't know/refused    96
    ## # ℹ 170 more rows

To explore a bit more the arguments of
[`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md),
we will use another dataset: the `billboard` dataset.

``` r

billboard
```

    ## # A tibble: 317 × 79
    ##    artist     track date.entered   wk1   wk2   wk3   wk4   wk5   wk6   wk7   wk8
    ##    <chr>      <chr> <date>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 2 Pac      Baby… 2000-02-26      87    82    72    77    87    94    99    NA
    ##  2 2Ge+her    The … 2000-09-02      91    87    92    NA    NA    NA    NA    NA
    ##  3 3 Doors D… Kryp… 2000-04-08      81    70    68    67    66    57    54    53
    ##  4 3 Doors D… Loser 2000-10-21      76    76    72    69    67    65    55    59
    ##  5 504 Boyz   Wobb… 2000-04-15      57    34    25    17    17    31    36    49
    ##  6 98^0       Give… 2000-08-19      51    39    34    26    26    19     2     2
    ##  7 A*Teens    Danc… 2000-07-08      97    97    96    95   100    NA    NA    NA
    ##  8 Aaliyah    I Do… 2000-01-29      84    62    51    41    38    35    35    38
    ##  9 Aaliyah    Try … 2000-03-18      59    53    38    28    21    18    16    14
    ## 10 Adams, Yo… Open… 2000-08-26      76    76    74    69    68    67    61    58
    ## # ℹ 307 more rows
    ## # ℹ 68 more variables: wk9 <dbl>, wk10 <dbl>, wk11 <dbl>, wk12 <dbl>,
    ## #   wk13 <dbl>, wk14 <dbl>, wk15 <dbl>, wk16 <dbl>, wk17 <dbl>, wk18 <dbl>,
    ## #   wk19 <dbl>, wk20 <dbl>, wk21 <dbl>, wk22 <dbl>, wk23 <dbl>, wk24 <dbl>,
    ## #   wk25 <dbl>, wk26 <dbl>, wk27 <dbl>, wk28 <dbl>, wk29 <dbl>, wk30 <dbl>,
    ## #   wk31 <dbl>, wk32 <dbl>, wk33 <dbl>, wk34 <dbl>, wk35 <dbl>, wk36 <dbl>,
    ## #   wk37 <dbl>, wk38 <dbl>, wk39 <dbl>, wk40 <dbl>, wk41 <dbl>, wk42 <dbl>, …

``` r

# ---------- datawizard -----------
billboard |>
  data_to_long(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )
```

``` r

# ---------- tidyverse -----------
billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )
```

    ## # A tibble: 5,307 × 5
    ##    artist  track                   date.entered week   rank
    ##    <chr>   <chr>                   <date>       <chr> <dbl>
    ##  1 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk1      87
    ##  2 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk2      82
    ##  3 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk3      72
    ##  4 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk4      77
    ##  5 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk5      87
    ##  6 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk6      94
    ##  7 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk7      99
    ##  8 2Ge+her The Hardest Part Of ... 2000-09-02   wk1      91
    ##  9 2Ge+her The Hardest Part Of ... 2000-09-02   wk2      87
    ## 10 2Ge+her The Hardest Part Of ... 2000-09-02   wk3      92
    ## # ℹ 5,297 more rows

#### Wider

Once again, we use an example in the
[tidyr](https://tidyr.tidyverse.org) vignette to show how close
[`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
and
[`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
are:

``` r

fish_encounters
```

    ## # A tibble: 114 × 3
    ##    fish  station  seen
    ##    <fct> <fct>   <int>
    ##  1 4842  Release     1
    ##  2 4842  I80_1       1
    ##  3 4842  Lisbon      1
    ##  4 4842  Rstr        1
    ##  5 4842  Base_TD     1
    ##  6 4842  BCE         1
    ##  7 4842  BCW         1
    ##  8 4842  BCE2        1
    ##  9 4842  BCW2        1
    ## 10 4842  MAE         1
    ## # ℹ 104 more rows

``` r

# ---------- datawizard -----------
fish_encounters |>
  data_to_wide(
    names_from = "station",
    values_from = "seen"
  )
```

``` r

# ---------- tidyverse -----------
fish_encounters |>
  pivot_wider(
    names_from = station,
    values_from = seen
  )
```

    ## # A tibble: 19 × 12
    ##    fish  Release I80_1 Lisbon  Rstr Base_TD   BCE   BCW  BCE2  BCW2   MAE   MAW
    ##    <fct>   <int> <int>  <int> <int>   <int> <int> <int> <int> <int> <int> <int>
    ##  1 4842        1     1      1     1       1     1     1     1     1     1     1
    ##  2 4843        1     1      1     1       1     1     1     1     1     1     1
    ##  3 4844        1     1      1     1       1     1     1     1     1     1     1
    ##  4 4845        1     1      1     1       1    NA    NA    NA    NA    NA    NA
    ##  5 4847        1     1      1    NA      NA    NA    NA    NA    NA    NA    NA
    ##  6 4848        1     1      1     1      NA    NA    NA    NA    NA    NA    NA
    ##  7 4849        1     1     NA    NA      NA    NA    NA    NA    NA    NA    NA
    ##  8 4850        1     1     NA     1       1     1     1    NA    NA    NA    NA
    ##  9 4851        1     1     NA    NA      NA    NA    NA    NA    NA    NA    NA
    ## 10 4854        1     1     NA    NA      NA    NA    NA    NA    NA    NA    NA
    ## 11 4855        1     1      1     1       1    NA    NA    NA    NA    NA    NA
    ## 12 4857        1     1      1     1       1     1     1     1     1    NA    NA
    ## 13 4858        1     1      1     1       1     1     1     1     1     1     1
    ## 14 4859        1     1      1     1       1    NA    NA    NA    NA    NA    NA
    ## 15 4861        1     1      1     1       1     1     1     1     1     1     1
    ## 16 4862        1     1      1     1       1     1     1     1     1    NA    NA
    ## 17 4863        1     1     NA    NA      NA    NA    NA    NA    NA    NA    NA
    ## 18 4864        1     1     NA    NA      NA    NA    NA    NA    NA    NA    NA
    ## 19 4865        1     1      1    NA      NA    NA    NA    NA    NA    NA    NA

### Joining

In [datawizard](https://easystats.github.io/datawizard/), joining
datasets is done with
[`data_join()`](https://easystats.github.io/datawizard/reference/data_merge.md)
(or its alias
[`data_merge()`](https://easystats.github.io/datawizard/reference/data_merge.md)).
Contrary to [dplyr](https://dplyr.tidyverse.org), this unique function
takes care of all types of join, which are then specified inside the
function with the argument `join` (by default, `join = "left"`).

Below, we show how to perform the four most common joins: full, left,
right and inner. We will use the datasets `band_members`and
`band_instruments` provided by [dplyr](https://dplyr.tidyverse.org):

``` r

band_members
```

    ## # A tibble: 3 × 2
    ##   name  band   
    ##   <chr> <chr>  
    ## 1 Mick  Stones 
    ## 2 John  Beatles
    ## 3 Paul  Beatles

``` r

band_instruments
```

    ## # A tibble: 3 × 2
    ##   name  plays 
    ##   <chr> <chr> 
    ## 1 John  guitar
    ## 2 Paul  bass  
    ## 3 Keith guitar

#### Full join

``` r

# ---------- datawizard -----------
band_members |>
  data_join(band_instruments, join = "full")
```

``` r

# ---------- tidyverse -----------
band_members |>
  full_join(band_instruments)
```

    ## # A tibble: 4 × 3
    ##   name  band    plays 
    ## * <chr> <chr>   <chr> 
    ## 1 Mick  Stones  NA    
    ## 2 John  Beatles guitar
    ## 3 Paul  Beatles bass  
    ## 4 Keith NA      guitar

#### Left and right joins

``` r

# ---------- datawizard -----------
band_members |>
  data_join(band_instruments, join = "left")
```

``` r

# ---------- tidyverse -----------
band_members |>
  left_join(band_instruments)
```

    ## # A tibble: 3 × 3
    ##   name  band    plays 
    ## * <chr> <chr>   <chr> 
    ## 1 Mick  Stones  NA    
    ## 2 John  Beatles guitar
    ## 3 Paul  Beatles bass

``` r

# ---------- datawizard -----------
band_members |>
  data_join(band_instruments, join = "right")
```

``` r

# ---------- tidyverse -----------
band_members |>
  right_join(band_instruments)
```

    ## # A tibble: 3 × 3
    ##   name  band    plays 
    ## * <chr> <chr>   <chr> 
    ## 1 John  Beatles guitar
    ## 2 Paul  Beatles bass  
    ## 3 Keith NA      guitar

#### Inner join

``` r

# ---------- datawizard -----------
band_members |>
  data_join(band_instruments, join = "inner")
```

``` r

# ---------- tidyverse -----------
band_members |>
  inner_join(band_instruments)
```

    ## # A tibble: 2 × 3
    ##   name  band    plays 
    ## * <chr> <chr>   <chr> 
    ## 1 John  Beatles guitar
    ## 2 Paul  Beatles bass

### Uniting

Uniting variables is useful e.g to create unique indices by combining
several variables or to gather years, months, and days into a single
date.
[`data_unite()`](https://easystats.github.io/datawizard/reference/data_unite.md)
offers an interface very close to
[`tidyr::unite()`](https://tidyr.tidyverse.org/reference/unite.html):

``` r

test <- data.frame(
  year = 2002:2004,
  month = c("02", "03", "09"),
  day = c("11", "22", "28"),
  stringsAsFactors = FALSE
)
test
```

    ##   year month day
    ## 1 2002    02  11
    ## 2 2003    03  22
    ## 3 2004    09  28

``` r

# ---------- datawizard -----------
test |>
  data_unite(
    new_column = "date",
    select = c("year", "month", "day"),
    separator = "-"
  )
```

``` r

# ---------- tidyverse -----------
test |>
  unite(
    col = "date",
    year, month, day,
    sep = "-"
  )
```

    ##         date
    ## 1 2002-02-11
    ## 2 2003-03-22
    ## 3 2004-09-28

``` r

# ---------- datawizard -----------
test |>
  data_unite(
    new_column = "date",
    select = c("year", "month", "day"),
    separator = "-",
    append = TRUE
  )
```

``` r

# ---------- tidyverse -----------
test |>
  unite(
    col = "date",
    year, month, day,
    sep = "-",
    remove = FALSE
  )
```

    ##   year month day       date
    ## 1 2002    02  11 2002-02-11
    ## 2 2003    03  22 2003-03-22
    ## 3 2004    09  28 2004-09-28

### Separating

Separating variables is the counterpart to uniting variables and is
useful to split values into multiple columns, e.g. when splitting a date
into values for years, months and days.
[`data_separate()`](https://easystats.github.io/datawizard/reference/data_separate.md)
offers an interface very close to
[`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html):

``` r

test <- data.frame(
  date_arrival = c("2002-02-11", "2003-03-22", "2004-09-28"),
  date_departure = c("2002-03-15", "2003-03-28", "2004-09-30"),
  stringsAsFactors = FALSE
)
test
```

    ##   date_arrival date_departure
    ## 1   2002-02-11     2002-03-15
    ## 2   2003-03-22     2003-03-28
    ## 3   2004-09-28     2004-09-30

``` r

# ---------- datawizard -----------
test |>
  data_separate(
    select = "date_arrival",
    new_columns = c("Year", "Month", "Day")
  )
```

``` r

# ---------- tidyverse -----------
test |>
  separate(
    date_arrival,
    into = c("Year", "Month", "Day")
  )
```

    ##   date_departure Year Month Day
    ## 1     2002-03-15 2002    02  11
    ## 2     2003-03-28 2003    03  22
    ## 3     2004-09-30 2004    09  28

Unlike
[`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html),
you can separate multiple columns in one step with
[`data_separate()`](https://easystats.github.io/datawizard/reference/data_separate.md).

``` r

test |>
  data_separate(
    new_columns = list(
      date_arrival = c("Arr_Year", "Arr_Month", "Arr_Day"),
      date_departure = c("Dep_Year", "Dep_Month", "Dep_Day")
    )
  )
```

    ##   Arr_Year Arr_Month Arr_Day Dep_Year Dep_Month Dep_Day
    ## 1     2002        02      11     2002        03      15
    ## 2     2003        03      22     2003        03      28
    ## 3     2004        09      28     2004        09      30

## Other useful functions

[datawizard](https://easystats.github.io/datawizard/) contains other
functions that are not necessarily included in
[dplyr](https://dplyr.tidyverse.org) or
[tidyr](https://tidyr.tidyverse.org) or do not directly modify the data.
Some of them are inspired from the package `janitor`.

### Work with rownames

We can convert a column in rownames and move rownames to a new column
with
[`rownames_as_column()`](https://easystats.github.io/datawizard/reference/rownames.md)
and
[`column_as_rownames()`](https://easystats.github.io/datawizard/reference/rownames.md):

``` r

mtcars <- head(mtcars)
mtcars
```

    ##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

``` r

mtcars2 <- mtcars |>
  rownames_as_column(var = "model")

mtcars2
```

    ##               model  mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## 1         Mazda RX4 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## 2     Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## 3        Datsun 710 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ## 4    Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    ## 5 Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    ## 6           Valiant 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

``` r

mtcars2 |>
  column_as_rownames(var = "model")
```

    ##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

### Work with row ids

[`rowid_as_column()`](https://easystats.github.io/datawizard/reference/rownames.md)
is close but not identical to
[`tibble::rowid_to_column()`](https://tibble.tidyverse.org/reference/rownames.html).
The main difference is when we use it with grouped data. While
[`tibble::rowid_to_column()`](https://tibble.tidyverse.org/reference/rownames.html)
uses one distinct rowid for every row in the dataset,
[`rowid_as_column()`](https://easystats.github.io/datawizard/reference/rownames.md)
creates one id for every row *in each group*. Therefore, two rows in
different groups can have the same row id.

This means that
[`rowid_as_column()`](https://easystats.github.io/datawizard/reference/rownames.md)
is closer to using
[`n()`](https://dplyr.tidyverse.org/reference/context.html) in
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html), like
the following:

``` r

test <- data.frame(
  group = c("A", "A", "B", "B"),
  value = c(3, 5, 8, 1),
  stringsAsFactors = FALSE
)
test
```

    ##   group value
    ## 1     A     3
    ## 2     A     5
    ## 3     B     8
    ## 4     B     1

``` r

test |>
  data_group(group) |>
  tibble::rowid_to_column()
```

    ##   rowid group value
    ## 1     1     A     3
    ## 2     2     A     5
    ## 3     3     B     8
    ## 4     4     B     1

``` r

test |>
  data_group(group) |>
  rowid_as_column()
```

    ## # A tibble: 4 × 3
    ## # Groups:   group [2]
    ##   rowid group value
    ##   <int> <chr> <dbl>
    ## 1     1 A         3
    ## 2     2 A         5
    ## 3     1 B         8
    ## 4     2 B         1

``` r

test |>
  data_group(group) |>
  mutate(id = seq_len(n()))
```

    ## # A tibble: 4 × 3
    ## # Groups:   group [2]
    ##   group value    id
    ##   <chr> <dbl> <int>
    ## 1 A         3     1
    ## 2 A         5     2
    ## 3 B         8     1
    ## 4 B         1     2

### Work with column names

When dealing with messy data, it is sometimes useful to use a row as
column names, and vice versa. This can be done with
[`row_to_colnames()`](https://easystats.github.io/datawizard/reference/colnames.md)
and
[`colnames_to_row()`](https://easystats.github.io/datawizard/reference/colnames.md).

``` r

x <- data.frame(
  X_1 = c(NA, "Title", 1:3),
  X_2 = c(NA, "Title2", 4:6)
)
x
```

    ##     X_1    X_2
    ## 1  <NA>   <NA>
    ## 2 Title Title2
    ## 3     1      4
    ## 4     2      5
    ## 5     3      6

``` r

x2 <- x |>
  row_to_colnames(row = 2)
x2
```

    ##   Title Title2
    ## 1  <NA>   <NA>
    ## 3     1      4
    ## 4     2      5
    ## 5     3      6

``` r

x2 |>
  colnames_to_row()
```

    ##       x1     x2
    ## 1  Title Title2
    ## 11  <NA>   <NA>
    ## 3      1      4
    ## 4      2      5
    ## 5      3      6

### Take a quick look at the data

``` r

# ---------- datawizard -----------
data_peek(iris)
```

``` r

# ---------- tidyverse -----------
glimpse(iris)
```

    ## Data frame with 150 rows and 5 variables
    ## 
    ## Variable     | Type    | Values                                        
    ## -----------------------------------------------------------------------
    ## Sepal.Length | numeric | 5.1, 4.9, 4.7, 4.6, 5, 5.4, 4.6, 5, 4.4, ...  
    ## Sepal.Width  | numeric | 3.5, 3, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, ...
    ## Petal.Length | numeric | 1.4, 1.4, 1.3, 1.5, 1.4, 1.7, 1.4, 1.5, ...   
    ## Petal.Width  | numeric | 0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.3, 0.2, ...   
    ## Species      | factor  | setosa, setosa, setosa, setosa, setosa, ...

[^1]: Note that we use `before` and `after` whereas
    [`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
    uses `.before` and `.after`.
