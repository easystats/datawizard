# Prepare objects for visualisation

This function prepares objects for visualisation by returning a list of
layers with data and geoms that can be easily plotted using for instance
`ggplot2`.

If the `see` package is installed, the call to `visualization_recipe()`
can be replaced by
[`plot()`](https://rdrr.io/r/graphics/plot.default.html), which will
internally call the former and then plot it using `ggplot`. The
resulting plot can be customized ad-hoc (by adding ggplot's geoms, theme
or specifications), or via some of the arguments of
`visualisation_recipe()` that control the aesthetic parameters.

See the specific documentation page for your object's class:

- modelbased:
  <https://easystats.github.io/modelbased/reference/visualisation_recipe.estimate_predicted.html>

- correlation:
  <https://easystats.github.io/correlation/reference/visualisation_recipe.easycormatrix.html>

## Usage

``` r
visualisation_recipe(x, ...)
```

## Arguments

- x:

  An `easystats` object.

- ...:

  Other arguments passed to other functions.
