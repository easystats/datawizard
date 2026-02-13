# Utility Function for Safe Prediction with `datawizard` transformers

This function allows for the use of (some of) `datawizard`'s
transformers inside a model formula. See examples below.\
\
Currently,
[`center()`](https://easystats.github.io/datawizard/reference/center.md),
[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md),
[`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md),
&
[`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md)
are supported.

## Usage

``` r
# S3 method for class 'dw_transformer'
makepredictcall(var, call)
```

## Arguments

- var:

  A variable.

- call:

  The term in the formula, as a call.

## Value

A replacement for `call` for the `predvars` attribute of the terms.

## See also

[`stats::makepredictcall()`](https://rdrr.io/r/stats/makepredictcall.html)

## Examples

``` r

data("mtcars")
train <- mtcars[1:30, ]
test <- mtcars[31:32, ]

m1 <- lm(mpg ~ center(hp), data = train)
predict(m1, newdata = test) # Data is "centered" before the prediction is made,
#> Maserati Bora    Volvo 142E 
#>      4.269496     22.911189 
# according to the center of the old data

m2 <- lm(mpg ~ standardize(hp), data = train)
m3 <- lm(mpg ~ scale(hp), data = train) # same as above
predict(m2, newdata = test) # Data is "standardized" before the prediction is made.
#> Maserati Bora    Volvo 142E 
#>      4.269496     22.911189 
predict(m3, newdata = test) # Data is "standardized" before the prediction is made.
#> Maserati Bora    Volvo 142E 
#>      4.269496     22.911189 


m4 <- lm(mpg ~ normalize(hp), data = mtcars)
m5 <- lm(mpg ~ rescale(hp, to = c(-3, 3)), data = mtcars)

(newdata <- data.frame(hp = c(range(mtcars$hp), 400))) # 400 is outside original range!
#>    hp
#> 1  52
#> 2 335
#> 3 400

model.frame(delete.response(terms(m4)), data = newdata)
#>   normalize(hp)
#> 1      0.000000
#> 2      1.000000
#> 3      1.229682
model.frame(delete.response(terms(m5)), data = newdata)
#>   rescale(hp, to = c(-3, 3))
#> 1                  -3.000000
#> 2                   3.000000
#> 3                   4.378092
```
