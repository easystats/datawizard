# Convenient text formatting functionalities

Convenience functions to manipulate and format text.

## Usage

``` r
text_format(
  text,
  sep = ", ",
  last = " and ",
  width = NULL,
  enclose = NULL,
  ...
)

text_fullstop(text)

text_lastchar(text, n = 1)

text_concatenate(text, sep = ", ", last = " and ", enclose = NULL)

text_paste(text, text2 = NULL, sep = ", ", enclose = NULL, ...)

text_remove(text, pattern = "", ...)

text_wrap(text, width = NULL, ...)
```

## Arguments

- text, text2:

  A character string.

- sep:

  Separator.

- last:

  Last separator.

- width:

  Positive integer giving the target column width for wrapping lines in
  the output. Can be "auto", in which case it will select 90\\ default
  width.

- enclose:

  Character that will be used to wrap elements of `text`, so these can
  be, e.g., enclosed with quotes or backticks. If `NULL` (default), text
  elements will not be enclosed.

- ...:

  Other arguments to be passed to or from other functions.

- n:

  The number of characters to find.

- pattern:

  Regex pattern to remove from `text`.

## Value

A character string.

## Examples

``` r
# Add full stop if missing
text_fullstop(c("something", "something else."))
#> [1] "something."      "something else."

# Find last characters
text_lastchar(c("ABC", "DEF"), n = 2)
#>  ABC  DEF 
#> "BC" "EF" 

# Smart concatenation
text_concatenate(c("First", "Second", "Last"))
#> [1] "First, Second and Last"
text_concatenate(c("First", "Second", "Last"), last = " or ", enclose = "`")
#> [1] "`First`, `Second` or `Last`"

# Remove parts of string
text_remove(c("one!", "two", "three!"), "!")
#> [1] "one"   "two"   "three"

# Wrap text
long_text <- paste(rep("abc ", 100), collapse = "")
cat(text_wrap(long_text, width = 50))
#>  abc abc abc abc abc abc abc abc abc abc abc abc
#> abc abc abc abc abc abc abc abc abc abc abc abc
#> abc abc abc abc abc abc abc abc abc abc abc abc
#> abc abc abc abc abc abc abc abc abc abc abc abc
#> abc abc abc abc abc abc abc abc abc abc abc abc
#> abc abc abc abc abc abc abc abc abc abc abc abc
#> abc abc abc abc abc abc abc abc abc abc abc abc
#> abc abc abc abc abc abc abc abc abc abc abc abc
#> abc abc abc abc

# Paste with optional separator
text_paste(c("A", "", "B"), c("42", "42", "42"))
#> [1] "A, 42" "42"    "B, 42"
```
