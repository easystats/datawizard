% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjust.R
\name{adjust}
\alias{adjust}
\alias{data_adjust}
\title{Adjust data for the effect of other variable(s)}
\usage{
adjust(
  data,
  effect = NULL,
  select = is.numeric,
  exclude = NULL,
  multilevel = FALSE,
  additive = FALSE,
  bayesian = FALSE,
  keep_intercept = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = FALSE
)

data_adjust(
  data,
  effect = NULL,
  select = is.numeric,
  exclude = NULL,
  multilevel = FALSE,
  additive = FALSE,
  bayesian = FALSE,
  keep_intercept = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{data}{A data frame.}

\item{effect}{Character vector of column names to be adjusted for (regressed
out). If \code{NULL} (the default), all variables will be selected.}

\item{select}{Variables that will be included when performing the required
tasks. Can be either
\itemize{
\item a variable specified as a literal variable name (e.g., \code{column_name}),
\item a string with the variable name (e.g., \code{"column_name"}), a character
vector of variable names (e.g., \code{c("col1", "col2", "col3")}), or a
character vector of variable names including ranges specified via \code{:}
(e.g., \code{c("col1:col3", "col5")}),
\item for some functions, like \code{data_select()} or \code{data_rename()}, \code{select} can
be a named character vector. In this case, the names are used to rename
the columns in the output data frame. See 'Details' in the related
functions to see where this option applies.
\item a formula with variable names (e.g., \code{~column_1 + column_2}),
\item a vector of positive integers, giving the positions counting from the left
(e.g. \code{1} or \code{c(1, 3, 5)}),
\item a vector of negative integers, giving the positions counting from the
right (e.g., \code{-1} or \code{-1:-3}),
\item one of the following select-helpers: \code{starts_with()}, \code{ends_with()},
\code{contains()}, a range using \code{:}, or \code{regex()}. \code{starts_with()},
\code{ends_with()}, and  \code{contains()} accept several patterns, e.g
\code{starts_with("Sep", "Petal")}. \code{regex()} can be used to define regular
expression patterns.
\item a function testing for logical conditions, e.g. \code{is.numeric()} (or
\code{is.numeric}), or any user-defined function that selects the variables
for which the function returns \code{TRUE} (like: \code{foo <- function(x) mean(x) > 3}),
\item ranges specified via literal variable names, select-helpers (except
\code{regex()}) and (user-defined) functions can be negated, i.e. return
non-matching elements, when prefixed with a \code{-}, e.g. \code{-ends_with()},
\code{-is.numeric} or \code{-(Sepal.Width:Petal.Length)}. \strong{Note:} Negation means
that matches are \emph{excluded}, and thus, the \code{exclude} argument can be
used alternatively. For instance, \code{select=-ends_with("Length")} (with
\code{-}) is equivalent to \code{exclude=ends_with("Length")} (no \code{-}). In case
negation should not work as expected, use the \code{exclude} argument instead.
}

If \code{NULL}, selects all columns. Patterns that found no matches are silently
ignored, e.g. \code{extract_column_names(iris, select = c("Species", "Test"))}
will just return \code{"Species"}.}

\item{exclude}{See \code{select}, however, column names matched by the pattern
from \code{exclude} will be excluded instead of selected. If \code{NULL} (the default),
excludes no columns.}

\item{multilevel}{If \code{TRUE}, the factors are included as random factors.
Else, if \code{FALSE} (default), they are included as fixed effects in the
simple regression model.}

\item{additive}{If \code{TRUE}, continuous variables as included as smooth terms
in additive models. The goal is to regress-out potential non-linear
effects.}

\item{bayesian}{If \code{TRUE}, the models are fitted under the Bayesian framework
using \code{rstanarm}.}

\item{keep_intercept}{If \code{FALSE} (default), the intercept of the model is
re-added. This avoids the centering around 0 that happens by default
when regressing out another variable (see the examples below for a
visual representation of this).}

\item{ignore_case}{Logical, if \code{TRUE} and when one of the select-helpers or
a regular expression is used in \code{select}, ignores lower/upper case in the
search pattern when matching against variable names.}

\item{regex}{Logical, if \code{TRUE}, the search pattern from \code{select} will be
treated as regular expression. When \code{regex = TRUE}, select \emph{must} be a
character string (or a variable containing a character string) and is not
allowed to be one of the supported select-helpers or a character vector
of length > 1. \code{regex = TRUE} is comparable to using one of the two
select-helpers, \code{select = contains()} or \code{select = regex()}, however,
since the select-helpers may not work when called from inside other
functions (see 'Details'), this argument may be used as workaround.}

\item{verbose}{Toggle warnings.}
}
\value{
A data frame comparable to \code{data}, with adjusted variables.
}
\description{
This function can be used to adjust the data for the effect of other
variables present in the dataset. It is based on an underlying fitting of
regressions models, allowing for quite some flexibility, such as including
factors as random effects in mixed models (multilevel partialization),
continuous variables as smooth terms in general additive models (non-linear
partialization) and/or fitting these models under a Bayesian framework. The
values returned by this function are the residuals of the regression models.
Note that a regular correlation between two "adjusted" variables is
equivalent to the partial correlation between them.
}
\examples{
\dontshow{if (all(insight::check_if_installed(c("bayestestR", "rstanarm", "gamm4"), quietly = TRUE))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
adjusted_all <- adjust(attitude)
head(adjusted_all)
adjusted_one <- adjust(attitude, effect = "complaints", select = "rating")
head(adjusted_one)
\donttest{
adjust(attitude, effect = "complaints", select = "rating", bayesian = TRUE)
adjust(attitude, effect = "complaints", select = "rating", additive = TRUE)
attitude$complaints_LMH <- cut(attitude$complaints, 3)
adjust(attitude, effect = "complaints_LMH", select = "rating", multilevel = TRUE)
}

# Generate data
data <- bayestestR::simulate_correlation(n = 100, r = 0.7)
data$V2 <- (5 * data$V2) + 20 # Add intercept

# Adjust
adjusted <- adjust(data, effect = "V1", select = "V2")
adjusted_icpt <- adjust(data, effect = "V1", select = "V2", keep_intercept = TRUE)

# Visualize
plot(
  data$V1, data$V2,
  pch = 19, col = "blue",
  ylim = c(min(adjusted$V2), max(data$V2)),
  main = "Original (blue), adjusted (green), and adjusted - intercept kept (red) data"
)
abline(lm(V2 ~ V1, data = data), col = "blue")
points(adjusted$V1, adjusted$V2, pch = 19, col = "green")
abline(lm(V2 ~ V1, data = adjusted), col = "green")
points(adjusted_icpt$V1, adjusted_icpt$V2, pch = 19, col = "red")
abline(lm(V2 ~ V1, data = adjusted_icpt), col = "red")
\dontshow{\}) # examplesIf}
}
