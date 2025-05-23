% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_merge.R
\name{data_merge}
\alias{data_merge}
\alias{data_join}
\alias{data_merge.data.frame}
\alias{data_merge.list}
\title{Merge (join) two data frames, or a list of data frames}
\usage{
data_merge(x, ...)

data_join(x, ...)

\method{data_merge}{data.frame}(x, y, join = "left", by = NULL, id = NULL, verbose = TRUE, ...)

\method{data_merge}{list}(x, join = "left", by = NULL, id = NULL, verbose = TRUE, ...)
}
\arguments{
\item{x, y}{A data frame to merge. \code{x} may also be a list of data frames
that will be merged. Note that the list-method has no \code{y} argument.}

\item{...}{Not used.}

\item{join}{Character vector, indicating the method of joining the data frames.
Can be \code{"full"}, \code{"left"} (default), \code{"right"}, \code{"inner"}, \code{"anti"}, \code{"semi"}
or \code{"bind"}. See details below.}

\item{by}{Specifications of the columns used for merging.}

\item{id}{Optional name for ID column that will be created to indicate the
source data frames for appended rows. Only applies if \code{join = "bind"}.}

\item{verbose}{Toggle warnings.}
}
\value{
A merged data frame.
}
\description{
Merge (join) two data frames, or a list of data frames. However, unlike
base R's \code{merge()}, \code{data_merge()} offers a few more methods to join data
frames, and it does not drop data frame nor column attributes.
}
\section{Merging data frames}{


Merging data frames is performed by adding rows (cases), columns
(variables) or both from the source data frame (\code{y}) to the target
data frame (\code{x}). This usually requires one or more variables which
are included in both data frames and that are used for merging, typically
indicated with the \code{by} argument. When \code{by} contains a variable present
in both data frames, cases are matched and filtered by identical values
of \code{by} in \code{x} and \code{y}.
}

\section{Left- and right-joins}{


Left- and right joins usually don't add new rows (cases), but only new
columns (variables) for existing cases in \code{x}. For \code{join = "left"} or
\code{join = "right"} to work, \code{by} \emph{must} indicate one or more columns that
are included in both data frames. For \code{join = "left"}, if \code{by} is an
identifier variable, which is included in both \code{x} and \code{y}, all variables
from \code{y} are copied to \code{x}, but only those cases from \code{y} that have
matching values in their identifier variable in \code{x} (i.e. all cases
in \code{x} that are also found in \code{y} get the related values from the new
columns in \code{y}). If there is no match between identifiers in \code{x} and \code{y},
the copied variable from \code{y} will get a \code{NA} value for this particular
case. Other variables that occur both in \code{x} and \code{y}, but are not used
as identifiers (with \code{by}), will be renamed to avoid multiple identical
variable names. Cases in \code{y} where values from the identifier have no
match in \code{x}'s identifier are removed. \code{join = "right"} works in
a similar way as \code{join = "left"}, just that only cases from \code{x} that
have matching values in their identifier variable in \code{y} are chosen.

In base R, these are equivalent to \code{merge(x, y, all.x = TRUE)} and
\code{merge(x, y, all.y = TRUE)}.
}

\section{Full joins}{


Full joins copy all cases from \code{y} to \code{x}. For matching cases in both
data frames, values for new variables are copied from \code{y} to \code{x}. For
cases in \code{y} not present in \code{x}, these will be added as new rows to \code{x}.
Thus, full joins not only add new columns (variables), but also might
add new rows (cases).

In base R, this is equivalent to \code{merge(x, y, all = TRUE)}.
}

\section{Inner joins}{


Inner joins merge two data frames, however, only those rows (cases) are
kept that are present in both data frames. Thus, inner joins usually
add new columns (variables), but also remove rows (cases) that only
occur in one data frame.

In base R, this is equivalent to \code{merge(x, y)}.
}

\section{Binds}{


\code{join = "bind"} row-binds the complete second data frame \code{y} to \code{x}.
Unlike simple \code{rbind()}, which requires the same columns for both data
frames, \code{join = "bind"} will bind shared columns from \code{y} to \code{x}, and
add new columns from \code{y} to \code{x}.
}

\examples{

x <- data.frame(a = 1:3, b = c("a", "b", "c"), c = 5:7, id = 1:3)
y <- data.frame(c = 6:8, d = c("f", "g", "h"), e = 100:102, id = 2:4)

x
y

# "by" will default to all shared columns, i.e. "c" and "id". new columns
# "d" and "e" will be copied from "y" to "x", but there are only two cases
# in "x" that have the same values for "c" and "id" in "y". only those cases
# have values in the copied columns, the other case gets "NA".
data_merge(x, y, join = "left")

# we change the id-value here
x <- data.frame(a = 1:3, b = c("a", "b", "c"), c = 5:7, id = 1:3)
y <- data.frame(c = 6:8, d = c("f", "g", "h"), e = 100:102, id = 3:5)

x
y

# no cases in "y" have the same matching "c" and "id" as in "x", thus
# copied variables from "y" to "x" copy no values, all get NA.
data_merge(x, y, join = "left")

# one case in "y" has a match in "id" with "x", thus values for this
# case from the remaining variables in "y" are copied to "x", all other
# values (cases) in those remaining variables get NA
data_merge(x, y, join = "left", by = "id")

data(mtcars)
x <- mtcars[1:5, 1:3]
y <- mtcars[28:32, 4:6]

# add ID common column
x$id <- 1:5
y$id <- 3:7

# left-join, add new variables and copy values from y to x,
# where "id" values match
data_merge(x, y)

# right-join, add new variables and copy values from x to y,
# where "id" values match
data_merge(x, y, join = "right")

# full-join
data_merge(x, y, join = "full")


data(mtcars)
x <- mtcars[1:5, 1:3]
y <- mtcars[28:32, c(1, 4:5)]

# add ID common column
x$id <- 1:5
y$id <- 3:7

# left-join, no matching rows (because columns "id" and "disp" are used)
# new variables get all NA values
data_merge(x, y)

# one common value in "mpg", so one row from y is copied to x
data_merge(x, y, by = "mpg")

# only keep rows with matching values in by-column
data_merge(x, y, join = "semi", by = "mpg")

# only keep rows with non-matching values in by-column
data_merge(x, y, join = "anti", by = "mpg")

# merge list of data frames. can be of different rows
x <- mtcars[1:5, 1:3]
y <- mtcars[28:31, 3:5]
z <- mtcars[11:18, c(1, 3:4, 6:8)]
x$id <- 1:5
y$id <- 4:7
z$id <- 3:10
data_merge(list(x, y, z), join = "bind", by = "id", id = "source")
}
\seealso{
\itemize{
\item Add a prefix or suffix to column names: \code{\link[=data_addprefix]{data_addprefix()}}, \code{\link[=data_addsuffix]{data_addsuffix()}}
\item Functions to reorder or remove columns: \code{\link[=data_reorder]{data_reorder()}}, \code{\link[=data_relocate]{data_relocate()}},
\code{\link[=data_remove]{data_remove()}}
\item Functions to reshape, pivot or rotate data frames: \code{\link[=data_to_long]{data_to_long()}},
\code{\link[=data_to_wide]{data_to_wide()}}, \code{\link[=data_rotate]{data_rotate()}}
\item Functions to recode data: \code{\link[=rescale]{rescale()}}, \code{\link[=reverse]{reverse()}}, \code{\link[=categorize]{categorize()}},
\code{\link[=recode_values]{recode_values()}}, \code{\link[=slide]{slide()}}
\item Functions to standardize, normalize, rank-transform: \code{\link[=center]{center()}}, \code{\link[=standardize]{standardize()}},
\code{\link[=normalize]{normalize()}}, \code{\link[=ranktransform]{ranktransform()}}, \code{\link[=winsorize]{winsorize()}}
\item Split and merge data frames: \code{\link[=data_partition]{data_partition()}}, \code{\link[=data_merge]{data_merge()}}
\item Functions to find or select columns: \code{\link[=data_select]{data_select()}}, \code{\link[=extract_column_names]{extract_column_names()}}
\item Functions to filter rows: \code{\link[=data_match]{data_match()}}, \code{\link[=data_filter]{data_filter()}}
}
}
