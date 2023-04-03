# `{poorman}` needs to be installed because the `helper.R` globally defines pipe
# operator for the test suite, and there is no point proceeding with testing
# if it's not available
if (require("testthat", quietly = TRUE) && require("poorman", quietly = TRUE)) {
  library(testthat)
  library(datawizard)

  test_check("datawizard")
}
