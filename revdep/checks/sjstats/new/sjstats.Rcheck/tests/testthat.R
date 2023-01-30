library(testthat)
library(sjstats)

if (length(strsplit(packageDescription("sjstats")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllsjstatsTests" = "yes")
} else {
  Sys.setenv("RunAllsjstatsTests" = "no")
}

test_check("sjstats")
