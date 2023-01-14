Sys.setenv("_R_CHECK_RD_VALIDATE_RD2HTML_" = "true")
Sys.setenv("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false")
Sys.setenv("_R_CHECK_CRAN_INCOMING_" = "false")

rcmdcheck::rcmdcheck(
  args = c("--as-cran", "--no-codoc", "--no-examples", "--no-tests", "--no-vignettes", "--no-build-vignettes", "--ignore-vignettes", "--no-install"),
  build_args = c("--no-build-vignettes"),
  error_on = "note"
)
