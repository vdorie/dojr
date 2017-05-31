if (require(testthat, quietly = TRUE)) {
  require(rsupp)
  test_check("rsupp")
} else {
  cat("package 'testthat' not available; cannot run unit tests\n")
}
