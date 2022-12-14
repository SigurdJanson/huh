# TODO: environment argument isn't really tested


test_that("primitive generics detected", {
  expect_true(.isS3Generic("["))
  expect_true(.isS3Generic("[["))

  expect_true(.isS3Generic("sum"))
  expect_true(.isS3Generic("mean"))

  expect_true(.isS3Generic("c"))
})



test_that("non-generic primitive is ruled out", {
  expect_false(.isS3Generic("UseMethod"))
})



test_that("non-functions are not generics", {
  a <- TRUE
  b <- NULL
  c <- data.frame()

  expect_false(.isS3Generic("a"))
  expect_false(.isS3Generic("b"))

  # Cannot distinguish `c()` from `c`.
  expect_true(.isS3Generic("c"))
})



test_that("user defined generics & methods detected", {
  my_method <- function(x) UseMethod("mymethod")
  my_method.character <- function(x) x

  expect_true(.isS3Generic("my_method"))
  #expect_true(is_s3_method("my_method.character"))
})



test_that("methods for group generics detected", {
  Ops.myclass <- function(x) x

  expect_false(.isS3Generic("Ops.myclass"))
  #expect_true(is_s3_method("Ops.myclass"))
})



test_that("user defined generics detected even if use non-standard", {
  my_method <- function(x) {
    x <- 1
    if (x > 2) UseMethod("mymethod")
  }

  expect_true(.isS3Generic("my_method"))
})



# SYMBOL AS ARGUMENT ==============
test_that("existing symbol is translated and yields TRUE", {
  expect_true(.isS3Generic(as.symbol("mean")))
  expect_false(.isS3Generic(as.symbol("mean.difftime")))
})



# EXCEPTIONS ============
test_that("missing argument throws error", {
  expect_error(.isS3Generic())
})

test_that("empty string returns FALSE", {
  expect_false(.isS3Generic(""))
})

test_that("closure throw an error", {
  expect_error(.isS3Generic(mean))
  expect_error(.isS3Generic(mean.POSIXct))
})



# Test taken from roxygen2 - but why shouldn't we be able to overwrite/conceal a primitive
# test_that("user defined functions override primitives", {
#   c <- function(x) x + 1
#   c.test <- function(x) x + 3
#
#   expect_false(.isS3Generic("c"))
#   #expect_false(is_s3_method("c"))
# })

