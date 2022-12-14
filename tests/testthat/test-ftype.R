

test_that("ftype() returns the same types as `sloop::ftype()` does", {
  # These are different: we do not identify internals
  # * non-primitives are simply just functions
  expect_identical(ftype(writeLines), list(type = "closure"))
  # * non-primitive generics are regular S3 generics
  expect_identical(
    ftype(unlist),
    list(type = "closure", paradigm = "S3", virtual = "generic"))
})


test_that("functions return as expected", {
  f <- function(x) x
  expect_equal(ftype(f), list(type = "closure"))
  expect_equal(
    ftype(sum),
    list(type = "primitive", paradigm = "S3", virtual = "generic"))
  expect_identical(
    ftype(`%in%`), list(type="closure"))
})


test_that("various flavours of S3 return as expected", {
  expect_equal(
    ftype(t),
    list(type = "closure", paradigm = "S3", virtual = "generic"))
  expect_equal(
    ftype(t.data.frame),
    list(type = "closure", paradigm = "S3", virtual = "method"))
  expect_equal(
    ftype(t.test),
    list(type = "closure", paradigm = "S3", virtual = "generic"))
})



test_that("function can be both S3 generic and method", {
  f <- function(x) UseMethod("f")
  f.foo <- function(x) UseMethod("f.foo")

  expect_equal(
    ftype(f.foo),
    list(type = "closure", paradigm = "S3", virtual = c("generic", "method")))
})


test_that("S4 methods and generics return as expected", {
  e <- attach(NULL, name = "test")
  on.exit(detach("test"))

  A <- setClass("A", contains = list(), where = e)

  setGeneric("f", function(x) 1, where = e)
  f <- getGeneric("f", where = e)
  expect_equal(
    ftype(f),
    list(type = "closure", paradigm = "S4", virtual = "generic"))

  setMethod("f", signature(x = "A"), function(x) 1, where = e)
  m <- getMethod("f", signature(x = "A"), where = e)
  expect_equal(
    ftype(m),
    list(type = "closure", paradigm = "S4", virtual = "method"))
})


test_that("RC methods return as expected", {
  B <- setRefClass("B", methods = list(f = function(x) x))
  b <- B$new()

  expect_equal(ftype(b$f), list(type = "closure", paradigm = "RC", virtual = "method"))
})




test_that("function names as string work, too", {
  expect_identical(
    ftype("sd"),
    list(type = "closure"))
  expect_identical(
    ftype("unlist"),
    list(type = "closure", paradigm = "S3", virtual = "generic"))
  expect_equal(
    ftype("t.data.frame"),
    list(type = "closure", paradigm = "S3", virtual = "method"))
  expect_equal(
    ftype("t.test"),
    list(type = "closure", paradigm = "S3", virtual = "generic"))
})


test_that("symbols/names work, too", {
  expect_identical(
    ftype(as.name("sd")),
    list(type = "closure"))
  expect_identical(
    ftype(as.name("unlist")),
    list(type = "closure", paradigm = "S3", virtual = "generic"))
  expect_equal(
    ftype(as.name("t.data.frame")),
    list(type = "closure", paradigm = "S3", virtual = "method"))
  expect_equal(
    ftype(as.name("t.test")),
    list(type = "closure", paradigm = "S3", virtual = "generic"))
})



# ERROR =====================

test_that("non-functions throw an error", {
  nonfunc <- "sd_this_will_never_be_a_string_that_will_be_found_to_be_a_function"
  expect_error(
    ftype(nonfunc),
    "'f'.*not.*function")
  expect_error(
    ftype(as.name(nonfunc)),
    "'f'.*not.*function")
})
