

test_that("false methods yield FALSE", {
  # non S3 methods taken from the tools package
  # https://github.com/wch/r-source/tree/79298c499218846d14500255efd622b5021c10ec/src/library/tools
  testobj <- c("pmax.int", "seq.int", # base
               "boxplot.stats",  #
               "t.test" # stats::
               )

  for (f in testobj)
    expect_false(.isS3Method(f), label = f)
})


test_that("Base S3 generics yield FALSE", {
  testobj <- c("print", "all.equal", "format", "as.double")
  for (f in testobj)
    expect_false(.isS3Method(f))
})


test_that("true base S3 methods yield TRUE", {
  testobj <- c("print.default", "all.equal.function",
               "all.equal.list", "format.difftime",
               "mean.default", "c.Date", "[.data.frame")
  for (f in testobj)
    expect_true(.isS3Method(f))
})


test_that("methods for group generics detected", {
  Ops.myclass <- function(x) x

  #expect_false(.isS3Generic("Ops.myclass"))
  expect_true(.isS3Method("Ops.myclass"))
})


test_that("user defined methods detected", {
  my_method <- function(x) UseMethod("mymethod")
  my_method.character <- function(x) x

  #expect_true(is_s3_generic("my_method"))
  expect_true(.isS3Method("my_method.character"))
})



