

makeHuhList <- function(...) {
  x <- list(...)
  class(x) <- "huh"
  x
}



test_that("vector, integer", {
  obj <- c(3L, 5L, 7L)
  expect(isTRUE(is.integer(obj)), "Test failed assumption")

  # act
  result <- huh(obj)

  # assert
  expect_identical(
    result,
    makeHuhList(
      name = "obj",
      type = "integer",
      class = "integer",
      mode = "numeric",
      dimensions = 1L,
      paradigm = "implicit"
      )
  )
})



test_that("factor, character", {
  obj <- factor(sample(LETTERS[1:3], 12, TRUE))
  expect(isTRUE(is.factor(obj)), "Test failed assumption")

  # act
  result <- huh(obj)

  # assert
  expect_identical(
    result,
    makeHuhList(
      name = "obj",
      type = "integer",
      class = "factor",
      mode = "numeric",
      dimensions = 0L,
      paradigm = "S3 class"
    )
  )
})


test_that("matrix", {
  obj <- matrix(1:4, 2)
  expect(isTRUE(is.matrix(obj)), "Test failed assumption")

  # act
  result <- huh(obj)

  # assert
  expect_identical(
    result,
    makeHuhList(
      name = "obj",
      type = "integer",
      class = c("matrix", "array"),
      mode = "numeric",
      dimensions = 2L,
      paradigm = "implicit"
    )
  )
})



test_that("data frame", {
  obj <- mtcars
  expect(isTRUE(is.data.frame(obj)), "Test failed assumption")

  # act
  result <- huh(obj)

  # assert
  expect_identical(
    result,
    makeHuhList(
      name = "obj",
      type = "list",
      class = "data.frame",
      mode = "list",
      dimensions = 2L,
      paradigm = "S3 class"
    )
  )
})



test_that("function", {
  obj <- mean
  expect(isTRUE(is.function(obj)), "Test failed assumption")

  # act
  result <- huh(obj)

  # assert
  expect_identical(
    result,
    makeHuhList(
      name = "obj",
      type = "closure",
      class = "function",
      mode = "function",
      dimensions = 0L,
      paradigm = "implicit"
    )
  )
})



test_that("symbol", {
  obj <- quote(mean)
  expect(isTRUE(is.symbol(obj)), "Test failed assumption")

  # act
  result <- huh(obj)

  # assert
  expect_identical(
    result,
    makeHuhList(
      name = "obj",
      type = "symbol",
      class = "name",
      mode = "name",
      dimensions = 0L,
      paradigm = "implicit"
    )
  )
})



test_that("S4", {
  student <- setClass("student", slots=c(name="character", age="numeric", GPA="numeric"))
  obj <- student(name="Harry Potter", age=97.3, GPA=-3)
  expect(isTRUE(isS4(obj)), "Test failed assumption")

  # act
  result <- huh(obj)

  # assert
  expect_identical(
    result,
    makeHuhList(
      name = "obj",
      type = "S4",
      class = "student",
      mode = "S4",
      dimensions = 0L,
      paradigm = "S4 class"
    )
  )
})



test_that("Reference class", {
  account <- setRefClass("Account")
  obj <- account$new()
  expect(isTRUE(inherits(obj, "refClass")), "Test failed assumption")

  # act
  result <- huh(obj)

  # assert
  expect_identical(
    result,
    makeHuhList(
      name = "obj",
      type = "S4",
      class = "Account",
      mode = "S4",
      dimensions = 0L,
      paradigm = "Reference class"
    )
  )
})


test_that("NULL", {
  # act
  result <- huh(NULL)

  # assert
  expect_identical(result, NULL)
})
