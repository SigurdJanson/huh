

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
      type = "integer",
      classS3 = "integer",
      mode = "numeric",
      dimensions = 1L
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
      type = "integer",
      classS3 = c("matrix", "array"),
      mode = "numeric",
      dimensions = 2L
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
      type = "closure",
      classS3 = "function",
      mode = "function",
      dimensions = 0L
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
      type = "symbol",
      classS3 = "name",
      mode = "name",
      dimensions = 0L
    )
  )
})



