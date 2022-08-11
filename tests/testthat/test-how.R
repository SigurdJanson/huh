
.ClassName <- "huh.how"
.NamesOfHow <- c("name", "ops", "comments")

# ATOMIC ==================

test_that("vector, integer", {
  obj <- c(3L, 5L, 7L)
  expect(isTRUE(is.integer(obj)), "Test failed assumption")

  # act
  result <- how(obj)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_identical(result$ops, list(vector = c("[c(...)]", "[[...]]")))
  expect_null(result$comments)
})

test_that("named vector, integer", {
  obj <- c(a=3L, b=5L, c=7L)
  expect(isTRUE(is.integer(obj)), "Test failed assumption")

  # act
  result <- how(obj)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_identical(result$ops, list(vector = c("[c(...)]", "[[...]]")))
  expect_match(result$comments, ".*attr")
})




# LIST==================

test_that("list", {
  obj <- list(a=3L, b=3:5L, c=c("a", "b"))
  expect(isTRUE(is.list(obj)), "Test failed assumption")

  # act
  result <- how(obj)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_identical(result$ops, list(list = c("[c(...)]"), reduced = c("[[...]]", "$...")))
  expect_null(result$comments)
})


