
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

# ARRAY  ==============


test_that("array", {
  obj <- array(1:24, c(4, 3, 2))
  expect(isTRUE(is.array(obj)), "Test failed assumption")

  # act
  result <- how(obj)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_named(result$ops, c("vector", "matrix", "array"))
  # MAYBE TEST CONTENT HERE
  expect_length(result$comments, 5L)
})



# MATRIX ==============

test_that("matrix", {
  obj <- matrix(1:12, c(4, 3))
  expect(isTRUE(is.matrix(obj)), "Test failed assumption")

  # act
  result <- how(obj)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_named(result$ops, c("vector", "matrix"))
  # MAYBE TEST CONTENT HERE
  expect_length(result$comments, 4L)
})


# DATA FRAME ==============

test_that("data.frame", {
  obj <- data.frame(a=1:12, b=letters[1:12])
  expect(isTRUE(is.data.frame(obj)), "Test failed assumption")

  # act
  result <- how(obj)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_named(result$ops, c("scalar", "column(s) as list", "column(s) as atomic", "data frame"))
  # MAYBE TEST CONTENT HERE
  expect_length(result$comments, 1L)
})



# COMPLEX ================

test_that("vector, complex", {
  obj <- complex(real = c(3, 5, 7), imaginary = c(0,2,7))
  expect(isTRUE(is.complex(obj)), "Test failed assumption")

  # act
  result <- how(obj)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_identical(result$ops, list(vector = c("[c(...)]", "[[...]]")))
  expect_match(result$comments, ".*'Re.*'Im")
})


# NOT SUBSETTABLE =========
test_that("Not subsettable", {
  expect_match(
    how(mean)$comments, "Object not subsettable",
  )
  expect_match(
    how(call("u"))$comments, "Object not subsettable",
  )
})
