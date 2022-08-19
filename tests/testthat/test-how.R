
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


# FACTOR ==================
test_that("factor", {
  obj <- factor(sample(1:3, 24, TRUE))
  expect(isTRUE(is.factor(obj)), "Test failed assumption")

  # act
  result <- how(obj)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_identical(result$ops, list(vector = c("[c(...)]", "[[...]]")))
  expect_match(result$comments, "Access")
})



# EXPRESSION ==================
test_that("expression", {
  obj <- expression(1 + 0:9)
  expect(isTRUE(is.expression(obj)), "Test failed assumption")

  # act
  result <- how(obj)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_identical(result$ops, list(expression = c("[c(...)]"),
                                    `mixed types` = c("[[...]]")))
})


# ENVIRONMENT ==================
test_that("environment", {
  env <- new.env()
  expect(isTRUE(is.environment(env)), "Test failed assumption")

  # act
  result <- how(env)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "env")
  expect_identical(result$ops, list(`mixed types` = c("[[...]]", "$...")))
  expect_match(result$comments, "Only character indices")
})



# QUOSURE ==================
test_that("quosure", {
  q <- rlang::quo(1:3)
  expect(isTRUE(rlang::is_quosure(q)), "Test failed assumption")

  # act
  result <- how(q)

  # assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "q")
  expect_identical(result$ops, list(NULL))
  expect_match(result$comments, "deprecated")
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


# UNKNOWN CLASS ==========
# do not use the same class name in all tests;
# registerS3method() is going to store the association
# across tests.

test_that("`how.default` finds `[` method for unknown classes", {
  `[.unknown1` <- function(x) TRUE
  registerS3method("[", "unknown1", "[.unknown1")

  obj <- structure(list(1, 2, "3"), class="unknown1")

  # Act
  result <- how(obj)

  # Assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_identical(result$ops, list(c("[c(...)]")))
})


test_that("`how.default` finds `[[` method for unknown classes", {
  `[[.unknown2` <- function(x) TRUE
  registerS3method("[[", "unknown2", "[[.unknown2")

  obj <- structure(list(1, 2, "3"), class="unknown2")

  # Act
  result <- how(obj)

  # Assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_identical(result$ops, list(c("[[...]]")))
})

test_that("`how.default` finds `$` method for unknown classes", {
  `$.unknown3` <- function(x) TRUE
  registerS3method("$", "unknown3", "$.unknown3")

  obj <- structure(list(1, 2, 3), class="unknown3")

  # Act
  result <- how(obj)

  # Assert
  expect_s3_class(result, .ClassName)
  expect_named(result, .NamesOfHow)
  expect_identical(result$name, "obj")
  expect_identical(result$ops, list(c("$...")))
})
