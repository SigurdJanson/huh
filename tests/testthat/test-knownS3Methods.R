

test_that("type is correct", {
  # Act
  result <- .knownMethodsS3("base")

  # Assert: base Package
  expect_type(result, "character")
  expect_named(result, NULL)

  # Act
  result <- .knownMethodsS3("stats")

  # Assert: non-base package
  expect_type(result, "character")
  expect_named(result, NULL)
})


test_that("'nsName' being NULL or 'base' gives the same result", {
  expect_identical(
    .knownMethodsS3(NULL),
    .knownMethodsS3("base")
  )
})


test_that("samples of known methods are present in the list of base functions", {
  package <- "base"
  look4 <- c("[[.data.frame", "t.data.frame", "mean.default")

  # Act
  result <- .knownMethodsS3(package)

  # Assert
  for (m in look4)
    expect_true(m %in% result, label = m)
})


test_that("samples of known methods are present in the list of package functions", {
  package <- "stats"
  look4 <- c("anova.lm", "ar.burg.default", "print.formula", "terms.formula")

  # Act
  result <- .knownMethodsS3(package)

  # Assert
  for (m in look4)
    expect_true(m %in% result, label = m)
})


test_that("samples of 'false friends' are NOT present", {
  package <- "stats"
  look4 <- c("ansari.test")

  # Act
  result <- .knownMethodsS3(package)

  # Assert
  for (m in look4)
    expect_false(m %in% result, label = m)
})



# EXCEPTIONS =============

test_that("No valid argument", {
  expect_error(.knownMethodsS3(integer()), "Namespace name is not a character string")
  expect_error(.knownMethodsS3(character()), "Namespace name is empty")
})
