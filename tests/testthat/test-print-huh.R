
# print.huh ===========

cli::test_that_cli("Arguments lang='S' and 'R' deliver different results", {
  SOutput <- c("mode", "storage mode")
  # Act
  #resultR <- capture_output_lines(print(huh(1:3)))
  #resultS <- capture_output_lines(print(huh(1:3), lang="S"))
  resultR <- cli::cli_fmt(print(huh(1:3)))
  resultS <- cli::cli_fmt(print(huh(1:3), lang="S"))

  # Assert
  # * length
  expect_identical(length(resultS), length(resultR)+length(SOutput))
  # * mode, storage mode, ... are part of S result
  for (s in SOutput)
    expect_true( any(startsWith(resultS, s)) )
  # * mode, storage mode, ... are NOT part of R result
  for (r in SOutput)
    expect_false( any(startsWith(resultR, r)) )
})


test_that("result of 'print.huh' is identical to input", {
  Mephisto <- new_huh("Faustus", "Scholar", "DesperateOne",
                      "Male", "Theater", 17, "OldAge", "Late Works")

  # Act
  cli::cli_fmt(
    result <- print(Mephisto)
  )

  # Assert
  expect_identical(result, Mephisto)
})



# print.huh.how ===========

test_that("print.huh.how declares 'how$ops == NULL' as not subsettable", {
  #
  styxHow <- new_huh.how("styx", .ops = NULL)
  # Act
  result <- capture_output_lines(print(styxHow))
  # Assert
  expect_identical(length(result), 1L)
  expect_match(result[1], "styx is not subsettable")
})


test_that("print.huh.how gives 'operations' as output", {
  #
  theGreatHow <- new_huh.how("the Great", .ops = list(Gatsby = c("[c(...)]", "[[...]]")))
  # Act
  result <- capture_output_lines(print(theGreatHow))
  # Assert
  expect_identical(length(result), 4L)
  expect_match(result[1], "Subsetting 'the Great'")
  expect_match(result[2], "")
  expect_match(result[3], "Gatsby")
  #expect_match(result[4], glob2rx("the Great[c(...)], the Great[[...]]"))
  expect_equal(result[4], "the Great[c(...)], the Great[[...]]")
})



test_that("print.huh.how gives 'comments' as output", {
  #
  Mephisto <- new_huh.how("Faustus", .ops = list(Gretchen = "/abc/"), .comments = "Drudenfuß")
  # Act
  result <- capture_output_lines(print(Mephisto))
  # Assert
  expect_identical(length(result), 5L)
  expect_match(result[1], "Subsetting 'Faustus'")
  expect_match(result[2], "")
  # ...
  #expect_match(result[5], "")
  expect_match(result[5], "Drudenfuß")
})



test_that("result of 'print.huh.how' is identical to input", {
  Mephisto <- new_huh.how("Faustus", .ops = list(Gretchen = "/abc/"), .comments = "Drudenfuß")

  # Act
  capture_output_lines(
    result <- print(Mephisto)
  )

  # Assert
  expect_identical(result, Mephisto)
})
