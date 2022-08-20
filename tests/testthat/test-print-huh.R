
test_that("Arguments lang='S' and 'R' deliver different results", {
  SOutput <- c("mode", "storage mode")
  # Act
  resultR <- capture_output_lines(print(huh(1:3)))
  resultS <- capture_output_lines(print(huh(1:3), lang="S"))

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
