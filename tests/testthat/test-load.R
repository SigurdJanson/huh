


test_that("onAttach shows message only if the argument is FALSE", {
  expect_message(huh:::.OnAttachMessage(FALSE), r"{^Welcome to 'huh!\?!\?'.*}")
  expect_message(huh:::.OnAttachMessage(TRUE), NA)
})
