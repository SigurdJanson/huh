
# `is.nested()` ==============

test_that("nested lists are detected", {
  # FALSE
  expect_false( is.nested(list(1, 2, 3, 4, 5)) )
  expect_false( is.nested(list(1, 2, 2:3, 4, 5)) )
  expect_false( is.nested(list(1:2, 2:3, 3:4, letters, LETTERS)) ) #mixed

  # TRUE
  expect_true( is.nested(list(1, 2, list(3, "a"), 4, 5)) )
  expect_true( is.nested(list(list(1), list(2), list(3), list(4), list(5))) )
})


test_that("`is.nested()` works with non-vector objects", {
  expect_false( is.nested(mean) )
})


test_that("`is.nested` can handle 'falsy' objects", {
  expect_false( is.nested(NULL) )
  expect_false( is.nested(Inf) )
  expect_false( is.nested(NA) )
})
