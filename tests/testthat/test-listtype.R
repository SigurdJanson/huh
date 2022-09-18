
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




# `depth()` ==============

test_that("`depth()` gives 1 for atomic vectors", {
  expect_identical( depth(1:5), 1L)
  expect_identical( depth(letters), 1L)
})

test_that("`depth()` returns correct results for different kinds of lists", {
  expect_identical( depth(list(1, 2, 3, 4, 5)), 1L)
  expect_identical( depth(list(1, 2, 2:3, 4, 5)), 1L)
  expect_identical( depth(list(1:2, 2:3, 3:4, letters, LETTERS)), 1L)

  expect_identical( depth(list(1, 2, list(3, "a"), 4, 5)), 2L)
  expect_identical( depth(list(list(1), list(2), list(3), list(4), list(5))), 2L)
  expect_identical(
    depth(list(list(1), list(2), list(3), list(list(4, list(4.1, 4.2, list(4.11))), list(letters)), list(5))),
    5L)
})

test_that("`depth()` works for expressions", {
  expect_identical( depth(expression(u)), 1L )
  expect_identical( depth(expression(u, 2)), 1L )
  expect_identical( depth(expression(u, 2, u + 0)), 2L )
  expect_identical( depth(expression(u, 2, u + 0:9)), 3L )
  expect_identical( depth(expression(u, 2, expression(u + 0:9))), 4L )
})


test_that("`depth()` returns NA for objects that aren't vectors", {
  expect_identical( depth(NULL), NA )
})
