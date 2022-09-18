
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





# `is.mixed()` ==============



test_that("Non-Recursive: Mixings on 1st level are identified correctly, other levels ignore", {
  # all numeric
  expect_false(is.mixed(list(1, 2, 3, 4, 5)))
  # all integer
  expect_false(is.mixed(list(1L, 2L, 2L:3L, 4L, 5L)))

  # numeric and integer
  expect_true(is.mixed(list(1, 2, 3, 4, 5L)))
  expect_true(is.mixed(list(1, 2, 2:3, 4, 5)))
  # integer and character
  expect_true(is.mixed(list(1:2, 2:3, 3:4, letters, LETTERS)))
  # vectors and lists
  expect_true(is.mixed(list(1, 2, list(3, "a"), 4, 5)))
})



test_that("Non-Recursive: Mixings on 1st level are identified correctly, other levels ignore", {
    # All lists
  expect_false(is.mixed(list(list(1), list(2), list(3), list(4), list(5))))
  expect_true(is.mixed(list(list(1), list(2), list("c"), list(4), list(5))))
  expect_true(is.mixed(list(list(1), list(2), list(3), list(list(4, list(4.1, 4.2, list(4.11))), list(letters)), list(5))))
})



test_that("Objects that aren't lists return `NA`", {
  expect_identical(is.mixed(c(1, 2, 3, 4, 5)), NA)
  expect_identical(is.mixed(NULL), NA)
})



test_that("other functions than the default `typeof()` work", {
  # a "builtin" function, see typeof()
  expect_false(is.mixed(list(1, 2, 3, 4, 5), length))
  expect_true(is.mixed(list(1, 2, 2:3, 4, 5), length))
  expect_true(is.mixed(list(1:2, 2:3, 3:4, letters, LETTERS), length))

  expect_true(is.mixed(list(matrix(1:4), 1.1, list(2)), class))

  # a closure
  expect_false(is.mixed(list(5, 4:6, 3:7, 2:8, 1:9), mean))
})



test_that("non function as argument `f` throws an error", {
  expect_error(is.mixed(list(1, 2, 3, 4, 5), "abc"), "'f' must be a function")
})



test_that("recursive levels are considered (and lists are not considered)", {
  expect_false(is.mixed(list(1, 2, 3, 4, 5), recursive = TRUE))

  expect_true(is.mixed(list(1, 2, 2:3, 4, 5), recursive = TRUE))

  expect_true(is.mixed(list(1:2, 2:3, 3:4, letters, LETTERS), recursive = TRUE))
  expect_true(is.mixed(list(1, 2, list(3, "a"), 4, 5), recursive = TRUE))

  expect_false(is.mixed(list(list(1), list(2), list(3), list(4), list(5)), recursive = TRUE))
  expect_false(is.mixed(list(list(1), list(2, list(3), list(4))), recursive = TRUE))

  expect_true(is.mixed(list(list(1), list(2), list("c"), list(4), list(5)), recursive = TRUE))
  expect_true(
    is.mixed(list(list(1), list(2), list(3), list(list(4, list(4.1, 4.2, list(4.11))), list(letters)), list(5)),
             recursive = TRUE))

})








