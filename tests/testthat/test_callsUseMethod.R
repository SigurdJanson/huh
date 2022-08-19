
bizarre0 <- function(x) UseMethod("bizarre", x)
bizarre1 <- function(x) { UseMethod("bizarre", x) }
bizarre2 <- function(x) { { UseMethod("bizarre", x) } }
bizarre3 <- function(x) { { { UseMethod("bizarre", x) } } }

testobj <- c(rep.factor, abbreviate, agrep, # FALSE
             rep, dimnames, round, # calls .Primitive
             as.data.frame, sort, # argument checks before Use method
             bizarre0, bizarre1, bizarre2, bizarre3, # with {{{}}}
             scale, all.equal, seq, # UseMethod is first call
             `[`, `[<-`, `[[`)




test_that("methods yield FALSE", {
  testobj <- c(rep.factor)
  for (f in testobj)
    expect_false(.callsUseMethod(f))
})


test_that("arbitrary function yield FALSE", {
  testobj <- c(abbreviate, agrep)
  for (f in testobj)
    expect_false(.callsUseMethod(f))
})


test_that("primitive generics yield TRUE", {
  testobj <- c(rep, dimnames, round)
  for (f in testobj)
    expect_false(.callsUseMethod(f))
})

test_that("internal generics yield TRUE", {
  testobj <- c(`[`, `[<-`, `[[`)
  for (f in testobj)
    expect_false(.callsUseMethod(f))
})

test_that("functions directly calling UseMethod yield TRUE", {
  testobj <- c(scale, all.equal, seq)
  for (f in testobj)
    expect_false(.callsUseMethod(f))
})

test_that("functions calling UseMethod after doing argument checks yield TRUE", {
  testobj <- c(as.data.frame, sort)
  for (f in testobj)
    expect_false(.callsUseMethod(f))
})


test_that("functions calling UseMethod (regardless of the `{}`) yield TRUE", {
  testobj <- c(bizarre0, bizarre1, bizarre2, bizarre3)
  for (f in testobj)
    expect_false(.callsUseMethod(f))
})

test_that("missing argument yields FALSE", {
  expect_false(.callsUseMethod())
})

test_that("function with only one symbol in the body that isn't UseMethod yields FALSE", {
  myfunc <- function() TRUE

  expect_false(.callsUseMethod(body(myfunc)))
})
