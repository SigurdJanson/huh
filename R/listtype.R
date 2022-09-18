# Helper functions for deeper analysis of 'lists'


#' is.nested
#'
#' @param x An object
#'
#' @return `TRUE` is the object `x` is a list that contains elements of type `list`.
#' Otherwise `FALSE`.
#' @export
#'
#' @examples
#' is.nested(list(1, 2, 3, 4, 5)) # FALSE
#' is.nested(list(1, 2, 2:3, 4, 5)) # FALSE
#' is.nested(list(1:2, 2:3, 3:4, letters, LETTERS)) # FALSE
#' is.nested(list(1, 2, list(3, "a"), 4, 5)) # TRUE
#' is.nested(list(list(1), list(2), list(3), list(4), list(5))) # TRUE
#' is.nested(NULL) # FALSE
is.nested <- function(x) {
  any(sapply(x, is.list))
}




#' depth
#'
#' Determines the (maximum) depth of nesting of vectors.
#'
#' @param x An object
#' @param current Starting level for the nesting. Default is 0 and there
#' is no reason to change that. If you set `current` to a value `x` the result will
#' be equal to `depth(...) + x`.
#'
#' @return The maximum nesting depth found anywhere in the list.
#' The result will (obviously) always be 1 for atomic vectors. Returns `NA` for
#' objects that are not vectors.
#' @export
#'
#' @examples
#' depth(list(1, 2, list(3, "a"), 4, 5))
#' depth(expression(u, 2, u + 0:9))
depth <- function(x, current = 0L) {
  if (is.recursive(x)) {
    current <- current + 1L
    result <- sapply(x, \(x) { if (is.recursive(x)) depth(x, current) else current })
    return(max(result))
  }
  if (is.vector(x)) { # vector but not recursive can NOT have a depth > 1
    return(current + 1L)
  }
  return(NA)
}



#' is.mixed
#'
#' @param x a list
#' @param f a function used on each element. `typeof()` by default, but any other function
#' can be used.
#' @param recursive By default `is.mixed()` goes through the entire nestings of the list
#' (default: `TRUE`). If `FALSE` only the first level will be analysed.
#' @details `is.mixed` returning `TRUE` implies that `unlist()` will cause some an implicit
#' type coercion on some list elements.
#' @return `TRUE` if all objects in the list `x` yield the same result `f(x)`. `NA` if `x` is
#' not a list.
#' @export
#'
#' @examples
#' is.mixed(list(1, 2, 2L:3L, 4, 5))
#' is.mixed(list(1L, 2L, 2L:3L, 4L, 5L))
is.mixed <- function(x, f=typeof, recursive=TRUE) { # has mixed types
  if (!is.list(x)) return(NA)
  # storage.mode already maps "closure", "builtin" and "special" to "function
  if (storage.mode(f) != "function")
    stop("'f' must be a function")

  if (recursive)
    result <- rapply(x, f, how = "unlist")
  else
    result <- sapply(x, f)

  result <- result |> unique() |> length()
  return(result > 1L)
}
