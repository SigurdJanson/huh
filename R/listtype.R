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

