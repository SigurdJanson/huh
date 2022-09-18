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


