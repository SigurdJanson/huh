

#' Determine the OOP paradigm of an object.
#'
#' @param x An object
#' @return Is x an implicit, S3, S4, RC, or R6 object.
#' @keywords internal
#' @noRd
.paradigm <- function(x) {
  xClass <- class(x)

  if (is.null(attr(x, "class"))) "implicit"
  else if (isS4(x)) ifelse(inherits(x, "refClass"), "Reference class", "S4 class")
  else if (inherits(x, "R6")) "R6 class"
  else if (length(xClass) > 0) "S3 class"
  else "unknown"
}



#' huh
#'
#' Creates a detailed summary of the nature of an object.
#'
#' @param x An R object
#'
#' @return A list of class `huh` containing the specification of the object.
#' @export
#'
#' @examples
#' huh(mean)
#' huh(1:3)
huh <- function(x) {
  if (is.null(x)) return(NULL)

  dims <- dim(x)
  dims <-
    if (is.null(dims))
      if (!is.vector(x) && !is.atomic(x)) 0L
      else 1L
    else length(dims)

  xClass <- class(x)
  attributes(xClass) <- NULL

  y <- list(
    name = deparse(substitute(x)),
    type = typeof(x),
    class = xClass,
    mode = mode(x),
    dimensions = dims,
    paradigm = .paradigm(x)
  )
  class(y) <- "huh"

  return(y)
}

