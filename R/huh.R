

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
  dims <- dim(x)
  dims <- ifelse(
    is.null(dims) && !is.vector(x),
    0L,
    ifelse(is.null(dims) && is.vector(x), 1L, dims))

# TODO: S4 classes
# TODO: R6 classes

  y <- list(
    type = typeof(x),
    classS3 = class(x),
    mode = mode(x),
    dimensions = dims
  )

  class(y) <- "huh"

  return(y)
}


#' print.huh
#'
#' A formatted output of a `huh` object to the console.
#'
#' @param x A `huh` object
#'
#' @return Returns `x` invisibly
#' @export
#'
#' @examples
#' print(huh(1:3))
print.huh <- function(x) {
  lwidth <- max(nchar(names(x)))
  mapply(
    \(y, n) cat(format(n, width=lwidth), ":", y, "\n"),
    x,
    names(x))

  return(invisible(x))
}
