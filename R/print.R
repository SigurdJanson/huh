
#' print.huh
#'
#' A formatted output of a `huh` object to the console.
#'
#' @param x A `huh` object
#' @param ... Additional arguments passed on to `format()`.
#'
#' @return Returns `x` invisibly
#' @export
#'
#' @examples
#' print(huh(1:3))
print.huh <- function(x, ...) {
  .print <- function(x, nx) {
    if (nx == "class") x <- paste(x, sep = ", ")
    cat(format(nx, width=lwidth, ...), ": ", x, "\n", sep="")
  }

  lwidth <- max(nchar(names(x)))
  mapply(.print, x, names(x))
  return(invisible(x))
}


#' print.huh.how
#'
#' @param x A `huh.how` object
#' @param ... Further arguments (currently ignored)
#'
#' @return `invisible(x)`
#' @export
#'
#' @examples
#' print(how(complex(1, 2, 3)))
print.huh.how <- function(x, ...) {
  if (is.null(x) || is.null(x[["ops"]])) {
    cat(x[["name"]], "is not subsettable")
    return(invisible(x))
  }

  #
  cat("Subsetting '", x[["name"]], "'", "\n", sep="")

  if (length(x[["ops"]]) > 0) {
    for(i in 1:length(x[["ops"]])) {
      cat("\n")
      cat(names(x[["ops"]][i]), "\n")
      cat(sapply(x[["ops"]][[i]], \(y) paste0(x[["name"]], y)), sep=", ")
    }
  }

  if (!is.null(x[["comments"]])) {
    cat("\n")
    sapply(x[["comments"]], cat, "\n")
  }

  return(invisible(x))
}
