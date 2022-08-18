
# Attributes that are removed from the list of attributes
# because they are displayed otherwise or are just obvious.
.redundantAttrs <- c("class", "dim")


#' .tableprint
#'
#' @param labels Labels to print, one for each statement
#' @param statements Statements documenting possible ways to subset the object
#' @param enum Specify how statements should be treated when they have more than
#' one element. See details.
#' @param ... Additional arguments passed on to `format()`.
#' @details The argument `enum` changes the way each statement is printed when
#' it contains several values. Normally a statement "class" is printed "matrix array.
#' If you add a list element `class=", "` you specify a separator for the values and
#' get an output like this: "matrix, array". `class="_"` would yield "matrix_array".
#'
#' @return `invisible(NULL)`
#' @keywords internal
#' @noRd
.tableprint <- function(labels, statements, enum = NULL, ...) {
  .print <- function(l, s) {
    if (!is.null(enum) && l %in% names(enum))
      s <- paste0(s, collapse = enum[[l]])
    cat(format(l, width=lwidth, ...), ": ", s, "\n", sep="")
  }
  lwidth <- max(nchar(labels))
  mapply(.print, labels, statements)

  invisible(NULL)
}



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
  x$attr <- x$attr[!(x$attr %in% .redundantAttrs)]

  .tableprint(names(x), x, enum=list(class=", "))
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
