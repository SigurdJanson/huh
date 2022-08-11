
#' how
#'
#' Prints how an object can be subset
#'
#' @param x An object
#'
#' @return `invisible(x)`
#' @export
#'
#' @examples
#' how(matrix(1:4, 2))
how <- function(x) UseMethod("how")


#' @describeIn how Default handler
#' @export
how.default <- function(x) {
  if (is.atomic(x)) {
    .how_atomic(x)
  } else {
    xclass <- attr(x, "class")
    if (!is.null(xclass)) {
      single <- sapply(xclass, \(y) getS3method("[", y, optional=TRUE))
      double <- sapply(xclass, \(y) getS3method("[[", y, optional=TRUE))
      dollar <- sapply(xclass, \(y) getS3method("$", y, optional=TRUE))

      msg <- "Supported subset operators:\n"
      oname <- deparse(substitute(x))
      if (!is.null(single)) {
        msg <- paste0(msg, " ", oname, "[c(...)]", "\n")
      }
      if (!is.null(double)) {
        msg <- paste0(msg, " ", oname, "[[...]]", "\n")
      }
      if (!is.null(dollar)) {
        msg <- paste0(msg, " ", oname, "$...", "\n")
      }
      cat(msg)
    } else {
      cat("Object not subsettable")
    }
  }
  invisible(x)
}


#' .how_atomic
#'
#' Helper for `how.default` that handles the case of atomic vectors
#'
#' @param x an atomic vector
#'
#' @return NULL
#' @keywords internal
#' @noRd
.how_atomic <- function(x) {
  print_handling.1d.atomic(x, deparse(substitute(x)))

  cat("Result: ATOMIC VECTOR", "\n")
  if (!is.null(attributes(x))) {
    cat("\n")
    cat('Access meta data using the function "attr( attributename )"', "\n")
  }
  cat("\n")
}




#' @describeIn how For arrays
#' @export
how.array <- function(x) {
  cat("one-dimensional access\n")
  print_handling.1d.atomic(x, deparse(substitute(x)))
  cat("multi-dimensional access\n")
  print_handling.nd.atomic(x, deparse(substitute(x)))
}

# how.matrix <- function(x) {
#   NextMethod("how", x)
# }

#' @describeIn how For data frames
#' @export
how.data.frame <- function(x) {
  NextMethod("how", x)
}


#' @describeIn how Complex numbers
#' @export
how.complex <- function(x) {
  NextMethod("how", x)

  cat('Access the real and imaginary part using the functions "Re()" and "Im()"', "\n")
  cat("\n")
}



#' .formatprint
#'
#' @param labels Labels to print, one for each statement
#' @param statements Statements documenting possible ways to subset the object
#' @param ... Additional arguments passed on to `format()`.
#'
#' @return `invisible(NULL)`
#' @keywords internal
#' @noRd
.formatprint <- function(labels, statements, ...) {
  .print <- function(label, statement) {
    cat(format(label, width=lwidth, ...), ": ", statement, "\n", sep="")
  }
  lwidth <- max(nchar(labels))
  mapply(.print, labels, statements)

  invisible(NULL)
}

print_handling.1d.atomic <- function(x, oname) {
  labels    <- c("Single element", "Multi-access")
  statement <- paste(oname, c("[index]", "[c(1, ...)]"), sep = "")

  .formatprint(labels, statement)
  cat("Result: ATOMIC VECTOR\n")

  return(invisible(NULL))
}

print_handling.2d.atomic <- function(x, oname) {
  labels    <- c("Single element", "Whole row", "Whole column",
                 "Row subset", "Column subset")
  statement <- c("[row, col]", "[row, ]", "[, col]", "[row, c(col1, ...)]", "[c(row1, ...), column]")
  statement <- paste(oname, statement, sep = "")
  .formatprint(labels, statement)
  cat("Result: ATOMIC VECTOR\n")

  labels    <- c("Any multi-access")
  statement <- paste(oname, c("[c(row1, row2, ...), c(col1, col2, ...)]"), sep = "")
  .formatprint(labels, statement)
  cat("> Result: MATRIX / ARRAY\n")

  return(invisible(NULL))
}


print_handling.nd.atomic <- function(x, oname) {
  labels    <- c("Single element", "Whole row", "Whole column")
  statement <- c("[row, col, ...]", "[row, , ]", "[, col, ]")
  statement <- paste(oname, statement, sep = "")
  .formatprint(labels, statement)
  cat("> Result: ATOMIC VECTOR\n")

  labels    <- c("Row subset", "Column subset", "Any multi-access")
  statement <- c("[row, col, ...]", "[row, , ]", "[, col, ]")
  statement <- paste(oname, statement, sep = "")

  .formatprint(labels, statement)
  cat("> Result: MATRIX / ARRAY\n")

  return(invisible(NULL))
}
