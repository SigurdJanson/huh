
#' new_huh.how
#'
#' Factory to create `huh.how` objects (S3).
#'
#' @param .name Name of the object
#' @param .ops A list describing available subsetting operators
#' @param .comments Additional comments
#'
#' @return An `huh.how` object
#' @keywords internal
#' @noRd
new_huh.how <- function(.name, .ops, .comments = NULL) {
  if (missing(.name) || !is.character(.name))
    stop("Subsetting specification needs an object name")
  hasops <- !(missing(.ops) || is.null(.ops) || all(is.na(.ops)) || length(.ops) == 0)
  if (hasops && !is.list(.ops)) .ops <- list(.ops)

  result <- structure(
    list(
      name = .name,
      ops = if (hasops) .ops else NULL,
      comments = .comments
    ),
    class = "huh.how"
  )
  return(result)
}

#' how
#'
#' Prints how an object can be subset
#'
#' @param x An object
#'
#' @return An object of class `huh.how`.
#' @export
#'
#' @examples
#' how(matrix(1:4, 2))
how <- function(x) UseMethod("how")


#' @describeIn how Default handler
#' @importFrom utils getS3method
#' @export
how.default <- function(x) {
  if (is.atomic(x)) {
    result <- .how_atomic(x, deparse(substitute(x)))
  } else {
    xclass <- attr(x, "class")
    if (!is.null(xclass)) {
      single <- sapply(xclass, \(y) getS3method("[", y, optional=TRUE))
      double <- sapply(xclass, \(y) getS3method("[[", y, optional=TRUE))
      dollar <- sapply(xclass, \(y) getS3method("$", y, optional=TRUE))

      ops <- list()
      if (!is.null(single)) ops <- c(ops, c("[c(...)]"))
      if (!is.null(double)) ops <- c(ops, c("[[...]]"))
      if (!is.null(dollar)) ops <- c(ops, c("$..."))

      result <-
        new_huh.how(
          .name = deparse(substitute(x)),
          .ops = ops,
          .comments = "Return types cannot be determined"
      )
    } else {
      result <-
        new_huh.how(
          .name = deparse(substitute(x)),
          .ops = NULL,
          .comments = "Object not subsettable"
        )
    }
  }
  return(result)
}


#' .how_atomic
#'
#' Helper for `how.default` that handles the case of atomic vectors
#'
#' @param x an atomic vector
#'
#' @return An object of class `huh.how`.
#' @keywords internal
#' @noRd
.how_atomic <- function(x, name) {
  hasattrs <- !is.null(attributes(x))

  if (hasattrs)
    result <- new_huh.how(
      .name = name,
      .ops = list(vector = c("[c(...)]", "[[...]]")),
      .comments = sprintf("Access meta data using the function 'attr(%s)'", name)
    )
  else
    result <- new_huh.how(
      .name = name,
      .ops = list(vector = c("[c(...)]", "[[...]]"))
    )
  return(result)
}


#' @describeIn how For lists
#' @export
how.list <- function(x) {
  new_huh.how(
    .name = deparse(substitute(x)),
    .ops = list(
      list = c("[c(...)]"),
      reduced = c("[[...]]", "$...")
    ),
    .comments = NULL
  )
}



#' @describeIn how For arrays
#' @export
how.array <- function(x) {
  new_huh.how(
    .name = deparse(substitute(x)),
    .ops = list(
      vector = c("[c(...)]", "[c(...), ..., ...]", "[[...]]"),
      matrix = c("[c(...), c(...), ...]"),
      array  = c("[c(...), c(...), c(...)]")
    ),
    .comments = c("Class will be reduced to leanest possible type. I.e. ...",
      "* 1-dim results yield atomic vectors,",
      "* 2-dim results class 'matrix' AND 'array';",
      "* 3-dim results are arrays.")
  )
}


#' @describeIn how For matrices
#' @export
how.matrix <- function(x) {
  new_huh.how(
    .name = deparse(substitute(x)),
    .ops = list(
      vector = c("[c(...)]", "[c(...), ...]", "[[...]]"),
      matrix = c("[c(...), c(...)]")
    ),
    .comments = c("Class will be reduced to leanest possible type. I.e. ...",
                  "* 1-dim results yield atomic vectors,",
                  "* 2-dim results class 'matrix'")
  )
}



#' @describeIn how For data frames
#' @export
how.data.frame <- function(x) {
  new_huh.how(
    .name = deparse(substitute(x)),
    .ops = list(
      scalar = c("[..., ...]"),
      `column(s) as list` = c("[c(...)]"),
      `column(s) as atomic` = c("[[...]]", "[, ...]", "$..."),
      `data frame`= c("[c(...), c(...)]")
    ),
    .comments = "Any subset that covers more than 1 row returns a data frame"
  )
}


#' @describeIn how Complex numbers
#' @export
how.complex <- function(x) {
  name <- deparse(substitute(x))
  result <- .how_atomic(x, name)

  result$comments <- append(
    result$comments,
    sprintf("Access the real and imaginary part using the functions 'Re(%s)' and 'Im(%s)'", name, name))
  return(result)
}



#' @describeIn how Factors
#' @export
how.factor <- function(x) {
  result <- .how_atomic(x, deparse(substitute(x)))

  result$comments <- append(
    sprintf("Access the levels using 'levels(%s)' or 'nlevels(%s)'", result$name, result$name),
    result$comments)
  return(result)
}


how.expression <- function(x) {
  new_huh.how(
    .name = deparse(substitute(x)),
    .ops = list(
      expression = c("[c(...)]"),
      `mixed types` = c("[[...]]")
    ),
    .comments = NULL
  )
}



# print_handling.2d.atomic <- function(x, oname) {
#   labels    <- c("Single element", "Whole row", "Whole column",
#                  "Row subset", "Column subset")
#   statement <- c("[row, col]", "[row, ]", "[, col]", "[row, c(col1, ...)]", "[c(row1, ...), column]")
#   statement <- paste(oname, statement, sep = "")
#   .formatprint(labels, statement)
#   cat("Result: ATOMIC VECTOR\n")
#
#   labels    <- c("Any multi-access")
#   statement <- paste(oname, c("[c(row1, row2, ...), c(col1, col2, ...)]"), sep = "")
#   .formatprint(labels, statement)
#   cat("> Result: MATRIX / ARRAY\n")
#
#   return(invisible(NULL))
# }


# print_handling.nd.atomic <- function(x, oname) {
#   labels    <- c("Single element", "Whole row", "Whole column")
#   statement <- c("[row, col, ...]", "[row, , ]", "[, col, ]")
#   statement <- paste(oname, statement, sep = "")
#   .formatprint(labels, statement)
#   cat("> Result: ATOMIC VECTOR\n")
#
#   labels    <- c("Row subset", "Column subset", "Any multi-access")
#   statement <- c("[row, col, ...]", "[row, , ]", "[, col, ]")
#   statement <- paste(oname, statement, sep = "")
#
#   .formatprint(labels, statement)
#   cat("> Result: MATRIX / ARRAY\n")
#
#   return(invisible(NULL))
# }
