
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
  if (is.atomic(x) && is.null(attr(x, "class"))) {
    result <- .how_atomic(x, deparse(substitute(x)))
  } else {
    xclass <- attr(x, "class")
    if (!is.null(xclass)) {
      single <- sapply(xclass, \(y) getS3method("[", y, optional=TRUE))
      double <- sapply(xclass, \(y) getS3method("[[", y, optional=TRUE))
      dollar <- sapply(xclass, \(y) getS3method("$", y, optional=TRUE))

      ops <- list()
      if (!all(sapply(single, is.null))) ops <- c(ops, c("[c(...)]"))
      if (!all(sapply(double, is.null))) ops <- c(ops, c("[[...]]"))
      if (!all(sapply(dollar, is.null))) ops <- c(ops, c("$..."))

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
#' @param name The original name of `x`
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
  names(result$ops)[names(result$ops) == "vector"] <- typeof(x)
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
  result <- new_huh.how(
    .name = deparse(substitute(x)),
    .ops = list(
      vector = c("[c(...)]", "[c(...), ..., ...]", "[[...]]"),
      matrix = c("[c(...), c(...), ...]"),
      array  = c("[c(...), c(...), c(...)]")
    ),
    .comments = c("The result is coerced to the lowest possible dimension. I.e. ...",
                  "* 1-dim results are atomic vectors,",
                  "* 2-dim results are 'matrix' AND 'array',",
                  "* 3-dim results are arrays.",
                  "Add the argument `drop=FALSE` to keep the class")
  )
  names(result$ops)[names(result$ops) == "vector"] <- typeof(x)
  return(result)
}


#' @describeIn how For matrices
#' @export
how.matrix <- function(x) {
  result <- new_huh.how(
    .name = deparse(substitute(x)),
    .ops = list(
      vector = c("[c(...)]", "[c(...), ...]", "[[...]]"),
      matrix = c("[c(...), c(...)]")
    ),
    .comments = c("The result is coerced to the lowest possible dimension. I.e. ...",
                  "* 1-dim results yield atomic vectors,",
                  "* 2-dim results class 'matrix'",
                  "Add the argument `drop=FALSE` to keep the class")
  )
  names(result$ops)[names(result$ops) == "vector"] <- typeof(x)
  return(result)
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
    c(
      sprintf("Access the levels using 'levels(%s)' or 'nlevels(%s)'", result$name, result$name),
      "By default, subsetting with `[]` keeps all factor levels.",
      "Add the argument `drop=TRUE` to keep only needed levels."
    ),
    result$comments)
  return(result)
}


#' @describeIn how Expressions
#' @export
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


#' @describeIn how Environments
#' @export
how.environment <- function(x) {
  new_huh.how(
    .name = deparse(substitute(x)),
    .ops = list(
      `mixed types` = c("[[...]]", "$...")
    ),
    .comments = c("Only character indices are allowed and no partial matching is done.")
  )
}


#' @describeIn how Quosures (see [rlang::quosure-tools])
#' @export
how.quosure <- function(x) {
  new_huh.how(
    .name = deparse(substitute(x)),
    .ops = list(
      NULL
    ),
    .comments = c("Subsetting quosures has been deprecated.")
  )
}


