

#' Determine the OOP paradigm of an object.
#'
#' @param x An object
#' @note x must be an object except functions, i.e.
#' `!(typeof(x) %in% c("closure", "builtin", "special", "function")`.
#' This function will not check the type.
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



#' new_huh
#'
#' Factory to create `huh` objects (S3).
#'
#' @param .name Name of the object
#' @param .type See `typeof()`
#' @param .class See `class()`
#' @param .mode See `mode()`
#' @param .smode See `storage.mode()`
#' @param .dims See `dims()`
#' @param .attr See `attributes()`
#' @param .paradigm The OOP paradigm the object is based on
#'
#' @return An `huh` object
#' @keywords internal
#' @noRd
new_huh <- function(.name, .type, .class, .mode, .storagemode, .dims, .attr, .paradigm) {
  if (missing(.name) || !is.character(.name))
    stop("Object specification ('huh') needs an object name")
  if (missing(.type) || !is.character(.type))
    stop("Object specification ('huh') needs a type")

  result <- list(
    name = .name,
    type = .type,
    class = .class,
    mode = .mode,
    storage.mode = .storagemode,
    dimensions = .dims,
    attr = .attr,
    paradigm = .paradigm
  )
  result <- structure(
    result[lengths(result) > 0L],
    class = "huh"
  )
  return(result)
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

  xClass <- class(x)
  attributes(xClass) <- NULL # classes may have names and stuff which we don't need

  xStorageMode <- storage.mode(x)

  if (xStorageMode == "function") { # closure, builtin, special, function
    fType <- ftype(substitute(x))
    xType <- fType$type
    xParadigm <- c(fType$paradigm, fType$virtual)
    dims <- NULL
  } else {
    xType <- typeof(x)
    xParadigm <- c(.paradigm(x), "object")

    dims <- dim(x)
    dims <-
      if (is.null(dims))
        if (!is.vector(x) && !is.atomic(x)) 0L
        else 1L
      else length(dims)
  }

  xAttr <- names(attributes(x))

  return(
    new_huh(
      .name = deparse(substitute(x)),
      .type = typeof(x),
      .class = xClass,
      .mode = mode(x),
      .storagemode = xStorageMode,
      .dims = dims,
      .attr = xAttr,
      .paradigm = xParadigm
    )
  )
}

