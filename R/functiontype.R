

#' .knownMethodsS3
#'
#' @param nsName A string identifying a namespace
#'
#' @return A list of names known to be S3 methods in the given
#' namespace.
#'
#' @keywords internal
#' @noRd
.knownMethodsS3 <- function(nsName = NULL) {
  if (is.null(nsName) || identical(nsName, "base")) {
    return(
      asNamespace("base", base.OK = TRUE)[[".__S3MethodsTable__."]] |>
        as.list() |>
        names()
    )
  } else {
    if (!is.character(nsName)) stop("Namespace name is not a character string")
    if (length(nsName) == 0) stop("Namespace name is empty")
    ns <- asNamespace(nsName, base.OK = FALSE)
    return(get("S3methods", envir = ns[[".__NAMESPACE__."]])[, 3L])
  }
}



#' .knownInternalGenericS3
#'
#' Provide a list with internal generics.
#'
#' @return a vector of function names
#'
#' @keywords internal
.knownInternalGenericS3 <- function() {
  knownGenerics <- NULL

  # Get the name of all known S3 generic functions
  knownGenerics <- c(names(.knownS3Generics), names(.GenericArgsEnv))

  # tools:::.get_internal_S3_generics() if available
  ns <- getNamespace("tools")
  if (exists(".get_internal_S3_generics", envir=ns, inherits=FALSE)) {
    names <- get(".get_internal_S3_generics", envir=ns, inherits=FALSE)()
    knownGenerics <- c(knownGenerics, names)
  }
  unique(knownGenerics)
} # .knownInternalGenericS3()





#' .callsUseMethod
#'
#' Searches for a call of `UseMethod` in the body of a function to
#' determine if a function is an S3 generic function.
#'
#' @param x A function body
#' @details This version is an improved version of the undocumented
#' `roxygen2:::calls_use_method()` with almost 3 times the speed.
#' The gain in performance is due to 2 reasons: if `UseMethod`
#' is not the first element of the body there are most likely
#' argument checks before the call. Hence, this function starts the search
#' from the end. And, secondly, the original function coerces the body to
#' a list which seems to be costly.
#'
#' Unlike `utils::isS3stdGeneric()` this function finds the `UseMethod` call
#' anywhere inside the function, while the utils package does not consider
#' such functions to be "standard generics".
#'
#' @note Be careful how you use this. A call to `UseMethod` in a local function
#' does not produce a dispatch on it's outer function.
#' @return TRUE/FALSE
#' @keywords internal
#' @noRd
#' @references Taken from [roxygen2](https://github.com/r-lib/roxygen2/) and optimised.
.callsUseMethod <- function(x) {
  # Base cases
  if (missing(x)) return(FALSE)
  if (!is.call(x)) return(FALSE)
  if (identical(x[[1L]], quote(UseMethod))) return(TRUE)
  if (length(x) == 1L) return(FALSE)

  # Recursive case: arguments to call
  for (narg in length(x):2L) {
    if (.callsUseMethod(x[[narg]])) return(TRUE)
  }

  FALSE
}


#' .isS3Generic
#'
#' Determine if a function is an S3 generic.
#'
#' @param name A name of a function (i.e. a character string or symbol).
#' @param env Base environment in which to look for function definition.
#' @return TRUE/FALSE
#' @details
#' `.isS3Generic` looks at the function body to see if it
#' calls [UseMethod()]. If not, it compares the function name to
#' `.knownInternalGenericS3()` to make sure it also recognises internal or primitive
#' generics. That way the function may not be able to distinguish between
#' objects and functions with same name. Example: `c <- data.frame(); .isS3Generic("c")`
#' does not return `FALSE` as a test of other logicals would. Since `c()` exists
#' in `.knownInternalGenericS3()` this function will return `TRUE`.
#'
#' @keywords internal
#' @references Adapted from [roxygen2](https://github.com/r-lib/roxygen2/) and optimised.
#' @noRd
.isS3Generic <- function(name, env = parent.frame()) {
  if (missing(name)) stop("Missing function name. Nothing to do.")

  if (is.name(name))
    name <- as.character(name) #translate name/symbol to string
  if (is.character(name)) {
    if (name == "") return(FALSE)
    f <- get0(name, mode="function", envir = env)
    if (is.null(f)) return(FALSE)
  } else {
    stop("'name' is not of character or symbol")
  }

  #if (!exists(name, mode="function", envir = env)) return(FALSE)
  if (!is.function(f)) return(FALSE)

  if (inherits(f, "groupGenericFunction")) return(TRUE)

  if (.callsUseMethod(body(f))) return(TRUE)

  return(name %in% .knownInternalGenericS3())
}






#' .isS3Method
#'
#' Checks if `method` is the name of a valid / registered S3 method.
#'
#' @param name Function or name of function.
#' @param env Base environment in which to look for function definition.
#'
#' @return TRUE/FALSE
#'
#' @importFrom utils isS3method
#' @keywords internal
.isS3Method <- function(method, env = parent.frame())
{
  # Use `utils` until - maybe - someday we'll find something better
  utils::isS3method(method, envir = env)
}



#' Determine function type.
#'
#' This function figures out whether the input function is a
#' regular/primitive/internal function, a internal/S3/S4 generic, or a
#' S3/S4/RC method.
#'
#' @note This is function is slightly simplified as it's possible
#' for a method from one class to be a generic for another class, but that
#' seems like such a bad idea that hopefully no one has done it.
#' @note This function has been taken from `[sloop](https://github.com/r-lib/sloop)::`
#' `ftype` and optimised (by reducing dependencies and instantiating objects only when needed).
#' Unlike the version from roxygen2 this function does not distinguish internal and primitive
#' generics as I do not see what value that has for users (except may curiosity).
#'
#' @param f unquoted function name
#' @return A named list `list(type = type, paradigm = paradigm, virtual = virtual)`.
#' The entries `paradigm` and `virtual` are optional and only present when
#' the function is an OOP method. In that case `paradigm` indicates one of S3, S4,
#' RC (reference classes), or R6. `virtual` indicates if the function is a generic
#' or a method.
#' @export
#' @examples
#' ftype(`%in%`)
#' ftype(sum)
#' ftype(t.data.frame)
#' ftype(t.test) # Tricky!
#' ftype(writeLines)
#' ftype(unlist)
ftype <- function(f) {
  if (is.character(f) || is.name(f)) {
    fname <- as.character(f)
    f <- get0(f, mode="function")
    if (is.null(f)) stop("Argument 'f' is not a known function in this context")
  } else if (is.function(f)) { ##original: (!is.function(f) && !is.function(f))
    fname <- deparse(substitute(f))
  } else {
    stop("'f' is not a function")
  }

  type <- switch(tf <- typeof(f), builtin = , special = "primitive", tf) #"function"
  paradigm <- NULL
  virtual <- NULL

  if (type == "primitive") {
    if (fname %in% .knownInternalGenericS3()) {
      paradigm <- "S3"
      virtual <- "generic"
    }
  } else if (inherits(f, "standardGeneric")) {
    paradigm <- "S4"; virtual <- "generic"
  } else if (inherits(f, "MethodDefinition")) {
    paradigm <- "S4"; virtual <- "method"
  } else if (inherits(f, "refMethodDef")) {
    paradigm <- "RC"; virtual <- "method"
  } else {

    env <- parent.frame(1) ##original: rlang::caller_env(n=1) parent.frame(n + 1)

    gen <- .isS3Generic(fname, env)
    mth <- .isS3Method(fname, env)

    if (gen || mth)
      paradigm <- "S3"
    virtual <- c(if (gen) "generic", if (mth) "method")

  } #else

  result <- list(type = type, paradigm = paradigm, virtual = virtual)
  return(result[lengths(result) != 0])
}
