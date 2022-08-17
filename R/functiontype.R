

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
  if (is.null(nsName) || nsName == "base") {
    return(
      asNamespace("base", base.OK = TRUE)[[".__S3MethodsTable__."]] |>
        as.list() |>
        names()
    )
  } else {
    if (!is.character(nsName)) stop("Namespace name is not a character string")
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
#' @param name Function or name of function.
#' @param env Base environment in which to look for function definition.
#' @return TRUE/FALSE
#' @details
#' If the argument `name` is a function it will try to determine it's name with
#' `deparse(substitute(name))`. If a function has been passed on through several functions
#' The original name may not be determined anymore.
#'
#' `.isS3Generic` looks at the function body to see if it
#' calls [UseMethod()]. If not, it compares the function name to
#' `.knownInternalGenericS3()` to make sure it also recognises internal or primitive
#' generics
#'
#' @keywords internal
#' @references Adapted from [roxygen2](https://github.com/r-lib/roxygen2/) and optimised.
#' @noRd
.isS3Generic <- function(name, env = parent.frame()) {
  if (missing(name)) stop("Missing function namey. Nothing to do.")
  if (name == "") return(FALSE)

  if (is.character(name)) {
    f <- get(name, envir = env)
  } else {
    f <- name
    name <- deparse(substitute(f))
  }

  if (!exists(name, envir = env)) return(FALSE)
  if (!is.function(f)) return(FALSE)

  if (inherits(f, "groupGenericFunction")) return(TRUE)

  if (.callsUseMethod(body(f))) return(TRUE)

  return(name %in% .knownInternalGenericS3())
}





# .isS3Method <- function(name, env = parent.frame()) {
#   if (missing(name)) stop("Missing function name. Nothing to do.")
#   if (name == "") return(FALSE)
#
#   if (is.character(name)) {
#     f <- get(name, envir = env)
#   } else {
#     f <- name
#     name <- deparse(substitute(f))
#   }
#
#   # Search in the last environment of the search path first
#   # (assumption: we get most hits there)
#   knowns <- .knownMethodsS3()
#   if (name %in% knowns)
#     return(TRUE)
#   else {
#     if (identical(env, globalenv())) return(FALSE)
#
#     # Search through the search path
#
#     # TODO: continue
#     ns <- asNamespace(environmentName(env), base.OK = TRUE)
#     nsName <- getNamespaceInfo(ns, "spec")[["name"]]
#     knowns <- .knownMethodsS3(nsName)
#   }
#
#   return()
# }



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
#' @note This function has been taken from `[roxygen2](https://github.com/r-lib/roxygen2/)::`
#' `ftype` and optimised (by reducing dependencies and instantiating objects only when needed).
#' Unlike the version from roxygen2 this function does not distinguish internal and primitive
#' generics as I do not see what value that has for users (except may curiosity).
#'
#' @param f unquoted function name
#' @return a character of vector of length 1 or 2.
#' @details -
#' @examples
#' ftype(`%in%`)
#' ftype(sum)
#' ftype(t.data.frame)
#' ftype(t.test) # Tricky!
#' ftype(writeLines)
#' ftype(unlist)
######## @importFrom rlang enexpr
ftype <- function(f) {
  if (!is.function(f)) ##original: (!is.function(f) && !is.function(f))
    stop("`f` is not a function")

  src <- ""
  branch <- ""

  ##original:
  ## here we do not distinguish between "internal generics" and "primitive generics"
  # if (is.primitive(f)) {
  #   c("primitive", if (is_internal_generic(primitive_name(f))) "generic")
  # } else if (is_internal(f)) {
  #   c("internal", if (is_internal_generic(internal_name(f))) "generic")
  if (is.primitive(f)) {
    src <- "primitive"
    if (f %in% names(.GenericArgsEnv)) #TODO: further optimise this check
      branch <- "generic"
    else
      branch <- ""
  } else if (inherits(f, "standardGeneric")) {
    src <- "S4"; branch <- "generic"
  } else if (inherits(f, "MethodDefinition")) {
    src <- "S4"; branch <- "method"
  } else if (inherits(f, "refMethodDef")) {
    src <- "RC"; branch <- "method"
  } else {

    #fexpr <- rlang::enexpr(f) # TODO: find a way to eliminate rlang::enexpr
    fexpr <- substitute(f)
    env <- parent.frame(2) ##original: rlang::caller_env(n=1) parent.frame(n + 1)

    if (typeof(fexpr) != "symbol") { ##original: if (!is_symbol(fexpr))
      warning("Determination of S3 status requires function name", call. = FALSE)
      gen <- FALSE
      mth <- FALSE
    } else {
      fname <- deparse(substitute(f))
      gen <- .isS3Generic(fname, env)
      mth <- .isS3Method(fname, env)
    }

    if (gen || mth)
      src <- "S3"
    else
      src <- "function"
    branch <- c(if (gen) "generic", if (mth) "method")

  } #else

  return(c(src, branch))
}
