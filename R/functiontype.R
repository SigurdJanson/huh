

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
#'
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
#'
#' @details
#' `.isS3Generic` looks at the function body to see if it
#' calls [UseMethod()]. If not, it compares the function name to
#' `.knownInternalGenericS3()` to make sure it also recognises internal or primitive
#' generics
#'
#' @keywords internal
#' @references Taken from [roxygen2](https://github.com/r-lib/roxygen2/) and optimised.
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



#' #' Determine function type.
#' #'
#' #' This function figures out whether the input function is a
#' #' regular/primitive/internal function, a internal/S3/S4 generic, or a
#' #' S3/S4/RC method.
#' #'
#' #' @note This is function is slightly simplified as it's possible
#' #' for a method from one class to be a generic for another class, but that
#' #' seems like such a bad idea that hopefully no one has done it.
#' #' @note This function has been taken from `[roxygen2](https://github.com/r-lib/roxygen2/)::`
#' #' `ftype` and optimised (by reducing dependencies and instantiating objects only when needed).
#' #' Unlike the version from roxygen2 this function does not dinstinguish internal and primitive
#' #' generics as I do not see what value that has for users (except may curiosity).
#' #'
#' #' @param f unquoted function name
#' #' @return a character of vector of length 1 or 2.
#' #' @details -
#' #' @importFrom methods is
#' #' @_importFrom rlang enexpr
#' #' @examples
#' #' ftype(`%in%`)
#' #' ftype(sum)
#' #' ftype(t.data.frame)
#' #' ftype(t.test) # Tricky!
#' #' ftype(writeLines)
#' #' ftype(unlist)
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
  } else if (methods::is(f, "standardGeneric")) {
    src <- "S4"; branch <- "generic"
  } else if (methods::is(f, "MethodDefinition")) {
    src <- "S4"; branch <- "method"
  } else if (methods::is(f, "refMethodDef")) {
    src <- "RC"; branch <- "method"
  } else {

    fexpr <- rlang::enexpr(f) # TODO: find a way to eliminate rlang::enexpr
    env <- parent.frame(2) ##original: rlang::caller_env(n=1) parent.frame(n + 1)

    if (!is_symbol(fexpr)) {
      warning("Determination of S3 status requires function name", call. = FALSE)
      gen <- FALSE
      mth <- FALSE
    } else {
      fname <- deparse(substitute(f))
      gen <- .isS3Generic(fname, env)
      mth <- is_s3_method(fname, env)
    }

    if (gen || mth)
      src <- "S3"
    else
      src <- "function"
    branch <- c(if (gen) "generic", if (mth) "method")

  } #else

  return(c(src, branch))

  # } else {
  #   if (!is_symbol(fexpr)) {
  #     warning("Determination of S3 status requires function name", call. = FALSE)
  #     gen <- FALSE
  #     mth <- FALSE
  #   } else {
  #     fname <- as.character(fexpr)
  #     gen <- is_s3_generic(fname, env)
  #     mth <- is_s3_method(fname, env)
  #   }
  #
  #   if (!gen & !mth) {
  #     "function"
  #   } else {
  #     c("S3", if (gen) "generic", if (mth) "method")
  #   }
  # }
}
