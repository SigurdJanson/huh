
# Attributes that are removed from the list of attributes
# because they are displayed otherwise or are just obvious.
.redundantAttrs <- c("class", "dim")

# Attributes that are only printed with language=S
.sTypes <- c("mode", "storage.mode")

.huh.help = list(
  type   = "{.help [type](base::typeof)}",
  class  = "{.help [class](base::class)}",
  mode   = "{.help [mode](base::mode)}",
  `storage mode` = "{.help [storage mode](base::mode)}"#,
  #dimensions = "{.run [dimensions](base::dim({%s}))}" #{.run [snapshot_review()](testthat::snapshot_review())}
)

.basetypes <- c("logical", "integer", "double", "numeric", "complex", "character", "raw",
                "list", "data.frame", "ts", "array", "matrix",
                "call", "expression", "name")




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
#' * Dots in labels are replaced with spaces.
#' * NULL elements are dropped.
#' @return `invisible(NULL)`
#' @keywords internal
#' @noRd
#-----importFrom cli cli_div cli_end cli_text
.tableprint <- function(labels, statements, enum = NULL, ...) {
  .print <- function(l, s) {
    l <- gsub("\\.", " ", l)
    if (!is.null(enum) && l %in% names(enum))
      s <- paste0(s, collapse = enum[[l]])

    if (usecli) {
      if (l %in% names(.huh.help))
        l <- .huh.help[[l]]
      if (s %in% .basetypes)
        s <- sprintf("{.topic [%s](base::%s)}", s, s)
      cli::cli_text(paste(format(l, width=lwidth, ...), ": ", s, "\n", sep=""))
    } else {
      cat(format(l, width=lwidth, ...), ": ", s, "\n", sep="")
    }
  }

  usecli <- requireNamespace("cli", quietly = TRUE)

  # Remove NULL elements, first
  labels <- labels[lengths(statements) > 0]
  statements <- statements[lengths(statements) > 0]

  if (usecli)
    cli_container <- cli::cli_div()
  # Column width for labels
  lwidth <- max(nchar(labels))
  mapply(.print, labels, statements)

  if (usecli)
    cli_container <- cli::cli_end(cli_container)

  invisible(NULL)
}



#' print.huh
#'
#' A formatted output of a `huh` object to the console.
#'
#' @param x A `huh` object
#' @param lang Indicates which properties should be printed.
#' Either "R" (default) or "S". See details.
#' @param ... Additional arguments passed on to `format()`.
#' @details `lang="S"` also prints an object's \code{\link[=mode]{mode}}
#' and \code{\link[=storage.mode]{storage mode}}.
#' These properties are redundant in R, where you only need [typeof()].
#' Mode and storage mode "exist solely for S compatibility" (Wickham, 2017)
#' and are usually not displayed.
#' @return Returns `x` invisibly
#' @references
#' Wickham, H. (2017). [Advanced R](http://adv-r.had.co.nz/OO-essentials.html). 1st edition.
#' @export
#'
#' @examples
#' print(huh(1:3))
#' print(huh(matrix(1:12, 4)), lang="S")
print.huh <- function(x, lang = c("R", "S"), ...) {
  lang <- match.arg(lang)
  px <- switch(lang,
         R = x[!(names(x) %in% .sTypes)],
         S = x)

  px$attr <- x$attr[!(x$attr %in% .redundantAttrs)]

  .tableprint(names(px), px, enum=list(class=", ", paradigm=", ", attr=", "))
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
