
.OnAttachMessage <- function(hascli) {
  if (!hascli) {
    packageStartupMessage(
      "Welcome to 'huh!?!?'\n",
      "Please note: 'huh!?!?' is a lot more helpful when combined with the 'cli' package.\n",
      "Whether you want to use 'cli' or not is your choice.")
  }
  invisible()
}



#inherit base::.onAttach
#' @export
.onAttach <- function(libname, pkgname) {
  hascli <- requireNamespace("cli", quietly = TRUE)
  .OnAttachMessage(hascli)
  invisible()
}
