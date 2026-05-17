#' @export
wd <- function(path) {
  options(facai.prev_wd = getwd())
  setwd(path)
  invisible(getwd())
}

#' @export
goback <- function(verbose = TRUE) {
  prev <- getOption("facai.prev_wd")
  if (is.null(prev))
    cli::cli_abort("No previous working directory recorded. Use {.fn wd} first.")
  options(facai.prev_wd = getwd())
  setwd(prev)
  if (verbose) message("Working directory: ", prev)
  invisible(prev)
}
