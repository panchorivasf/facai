#' Launch the gfb3 Formatter Shiny App
#'
#' Opens an interactive Shiny application for reformatting forest inventory
#' data into the Global Forest Biodiversity Initiative (gfb3) format.
#' Supports column mapping, plot-level constants, status code translation,
#' and merging of multiple censuses with automatic derivation of PrevDBH,
#' PrevYr, and Status.
#'
#' @param ... Arguments passed to \code{\link[shiny]{runApp}}, e.g.
#'   \code{port}, \code{launch.browser}.
#'
#' @return Called for its side effect (launches the app). Returns invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#'   gfb3_formatter()
#' }
gfb3_formatter <- function(...) {
  app_dir <- system.file("shiny", "gfb3_formatter", package = "facai")
  if (app_dir == "") {
    stop(
      "Could not find the gfb3_formatter app. ",
      "Try reinstalling facai with: devtools::install_github('your-org/facai')",
      call. = FALSE
    )
  }
  shiny::runApp(app_dir, ...)
}
