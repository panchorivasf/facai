#' Return a GFB3 template tibble with correct column types
#'
#' @param n Integer. Number of empty rows to include (default 0).
#' @return A tibble with all GFB3 columns and correct data types.
#' @export
gfb3_template <- function(n = 0L) {
  template <- tibble::tibble(
    # Plot metadata
    PlotID       = character(n),
    PA    = double(n), # Plot area (ha)
    Latitude     = double(n),
    Longitude    = double(n),
    # Tree data
    TreeID       = character(n),
    Species      = character(n),
    Status       = character(n),
    DBH          = double(n),
    YR           = integer(n),
    PrevDBH      = double(n),
    PrevYR       = integer(n)
  )
  template
}
