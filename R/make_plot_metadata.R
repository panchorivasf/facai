#' Create a plot metadata tibble
#'
#' Interactively builds a tibble of plot-level metadata for use in the
#' InterNodes contributor registry and leaflet map visualisation.
#'
#' @param country Character. Official UN country name (use \code{get_country_code()}
#'   to look up the correct name).
#' @param site Character. Site or forest name.
#' @param pi Character. Full name of the principal investigator.
#' @param pie Character. Email address of the principal investigator.
#' @param plot_ids Character vector. Vector of PlotIDs.
#' @param size Numeric. Plot area in hectares (scalar, applied to all plots).
#' @param latitude Numeric. Latitude in decimal degrees (scalar, applied to all plots).
#' @param longitude Numeric. Longitude in decimal degrees (scalar, applied to all plots).
#' @param data Optional data frame with columns \code{PlotID}, \code{Size},
#'   \code{Latitude}, \code{Longitude}. If provided, these columns are taken
#'   from the data frame and the scalar arguments are ignored.
#'
#' @return A tibble with one row per plot and columns: \code{Country},
#'   \code{Site}, \code{PI}, \code{PIe}, \code{Dataset}, \code{PlotID},
#'   \code{Size}, \code{Latitude}, \code{Longitude}.
#'
#' @examples
#' \dontrun{
#' ituri_xy <- make_plot_metadata(
#'   country   = "Democratic Republic of the Congo",
#'   site      = "Ituri",
#'   pi        = "Jean-Remy Makana",
#'   pie       = "jeanremy@example.com",
#'   plot_ids  = c("in_co_edoro_1", "in_co_edoro_2", "in_co_lenda_1"),
#'   size      = 10,
#'   latitude  = 1.4368,
#'   longitude = 28.5826
#' )
#' }
#'
#' @importFrom tibble tibble
#' @export
make_plot_metadata <- function(country,
                               site,
                               pi,
                               pie,
                               plot_ids  = NULL,
                               size      = NULL,
                               latitude  = NULL,
                               longitude = NULL,
                               data      = NULL) {
  if (!is.null(data)) {
    plot_ids  <- data$PlotID
    size      <- data$Size
    latitude  <- data$Latitude
    longitude <- data$Longitude
  }

  tibble::tibble(
    Country   = country,
    Site      = site,
    PI        = pi,
    PIe       = pie,
    Dataset   = length(plot_ids),
    PlotID    = plot_ids,
    Size      = size,
    Latitude  = latitude,
    Longitude = longitude
  )
}
