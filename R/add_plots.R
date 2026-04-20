#' Add plot markers to a leaflet map
#'
#' A convenience wrapper around \code{\link[leaflet]{addCircleMarkers}} for
#' displaying forest plot locations with standardised popups. Designed to be
#' used in a leaflet pipe chain.
#'
#' @param map A leaflet map object, as produced by \code{\link[leaflet]{leaflet}}
#'   or any subsequent leaflet layer function.
#' @param plot_data A data frame or \code{sf} object containing plot-level data.
#'   Must include columns: \code{Country}, \code{Site}, \code{PlotID},
#'   \code{PI}, \code{PIe}, \code{Dataset}, \code{Size}, \code{Latitude},
#'   \code{Longitude}.
#' @param color Character. Marker fill color. Default \code{"green"}.
#' @param size Numeric. Marker radius in pixels. Default \code{4}.
#' @param opacity Numeric. Marker fill opacity, between 0 and 1. Default \code{0.7}.
#'
#' @return A leaflet map object with circle markers added.
#'
#' @examples
#' \dontrun{
#' leaflet(plot_data) |>
#'   addTiles() |>
#'   add_plots(plot_data = my_plots, color = "green", size = 4)
#' }
#'
#' @importFrom leaflet addCircleMarkers
#' @export
add_plots <- function(map,
                      plot_data,
                      color = "green",
                      size = 4,
                      opacity = 0.7,
                      group = "New - Received"){

  addCircleMarkers(map,
                   data = plot_data,
                   radius = size,
                   stroke = FALSE,
                   fillOpacity = opacity,
                   color = color,
                   popup = ~paste0(
                     "Country: ", Country," <br>",
                     "Site: ",Site,"  <br>",
                     "PI: ", PI, " <br>",
                     "PIe: ", PIe," <br>",
                     "Dataset: ", Dataset," plots <br>",
                     "Censuses: ", Censuses, "<br>",
                     "PlotID: ", PlotID, " <br>",
                     "Plot Size: ", Size," ha"),
                   group = group)
}
