#' Add a single point marker in exploratory mode
#'
#' A thin wrapper around [leaflet::addCircleMarkers()] that adds a styled
#' circle marker to a Leaflet map with sensible defaults for interactive
#' exploration.
#'
#' @param map A Leaflet map object, as returned by [leaflet::leaflet()] or
#'   piped through other `leaflet` layer functions.
#' @param lng Numeric. Longitude of the point in decimal degrees.
#' @param lat Numeric. Latitude of the point in decimal degrees.
#' @param popup Character. HTML string displayed in the marker's popup on click.
#'   Defaults to `""` (no popup).
#' @param color Character. Fill color of the circle marker, as a CSS color name
#'   or hex string. Defaults to `"dodgerblue"`.
#' @param group Character. Name of the layer group the marker is assigned to,
#'   used with [leaflet::addLayersControl()] for toggling. Defaults to
#'   `"New - Exploring"`.
#'
#' @return The modified Leaflet map object (invisibly), with the circle marker
#'   added. Suitable for further piping.
#'
#' @seealso [leaflet::addCircleMarkers()]
#'
#' @examples
#' leaflet::leaflet() |>
#'   leaflet::addTiles() |>
#'   add_point(lng = -76.5, lat = 42.4, popup = "Test site")
#' @export
add_point <- function(map,
                      lng, lat,
                      popup = "",
                      color= "dodgerblue",
                      group = "New - Exploring") {
  leaflet::addCircleMarkers(
    map,
    lng     = lng,
    lat     = lat,
    radius  = 4,
    fillOpacity = 0.9,
    stroke  = FALSE,
    color   = color,
    group = group,
    popup = popup
  )
}
