#' Interactively Filter Spatial Points by Drawn Polygon
#'
#' Launches an interactive leaflet map allowing the user to draw a polygon
#' around the plots they want to keep. Returns and optionally saves the
#' filtered \code{sf} object to the global environment.
#'
#' @param sf_object An \code{sf} object containing the plots to filter.
#'   Must have a \code{PlotID} column for popup labels.
#' @param object_name Character. Name of the object to assign in the global
#'   environment. Defaults to \code{"filtered_plots"}.
#'
#' @return An \code{sf} object containing only the plots within the drawn
#'   polygon, or \code{NULL} if no selection was made. The result is also
#'   assigned to \code{object_name} in the global environment as a side effect.
#'
#' @details
#' The function reprojects the input to WGS84 (EPSG:4326) for display, but
#' returns the filtered object in the original CRS of \code{sf_object}.
#' The user must draw a polygon using the map toolbar and close/finish the
#' selection for results to be returned.
#'
#' @importFrom sf st_transform st_within
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom mapedit editMap
#'
#' @examples
#' \dontrun{
#' plots_to_keep <- manual_plot_filter(my_plots_sf)
#' plots_to_keep <- manual_plot_filter(my_plots_sf, "honduras_selection")
#' }
manual_plot_filter <- function(sf_object, object_name = "filtered_plots") {

  sf_wgs84 <- sf::st_transform(sf_object, crs = 4326)

  base_map <- leaflet::leaflet(sf_wgs84) |>
    leaflet::addTiles() |>
    leaflet::addCircleMarkers(radius = 5,
                              color = "red",
                              fillOpacity = 0.8,
                              popup = ~paste0("PlotID: ", PlotID))

  message("Draw a polygon around the plots you want to KEEP")
  selected <- mapedit::editMap(base_map)

  drawn_polygon <- selected$finished

  if (is.null(drawn_polygon) || nrow(drawn_polygon) == 0) {
    message("No selection made")
    return(NULL)
  }

  within_selection <- sf::st_within(sf_wgs84, drawn_polygon, sparse = FALSE)
  filtered_sf <- sf_object[within_selection, ]

  assign(object_name, filtered_sf, envir = .GlobalEnv)

  message(paste0("Saved ", nrow(filtered_sf), " plots to '", object_name, "'"))

  return(filtered_sf)
}
