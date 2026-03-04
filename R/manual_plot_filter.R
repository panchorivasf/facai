library(mapedit)
library(leaflet)
library(sf)

manual_plot_filter <- function(sf_object, object_name = "filtered_plots") {
  
  # Transform to WGS84 for leaflet
  sf_wgs84 <- st_transform(sf_object, crs = 4326)
  
  # Create base map
  base_map <- leaflet(sf_wgs84) |>
    addTiles() |>
    addCircleMarkers(radius = 5,
                     color = "red",
                     fillOpacity = 0.8,
                     popup = ~paste0("PlotID: ", PlotID))
  
  # Allow user to draw polygon to SELECT plots to KEEP
  message("Draw a polygon around the plots you want to KEEP")
  selected <- editMap(base_map)
  
  # Extract the drawn polygon
  drawn_polygon <- selected$finished
  
  if (is.null(drawn_polygon) || nrow(drawn_polygon) == 0) {
    message("No selection made")
    return(NULL)
  }
  
  # Find plots within the drawn polygon
  within_selection <- st_within(sf_wgs84, drawn_polygon, sparse = FALSE)
  filtered_sf <- sf_object[within_selection, ]
  
  # Assign to global environment with specified name
  assign(object_name, filtered_sf, envir = .GlobalEnv)
  
  message(paste0("Saved ", nrow(filtered_sf), " plots to '", object_name, "'"))
  
  return(filtered_sf)
}

manual_plot_filter(outhsf)

# Usage:
# plots_to_keep <- manual_plot_filter(plots_outside, "manual_selection")