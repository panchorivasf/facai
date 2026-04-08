#' Create an Interactive Leaflet Map with Multiple Basemaps
#'
#' This function creates an interactive leaflet map from spatial data with
#' automatic CRS detection and transformation to WGS84. It provides multiple
#' basemap options accessible through a radio button control (similar to Google
#' Maps layer switcher) and optional country boundary overlays.
#'
#' @param data An \code{sf} object or a data frame/tibble/data.table containing
#'   a \code{geometry} column. If the CRS is not specified or not WGS84, the
#'   function will attempt to auto-detect the UTM zone based on coordinate
#'   ranges and transform to WGS84 (EPSG:4326) for display.
#' @param countries Character vector of country names to overlay as boundaries.
#'   Country names should match those recognized by \code{rnaturalearth::ne_countries()}.
#'   Default is \code{NULL} (no country boundaries displayed). Examples:
#'   \code{"honduras"}, \code{c("honduras", "guatemala", "nicaragua")}.
#' @param id_col Character string specifying the column name to use as a unique
#'   identifier in popups. Currently not implemented but reserved for future use.
#'   Default is \code{NULL}.
#' @param popup_cols Character vector specifying which columns to display in
#'   the popup when clicking on features. If \code{NULL} (default), the function
#'   automatically selects the first 5 non-geometry columns.
#'
#' @return A \code{leaflet} object. To save as a standalone HTML file use:
#'   \code{htmlwidgets::saveWidget(map, "map.html", selfcontained = TRUE)}
#'   The \code{selfcontained = TRUE} argument is important — it bundles all
#'   JS/CSS so the map renders correctly when opened from a local file path.
#'
#' @import leaflet
#' @import sf
#' @importFrom dplyr %>%
#' @importFrom rnaturalearth ne_countries
#'
#' @export
interactive_map <- function(data,
                            countries = NULL,
                            id_col = NULL,
                            popup_cols = NULL) {

  # Convert to sf if it's a data frame with a geometry column
  if (!inherits(data, "sf")) {
    if (!("geometry" %in% names(data))) {
      stop("Data frame must have a 'geometry' column or be an sf object")
    }
    data <- sf::st_as_sf(data)
  }

  # CRS handling: transform to WGS84 if needed
  current_crs <- sf::st_crs(data)

  if (is.na(current_crs) || current_crs != sf::st_crs(4326)) {

    if (is.na(current_crs)) {
      centroid <- sf::st_coordinates(sf::st_centroid(sf::st_union(data)))
      lon <- centroid[1, "X"]
      lat <- centroid[1, "Y"]

      if (abs(lon) > 180 || abs(lat) > 90) {
        utm_zone  <- floor((mean(sf::st_coordinates(data)[, "X"]) + 180) / 6) + 1
        hemisphere <- ifelse(mean(sf::st_coordinates(data)[, "Y"]) > 0, "N", "S")
        epsg_code  <- ifelse(hemisphere == "N", 32600 + utm_zone, 32700 + utm_zone)
        message(paste0("Auto-detected UTM Zone ", utm_zone, hemisphere,
                       " (EPSG:", epsg_code, ")"))
        sf::st_crs(data) <- epsg_code
      } else {
        message("Assuming WGS84 (EPSG:4326)")
        sf::st_crs(data) <- 4326
      }
    }

    data <- sf::st_transform(data, crs = 4326)
  }

  # Base map ----------------------------------------------------------------
  # NOTE: OpenStreetMap and OpenTopoMap block tile requests from local HTML
  # files (file:// referer policy). All providers below work without a referer
  # and are safe for htmlwidgets::saveWidget(..., selfcontained = TRUE).
  map <- leaflet::leaflet(data) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                              group = "Light")         |>  # default base layer
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                              group = "Satellite")     |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter,
                              group = "Dark")          |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap,
                              group = "Streets")       |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap,
                              group = "Topographic")   # replaces OpenTopoMap

  # Country boundaries ------------------------------------------------------
  if (!is.null(countries)) {
    country_boundaries <- rnaturalearth::ne_countries(
      country      = countries,
      returnclass  = "sf",
      scale        = "medium"
    )

    map <- map |>
      leaflet::addPolygons(
        data        = country_boundaries,
        fillColor   = "transparent",
        fillOpacity = 0.1,
        color       = "black",
        weight      = 2,
        group       = "Country Boundaries",
        label       = ~name
      )
  }

  # Popup content -----------------------------------------------------------
  if (is.null(popup_cols)) {
    popup_cols <- names(data)[!names(data) %in% c("geometry", "geom")]
    popup_cols <- head(popup_cols, 5)
  }

  popup_html <- lapply(seq_len(nrow(data)), function(i) {
    content <- sapply(popup_cols, function(col) {
      if (col %in% names(data)) paste0("<b>", col, ":</b> ", data[[col]][i])
    })
    paste(content, collapse = "<br>")
  })

  # Geometry layer ----------------------------------------------------------
  geom_type <- as.character(sf::st_geometry_type(data, by_geometry = FALSE))

  if (grepl("POINT", geom_type)) {
    map <- map |>
      leaflet::addCircleMarkers(
        radius      = 2,
        fillColor       = "red",
        fillOpacity = 0.7,
        stroke      = FALSE,
        # weight      = 2,
        popup       = popup_html,
        group       = "Data Points"
      )
  } else if (grepl("POLYGON", geom_type)) {
    map <- map |>
      leaflet::addPolygons(
        fillColor   = "blue",
        fillOpacity = 0.4,
        color       = "darkblue",
        weight      = 2,
        popup       = popup_html,
        group       = "Data Polygons"
      )
  } else if (grepl("LINE", geom_type)) {
    map <- map |>
      leaflet::addPolylines(
        color   = "blue",
        weight  = 3,
        opacity = 0.7,
        popup   = popup_html,
        group   = "Data Lines"
      )
  }

  # Layer control -----------------------------------------------------------
  active_overlays <- c(
    if (grepl("POINT",   geom_type))   "Data Points",
    if (grepl("POLYGON", geom_type))   "Data Polygons",
    if (grepl("LINE",    geom_type))   "Data Lines",
    if (!is.null(countries))           "Country Boundaries"
  )

  map <- map |>
    leaflet::addLayersControl(
      baseGroups   = c("Light", "Satellite", "Dark", "Streets", "Topographic"),
      overlayGroups = active_overlays,
      options      = leaflet::layersControlOptions(collapsed = FALSE)
    )

  return(map)
}
