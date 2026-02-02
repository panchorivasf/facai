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
#'   automatically selects the first 5 non-geometry columns. Column names should
#'   match those in the input data.
#'
#' @return A \code{leaflet} object that can be displayed interactively in RStudio
#'   Viewer, R Markdown documents, or Shiny applications. The map includes:
#'   \itemize{
#'     \item Multiple basemap options (OpenStreetMap, Satellite, Light, Dark,
#'           Topographic, Streets) selectable via radio buttons
#'     \item Interactive popups showing selected attribute information
#'     \item Optional country boundary overlays
#'     \item Automatic styling based on geometry type (points, polygons, or lines)
#'   }
#'
#' @details
#' \strong{CRS Handling:}
#' The function automatically handles coordinate reference systems:
#' \itemize{
#'   \item If data is already in WGS84 (EPSG:4326), no transformation occurs
#'   \item If data has a defined CRS other than WGS84, it transforms to WGS84
#'   \item If data has no CRS but coordinates suggest UTM (absolute values > 180
#'         for longitude or > 90 for latitude), it attempts to auto-detect the
#'         UTM zone and hemisphere, then transforms to WGS84
#'   \item If coordinates appear to be in decimal degrees, assumes WGS84
#' }
#'
#' \strong{Geometry Types:}
#' The function automatically detects and styles different geometry types:
#' \itemize{
#'   \item \strong{POINT}: Rendered as red circle markers
#'   \item \strong{POLYGON}: Rendered as blue polygons with semi-transparent fill
#'   \item \strong{LINE}: Rendered as blue lines
#' }
#'
#' \strong{Basemap Providers:}
#' Six basemap options are available via radio button control:
#' \itemize{
#'   \item OpenStreetMap: Standard OSM mapping
#'   \item Satellite: Esri World Imagery (aerial/satellite)
#'   \item Light: CartoDB Positron (minimal light theme)
#'   \item Dark: CartoDB Dark Matter (dark theme)
#'   \item Topographic: OpenTopoMap (topographic features)
#'   \item Streets: Esri World Street Map
#' }
#'
#' @import leaflet
#' @import sf
#' @importFrom dplyr %>%
#' @importFrom rnaturalearth ne_countries
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(leaflet)
#'
#' # Example 1: Basic usage with an sf object
#' interactive_map(plot_sf)
#'
#' # Example 2: Add country boundaries for Honduras and neighbors
#' interactive_map(plot_sf,
#'                countries = c("honduras", "guatemala", "el salvador",
#'                             "nicaragua", "belize"))
#'
#' # Example 3: Customize popup columns
#' interactive_map(plot_sf,
#'                countries = "honduras",
#'                popup_cols = c("PlotID", "plo_elevation", "plo_notes"))
#'
#' # Example 4: Use with data frame containing geometry column
#' # (will auto-detect CRS from coordinates)
#' plot_data_with_geom <- data.frame(
#'   id = 1:3,
#'   value = c(10, 20, 30),
#'   geometry = st_sfc(
#'     st_point(c(-87.5, 14.5)),
#'     st_point(c(-87.6, 14.6)),
#'     st_point(c(-87.7, 14.7)),
#'     crs = 4326
#'   )
#' )
#' interactive_map(plot_data_with_geom)
#'
#' # Example 5: Map plots outside country boundaries
#' interactive_map(plots_outside,
#'                countries = "honduras",
#'                popup_cols = c("PlotID", "plo_elevation"))
#' }
#'
#' @note
#' \itemize{
#'   \item Requires internet connection to load basemap tiles
#'   \item For large datasets (>10,000 features), consider simplifying geometries
#'         or filtering data for better performance
#'   \item Country boundaries are obtained from Natural Earth data via the
#'         \code{rnaturalearth} package at medium scale (1:50m)
#'   \item UTM zone auto-detection assumes coordinates are in meters and uses
#'         a simple centroid-based calculation
#' }
#'
#' @seealso
#' \code{\link[leaflet]{leaflet}}, \code{\link[sf]{st_transform}},
#' \code{\link[rnaturalearth]{ne_countries}}
#'
#' @export
interactive_map <- function(data,
                            countries = NULL,
                            id_col = NULL,
                            popup_cols = NULL) {

  # Convert to sf if it's a data frame
  if (!inherits(data, "sf")) {

    if (!("geometry" %in% names(data))) {
      stop("Data frame must have a 'geometry' column or be an sf object")
    }

    data <- st_as_sf(data)
  }

  # Get current CRS
  current_crs <- st_crs(data)

  # If not in WGS84, transform
  if (is.na(current_crs) || current_crs != st_crs(4326)) {

    if (is.na(current_crs)) {
      # Auto-detect UTM zone from centroid
      centroid <- st_coordinates(st_centroid(st_union(data)))
      lon <- centroid[1, "X"]
      lat <- centroid[1, "Y"]

      # Check if coordinates look like UTM (large numbers)
      if (abs(lon) > 180 || abs(lat) > 90) {
        # Estimate UTM zone from coordinate range
        utm_zone <- floor((mean(st_coordinates(data)[, "X"]) + 180) / 6) + 1

        # Determine hemisphere from Y coordinates
        hemisphere <- ifelse(mean(st_coordinates(data)[, "Y"]) > 0, "N", "S")

        # Construct EPSG code for UTM
        epsg_code <- ifelse(hemisphere == "N",
                            32600 + utm_zone,  # Northern hemisphere
                            32700 + utm_zone)  # Southern hemisphere

        message(paste0("Auto-detected UTM Zone ", utm_zone, hemisphere,
                       " (EPSG:", epsg_code, ")"))
        st_crs(data) <- epsg_code
      } else {
        # Assume WGS84
        message("Assuming WGS84 (EPSG:4326)")
        st_crs(data) <- 4326
      }
    }

    # Transform to WGS84
    data <- st_transform(data, crs = 4326)
  }

  # Create base map with multiple basemap options
  map <- leaflet(data) %>%
    # Add different base layers
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldStreetMap, group = "Streets")

  # Add country boundaries if specified
  if (!is.null(countries)) {
    library(rnaturalearth)

    country_boundaries <- ne_countries(country = countries,
                                       returnclass = "sf",
                                       scale = "medium")

    map <- map %>%
      addPolygons(data = country_boundaries,
                  fillColor = "transparent",
                  fillOpacity = 0.1,
                  color = "black",
                  weight = 2,
                  group = "Country Boundaries",
                  label = ~name)
  }

  # Create popup content
  if (is.null(popup_cols)) {
    # Auto-select interesting columns (exclude geometry)
    popup_cols <- names(data)[!names(data) %in% c("geometry", "geom")]
    popup_cols <- head(popup_cols, 5)  # Limit to first 5 columns
  }

  # Build popup HTML
  popup_html <- lapply(1:nrow(data), function(i) {
    content <- sapply(popup_cols, function(col) {
      if (col %in% names(data)) {
        paste0("<b>", col, ":</b> ", data[[col]][i])
      }
    })
    paste(content, collapse = "<br>")
  })

  # Determine geometry type and add appropriate layer
  geom_type <- as.character(st_geometry_type(data, by_geometry = FALSE))

  if (grepl("POINT", geom_type)) {
    map <- map %>%
      addCircleMarkers(
        radius = 6,
        color = "red",
        fillColor = "red",
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        popup = popup_html,
        group = "Data Points"
      )
  } else if (grepl("POLYGON", geom_type)) {
    map <- map %>%
      addPolygons(
        fillColor = "blue",
        fillOpacity = 0.4,
        color = "darkblue",
        weight = 2,
        popup = popup_html,
        group = "Data Polygons"
      )
  } else if (grepl("LINE", geom_type)) {
    map <- map %>%
      addPolylines(
        color = "blue",
        weight = 3,
        opacity = 0.7,
        popup = popup_html,
        group = "Data Lines"
      )
  }

  # Add layer control with radio buttons for basemaps
  map <- map %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite", "Light",
                     "Dark", "Topographic", "Streets"),
      overlayGroups = c("Data Points", "Data Polygons", "Data Lines",
                        "Country Boundaries")[
                          c("Data Points", "Data Polygons", "Data Lines",
                            "Country Boundaries") %in%
                            c(if(grepl("POINT", geom_type)) "Data Points",
                              if(grepl("POLYGON", geom_type)) "Data Polygons",
                              if(grepl("LINE", geom_type)) "Data Lines",
                              if(!is.null(countries)) "Country Boundaries")
                        ],
      options = layersControlOptions(collapsed = FALSE)
    )

  return(map)
}
