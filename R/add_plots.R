#' Add plot markers to a Leaflet map
#'
#' @param map A `leaflet` map object.
#' @param plot_data A data frame containing plot-level data. Must include all
#'   columns referenced in `popup_fields`. When the data frame contains a
#'   \code{DatasetSize} column (as produced by \code{metadata_gfb3()}), its
#'   value is used for the *"Dataset size"* popup entry; otherwise the total
#'   number of rows in \code{plot_data} is used as a fallback.
#' @param color Character. Fill color for the circle markers. Default `"green"`.
#' @param size Numeric. Radius of the circle markers in pixels. Default `4`.
#' @param opacity Numeric. Fill opacity of the markers, between 0 and 1. Default `0.9`.
#' @param group Character. Layer group name for the markers, used for Leaflet
#'   layer controls. Default `"New - Received"`.
#' @param popup_fields Character vector of field names to display in the marker
#'   popup, in the desired order. Each element should be a column name in
#'   `plot_data`, except for the following special values which are computed
#'   automatically:
#'   \describe{
#'     \item{`"Dataset"`}{Total number of plots, taken from the
#'       \code{DatasetSize} column when present, otherwise \code{nrow(plot_data)}.
#'       Labelled *"Dataset size"* with a singular/plural suffix.}
#'     \item{`"Censuses"`}{Census range derived from the `Censuses` column,
#'       shown as a single value or `"min - max"` range.}
#'     \item{`"PlotArea"`}{Plot area from the `PlotArea` column, labelled
#'       *"Plot Area"* with a `" ha"` suffix.}
#'   }
#'   All other values are treated as bare column names and labelled with the
#'   column name itself. `NA` values are silently omitted.
#'   Defaults to `c("Country", "Site", "PI", "PIe", "Dataset", "Censuses",
#'   "PlotID", "PlotArea")`.
#'
#' @return The input `map` object with circle markers added.
#'
#' @examples
#' \dontrun{
#' library(leaflet)
#' map <- leaflet() |> addTiles()
#'
#' # Default popup fields
#' map |> add_plots(plot_data)
#'
#' # Custom subset, adding a Notes column
#' map |> add_plots(plot_data,
#'                  popup_fields = c("Country", "Site", "PI",
#'                                   "Dataset", "PlotID", "Notes"))
#' }
#'
#' @importFrom leaflet addCircleMarkers
#' @export
add_plots <- function(map,
                      plot_data,
                      color = "green",
                      size = 4,
                      opacity = 0.9,
                      group = "New - Received",
                      popup_fields = c("Country", "Site", "PI", "PIe",
                                       "Dataset", "Censuses", "PlotID", "PlotArea")) {

  # Use DatasetSize column when available (set by metadata_gfb3()); fall back
  # to nrow() for backwards compatibility with plain plot-level data frames.
  Dataset <- if ("DatasetSize" %in% names(plot_data)) {
    plot_data$DatasetSize[1]
  } else {
    nrow(plot_data)
  }

  censuses <- as.numeric(plot_data$Censuses)
  census_min <- min(censuses, na.rm = TRUE)
  census_max <- max(censuses, na.rm = TRUE)
  census_text <- if (census_min != census_max) {
    paste0(census_min, " - ", census_max)
  } else {
    as.character(census_min)
  }

  # Build popup for each row
  popup_html <- apply(plot_data, 1, function(row) {
    lines <- lapply(popup_fields, function(field) {
      val <- switch(field,
                    "Dataset"  = paste0(Dataset, ifelse(Dataset == 1, " plot", " plots")),
                    "Censuses" = census_text,
                    "PlotArea"  = paste0(row[["PlotArea"]], " ha"),
                    row[[field]]
      )
      label <- switch(field,
                      "Dataset" = "Dataset size",
                      "PlotArea" = "Plot Area",
                      field
      )
      if (!is.null(val) && !is.na(val)) paste0(label, ": ", val, " <br>") else NULL
    })
    paste(Filter(Negate(is.null), lines), collapse = "")
  })

  addCircleMarkers(map,
                   data = plot_data,
                   radius = size,
                   stroke = FALSE,
                   fillOpacity = opacity,
                   color = color,
                   popup = popup_html,
                   group = group)
}
