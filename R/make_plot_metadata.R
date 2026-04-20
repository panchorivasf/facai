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
#' @param census Character or numeric. Number of censuses (or range like \code{"2-3"}).
#'   Required when \code{data} is \code{NULL}; derived automatically from
#'   \code{YR}/\code{PrevYR} when \code{data} is in gfb3 format.
#' @param plot_ids Character vector. Vector of PlotIDs.
#' @param size Numeric. Plot area in hectares (scalar, applied to all plots).
#' @param latitude Numeric. Latitude in decimal degrees (scalar, applied to all plots).
#' @param longitude Numeric. Longitude in decimal degrees (scalar, applied to all plots).
#' @param data Optional data frame. Accepts either a plot-level data frame with
#'   columns \code{PlotID}, \code{Size}, \code{Latitude}, \code{Longitude}, or
#'   a tree-level (gfb3) data frame with columns \code{PlotID}, \code{PA},
#'   \code{Latitude}, \code{Longitude}, \code{YR}, \code{PrevYR}. In the
#'   latter case \code{plot_ids}, \code{size}, \code{latitude},
#'   \code{longitude}, and \code{census} are all derived from the data.
#' @param export_xlsx  Logical. Whether to export the table as .xlsx  file.
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
                               census    = NULL,
                               plot_ids  = NULL,
                               size      = NULL,
                               latitude  = NULL,
                               longitude = NULL,
                               data      = NULL,
                               export_xlsx = TRUE) {

  # Resolve country, disambiguate if needed
  matches <- get_country_code(country)
  if (nrow(matches) == 0) {
    stop("No country found matching '", country, "'. Use get_country_code() to browse options.")
  }
  if (nrow(matches) > 1) {
    choices <- paste0(matches$country.name.en, " (", matches$iso3c, ")")
    pick <- menu(choices, title = paste0("Multiple matches for '", country, "'. Select one:"))
    if (pick == 0) stop("No country selected.")
    matches <- matches[pick, ]
  }

  if (!is.null(data)) {
    if ("PA" %in% names(data)) {
      plot_meta <- data |>
        dplyr::group_by(PlotID) |>
        dplyr::slice(1) |>
        dplyr::ungroup()

      plot_ids  <- plot_meta$PlotID
      size      <- plot_meta$PA
      latitude  <- plot_meta$Latitude
      longitude <- plot_meta$Longitude

      census_counts <- data |>
        dplyr::group_by(PlotID) |>
        dplyr::summarise(
          n_census = dplyr::n_distinct(c(YR, PrevYR)),
          .groups  = "drop"
        )
      min_c  <- min(census_counts$n_census)
      max_c  <- max(census_counts$n_census)
      census <- if (min_c == max_c) as.character(min_c) else paste0(min_c, "-", max_c)
    } else {
      plot_ids  <- data$PlotID
      size      <- data$Size
      latitude  <- data$Latitude
      longitude <- data$Longitude
    }
  }

  if (is.null(census)) {
    stop("`census` must be provided when `data` is NULL or does not contain YR/PrevYR columns.")
  }

  pi  <- paste(pi,  collapse = "; ")
  pie <- paste(pie, collapse = "; ")

  result <- tibble::tibble(
    Country   = matches$country.name.en,
    Site      = site,
    PI        = pi,
    PIe       = pie,
    Dataset   = length(plot_ids),
    Censuses  = census,
    PlotID    = plot_ids,
    Size      = size,
    Latitude  = latitude,
    Longitude = longitude
  )

  if (export_xlsx) {
    cnt      <- tolower(matches$iso3c)
    filename <- paste0("in_", cnt, "_", tolower(site), "_metadata.xlsx")
    filepath <- file.path(getwd(), filename)
    openxlsx::write.xlsx(result, filepath)
    cat("Table exported to", filepath, "!\n")
  }

  return(result)
}
