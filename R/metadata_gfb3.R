#' Create or complete a plot metadata tibble
#'
#' Interactively builds a tibble of plot-level metadata for use in the
#' InterNodes contributor registry and leaflet map visualisation.
#'
#' When \code{input_file} is supplied the function reads the existing
#' \code{.xlsx}, identifies columns that are entirely \code{NA} or empty,
#' re-calculates automatic fields (\code{DatasetSize}), and prompts the user
#' to supply any remaining manual fields that are still incomplete.
#'
#' @param country Character. Official UN country name (use \code{get_country_code()}
#'   to look up the correct name). If \code{NA}, derived from a \code{Country}
#'   or \code{country} column in \code{data} (checked in that order).
#' @param site Character. Site or forest name. If \code{NA}, derived from a
#'   \code{Site} or \code{site} column in \code{data} (checked in that order).
#' @param data A data frame. Accepts either a plot-level data frame with
#'   columns \code{PlotID}, \code{PlotArea}, \code{Latitude}, \code{Longitude}, or
#'   a tree-level (gfb3) data frame with columns \code{PlotID}, \code{PA},
#'   \code{Latitude}, \code{Longitude}, \code{YR}, \code{PrevYR}. In the
#'   latter case \code{plot_ids}, \code{size}, \code{latitude},
#'   \code{longitude}, and \code{census} are all derived from the data.
#' @param input_file Character. Optional path to a partially complete
#'   \code{.xlsx} metadata file previously exported by this function. When
#'   supplied, the file is read and incomplete fields are identified; automatic
#'   fields are re-calculated and manual fields that remain empty prompt the
#'   user interactively. All other arguments are ignored unless the
#'   corresponding column is still missing/empty in the file.
#' @param pi Character. Full name of the principal investigator.
#' @param pie Character. Email address of the principal investigator.
#' @param census Character or numeric. Number of censuses (or range like
#'   \code{"2-3"}). Derived from \code{data} when provided. Required when
#'   \code{data} is \code{NULL}, otherwise derived from \code{YR}/\code{PrevYR}.
#' @param plot_ids Character vector. Vector of PlotIDs. Required when \code{data}
#'   is \code{NULL}, otherwise derived from \code{PlotID}.
#' @param size Numeric. Plot area in hectares. Scalar, applied to all plots when
#'   \code{data} is \code{NULL}, otherwise derived from \code{PA}.
#' @param latitude Numeric. Latitude in decimal degrees. Scalar, applied to all
#'   plots when \code{data} is \code{NULL}, otherwise derived.
#' @param longitude Numeric. Longitude in decimal degrees. Scalar, applied to all
#'   plots when \code{data} is \code{NULL}, otherwise derived.
#' @param export_xlsx Logical. Whether to export the table as .xlsx file.
#' @param site_in_filename Logical. Whether to include the site name in the
#'   exported filename. Defaults to \code{TRUE}. Set to \code{FALSE} when
#'   \code{site} is derived from a column with multiple values and a clean
#'   country-level filename is preferred.
#' @param output_dir Character. Directory where exported files are saved.
#'   Defaults the exports folder in the parent directory.
#' @param filename Character. Optional. If provided, used directly as the
#'   output filename (with \code{.xlsx} appended if missing). Overrides
#'   \code{site_in_filename}.
#' @return A tibble with one row per plot and columns: \code{Country},
#'   \code{Site}, \code{PI}, \code{PIe}, \code{Censuses}, \code{PlotID},
#'   \code{PlotArea}, \code{Latitude}, \code{Longitude}, \code{DatasetSize}.
#'
#' @examples
#' \dontrun{
#' # Build from scratch
#' ituri_xy <- metadata_gfb3(
#'   country   = "Democratic Republic of the Congo",
#'   site      = "Forest1",
#'   pi        = "Jean-Paul Sartre",
#'   pie       = "jeanpsartre@gmail.com",
#'   plot_ids  = c("in_co_edoro_1", "in_co_edoro_2", "in_co_lenda_1"),
#'   size      = 10,
#'   latitude  = 1.4368,
#'   longitude = 28.5826
#' )
#'
#' # Complete a partially filled file
#' ituri_xy <- metadata_gfb3(input_file = "exports/in_cod_metadata.xlsx")
#' }
#'
#' @importFrom tibble tibble
#' @export
metadata_gfb3 <- function(country     = NA,
                          site        = NA,
                          data        = NULL,
                          input_file  = NULL,
                          pi          = NA,
                          pie         = NA,
                          census      = NULL,
                          plot_ids    = NULL,
                          size        = NULL,
                          latitude    = NULL,
                          longitude   = NULL,
                          export_xlsx = TRUE,
                          site_in_filename = FALSE,
                          output_dir    = "../exports",
                          filename = NULL) {

  # ---------------------------------------------------------------------------
  # BRANCH A: complete a partially filled xlsx
  # ---------------------------------------------------------------------------
  if (!is.null(input_file)) {
    if (!file.exists(input_file)) stop("input_file not found: ", input_file)
    partial <- openxlsx::read.xlsx(input_file)

    # Helper: is a column incomplete (all NA or all "")
    .col_empty <- function(x) all(is.na(x) | trimws(as.character(x)) == "")

    # Expected manual fields and their pretty prompt labels
    manual_fields <- c(
      Country  = "Country (official UN name, e.g. 'Brazil')",
      Site     = "Site / forest name",
      PI       = "Principal Investigator full name(s), semicolon-separated",
      PIe      = "PI email address(es), semicolon-separated",
      Censuses = "Number of censuses (integer or range like '2-3')"
    )

    # Identify which manual fields need user input
    incomplete_manual <- names(manual_fields)[
      vapply(names(manual_fields), function(col) {
        !(col %in% names(partial)) || .col_empty(partial[[col]])
      }, logical(1))
    ]

    if (length(incomplete_manual) > 0) {
      cli::cli_alert_warning(
        "The following fields are incomplete and require manual input:"
      )
      for (col in incomplete_manual) {
        cli::cli_text("  {.field {col}}: {manual_fields[col]}")
      }
      cat("\n")
    }

    # Prompt for each incomplete manual field
    for (col in incomplete_manual) {
      val <- readline(prompt = paste0(manual_fields[col], ": "))
      val <- trimws(val)
      if (nchar(val) == 0) {
        cli::cli_alert_warning("{.field {col}} left empty.")
        val <- NA_character_
      }
      partial[[col]] <- val
    }

    # Auto-calculate DatasetSize (always refresh from current nrow)
    partial$DatasetSize <- nrow(partial)

    # Report auto-calculated fields
    auto_fields <- c("DatasetSize")
    previously_missing_auto <- auto_fields[
      !vapply(auto_fields, function(col) col %in% names(partial) && !.col_empty(partial[[col]]),
              logical(1))
    ]
    if (length(previously_missing_auto) > 0 || TRUE) {
      cli::cli_alert_success(
        "Auto-calculated: {.field DatasetSize} = {nrow(partial)} plot{?s}."
      )
    }

    result <- tibble::as_tibble(partial)

    # Export
    if (export_xlsx) {
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      if (!is.null(filename)) {
        if (!grepl("\\.xlsx$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".xlsx")
      } else {
        filename <- basename(input_file)
      }
      filepath <- file.path(output_dir, filename)
      openxlsx::write.xlsx(result, filepath)
      cli::cli_alert_success("Table exported to {.path {normalizePath(filepath, winslash = '/')}}!")
    }

    return(result)
  }

  # ---------------------------------------------------------------------------
  # BRANCH B: build from scratch
  # ---------------------------------------------------------------------------

  # --- Resolve site from data if not provided ---
  if (length(site) == 1 && is.na(site)) {
    if (is.null(data)) stop("`site` is NA and no `data` was provided to derive it from.")
    site_col <- intersect(c("Site", "site"), names(data))
    if (length(site_col) == 0) stop("`site` is NA and no 'Site' or 'site' column found in `data`.")
    site <- data[[site_col[1]]]
  }

  # --- Resolve country from data if not provided ---
  if (length(country) == 1 && is.na(country)) {
    if (is.null(data)) stop("`country` is NA and no `data` was provided to derive it from.")
    cnt_col <- intersect(c("Country", "country"), names(data))
    if (length(cnt_col) == 0) stop("`country` is NA and no 'Country' or 'country' column found in `data`.")
    country <- data[[cnt_col[1]]]
  }

  # --- Process data ---
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

      # Trim site/country to plot level if pulled from full (tree-level) data
      if (length(site)    > 1) site    <- plot_meta[[intersect(c("Site",    "site"),    names(plot_meta))[1]]]
      if (length(country) > 1) country <- plot_meta[[intersect(c("Country", "country"), names(plot_meta))[1]]]

      census_counts <- data |>
        dplyr::group_by(PlotID, TreeID) |>
        dplyr::summarise(
          n_census = dplyr::n_distinct(c(YR[!is.na(YR)], PrevYR[!is.na(PrevYR)])),
          .groups = "drop"
        ) |>
        dplyr::group_by(PlotID) |>
        dplyr::summarise(
          n_census = max(n_census),
          .groups = "drop"
        )
      census <- as.character(census_counts$n_census[match(plot_ids, census_counts$PlotID)])

    } else {
      plot_ids  <- data$PlotID
      size      <- data$PlotArea
      latitude  <- data$Latitude
      longitude <- data$Longitude
    }
  }

  # --- Resolve country code, disambiguate if needed ---
  matches <- get_country_code(country[1])
  if (nrow(matches) == 0) {
    stop("No country found matching '", country[1], "'. Use get_country_code() to browse options.")
  }
  if (nrow(matches) > 1) {
    choices <- paste0(matches$country.name.en, " (", matches$iso3c, ")")
    pick <- menu(choices, title = paste0("Multiple matches for '", country[1], "'. Select one:"))
    if (pick == 0) stop("No country selected.")
    matches <- matches[pick, ]
  }

  if (is.null(census)) {
    stop("`census` must be provided when `data` is NULL or does not contain YR/PrevYR columns.")
  }

  pi  <- paste(pi,  collapse = "; ")
  pie <- paste(pie, collapse = "; ")

  result <- tibble::tibble(
    Country     = dplyr::coalesce(matches$un.name.en, matches$country.name.en),
    Site        = site,
    PI          = pi,
    PIe         = pie,
    Censuses    = as.numeric(census),
    PlotID      = plot_ids,
    PlotArea    = size,
    Latitude    = latitude,
    Longitude   = longitude,
    DatasetSize = length(plot_ids)   # number of plots in this metadata table
  )

  if (export_xlsx) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    if (!is.null(filename)) {
      if (!grepl("\\.xlsx$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".xlsx")
    } else {
      cnt      <- tolower(matches$iso3c)
      filename <- if (site_in_filename) {
        paste0("in_", cnt, "_", tolower(site[1]), "_metadata.xlsx")
      } else {
        paste0("in_", cnt, "_metadata.xlsx")
      }
    }
    filepath <- file.path(output_dir, filename)
    openxlsx::write.xlsx(result, filepath)
    cat("Table exported to", normalizePath(filepath, winslash = "/"), "!\n")
  }

  return(result)
}
