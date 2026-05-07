#' GFB3 Format Diagnostic Report
#'
#' Runs a suite of diagnostic checks on a GFB3-formatted dataset and prints a
#' structured report to the console. Optionally exports a narrative PDF report
#' and/or an Excel workbook with diagnostic tables.
#'
#' @param data A tibble in GFB3 format, as produced by \code{convert_paracou_gfb3()}
#'   or equivalent. Must contain columns: \code{PlotID}, \code{PA},
#'   \code{Latitude}, \code{Longitude}, \code{TreeID}, \code{Species},
#'   \code{Status}, \code{DBH}, \code{YR}, \code{PrevDBH}, \code{PrevYR}.
#'   DBH is assumed to be in centimetres.
#' @param curation_log Character string (or \code{NULL}). A free-text narrative
#'   describing the curation and data-wrangling decisions applied to the dataset
#'   before formatting (e.g., outlier removal, coordinate corrections, species
#'   name harmonisation). When provided, the notes are printed in the console
#'   report under a dedicated "Curation notes" section and, if exports are
#'   requested, written as a plain-text file (\code{curation_log.txt}) inside
#'   the temporary staging directory so that the Python export scripts can embed
#'   them in the PDF and/or Excel workbook. Default \code{NULL} (no notes).
#' @param plot_metadata A tibble as returned by \code{make_plot_metadata()}, or
#'   \code{NULL} (default). When provided, plot-level metadata (Country, Site,
#'   PI, PIe, Censuses, PlotID, Size, Latitude, Longitude) are included in the
#'   console report and, if exports are requested, a "Plot Locations" section
#'   with a summary table and a static map is added to the PDF / Excel workbook.
#'   Only the first occurrence of each PlotID is used (i.e. one row per plot).
#' @param export_pdf Logical. If \code{TRUE}, a narrative PDF report is written
#'   to \code{output_dir}. Requires Python with \code{reportlab} installed.
#'   Default \code{FALSE}.
#' @param export_xlsx Logical. If \code{TRUE}, an Excel workbook with diagnostic
#'   tables is written to \code{output_dir}. Requires Python with
#'   \code{openpyxl} installed. Default \code{FALSE}.
#' @param export_map Logical. If \code{TRUE} and \code{plot_metadata} is
#'   supplied, a self-contained interactive HTML map
#'   (\code{<dataset_name>_plot_map.html}) is saved to \code{output_dir}.
#'   The HTML is fully interactive (pan, zoom) and self-contained in a single
#'   file. The PDF / Excel report always uses a static PNG of the same map
#'   regardless of this setting. Requires \pkg{htmlwidgets}.
#'   Default \code{FALSE}.
#' @param output_dir Character. Directory where exported files are saved.
#'   Defaults to the current working directory.
#' @param dataset_name Character. Label used in export filenames and report
#'   headers. Defaults to \code{"gfb3_dataset"}.
#'
#' @return Invisibly returns a named list with elements \code{status},
#'   \code{ba}, \code{dbh}, \code{growth}, \code{flags}, \code{curation_log},
#'   and \code{plot_metadata}.
#'
#' @section Basal area diagnostics:
#' Stand basal area (BA) is computed per plot x census combination as the sum
#' of cross-sectional areas of all live trees (\code{Status == "0"}) divided by
#' plot area (\code{PA}, in hectares). DBH is assumed to be in centimetres:
#' \deqn{BA = \sum \pi (DBH / 200)^2 \; / \; PA}
#' Plot x census combinations with \eqn{50 \le BA < 100} m\eqn{^2}/ha are
#' flagged as warnings ("higher than typical"); those with \eqn{BA \ge 100}
#' m\eqn{^2}/ha are flagged as critical failures ("implausible BA").
#'
#' @section Map export dependencies:
#' When \code{plot_metadata} is supplied and either \code{export_pdf} or
#' \code{export_xlsx} is \code{TRUE}, \code{report_gfb3()} will attempt to
#' render a Leaflet map screenshot using \pkg{leaflet}, \pkg{mapview}, and
#' \pkg{webshot2} (which requires a headless Chromium browser). These packages
#' are not installed with \pkg{facai} by default. Run
#' \code{\link{facai_setup_map}()} once after installing the package to set
#' everything up:
#'
#' \preformatted{
#' facai_setup_map()
#' }
#'
#' If these packages are not available, the map will fall back to a static
#' \pkg{ggplot2} / \pkg{rnaturalearth} plot, and then to a base-R plot if
#' those are also absent. The rest of the report is unaffected in all cases.
#'
#' @examples
#' \dontrun{
#' my_notes <- "
#' - Removed 14 stems with DBH < 1 cm (measurement artefacts confirmed by field team).
#' - Corrected plot coordinates for PA 07 (GPS drift in 2018 census; adjusted to match
#'   2014 reference positions).
#' - Harmonised species names against the TNRS database; 32 morphospecies retained as-is.
#' - Status recoded from local 3-class scheme (V/M/R) to GFB3 (0/1/2).
#' "
#'
#' meta <- make_plot_metadata(data = my_gfb3, pi = "Jane Smith",
#'                            pie = "j.smith@uni.edu")
#'
#' result <- report_gfb3(paracou_gfb3,
#'                       curation_log  = my_notes,
#'                       plot_metadata = meta,
#'                       export_pdf    = TRUE,
#'                       export_xlsx   = TRUE,
#'                       export_map    = TRUE,
#'                       output_dir    = "reports/",
#'                       dataset_name  = "paracou_01")
#' }
#'
#' @importFrom dplyr count mutate select filter summarise pull arrange group_by
#'   ungroup n_distinct distinct left_join cumany first
#' @importFrom cli cli_h1 cli_h2 cli_bullets cli_alert_warning cli_alert_success
#'   cli_alert_danger cli_verbatim
#' @export
report_gfb3 <- function(data,
                        curation_log  = NULL,
                        plot_metadata = NULL,
                        export_pdf    = FALSE,
                        export_xlsx   = FALSE,
                        export_map    = FALSE,
                        output_dir    = ".",
                        dataset_name  = "data") {

  dat <- data

  # ── 1. Basic counts ──────────────────────────────────────────────────────────
  n_rows   <- nrow(dat)
  n_trees  <- n_distinct(dat$TreeID)
  n_plots  <- n_distinct(dat$PlotID)
  yr_range <- range(c(dat$YR, dat$PrevYR), na.rm = TRUE)

  # ── 2. Status distribution ───────────────────────────────────────────────────
  status_labels <- c(
    "0" = "alive",
    "1" = "dead between inventories",
    "2" = "new recruit",
    "9" = "missing"
  )

  dat <- dat |>
    mutate(Status = as.character(Status))

  status_tbl <- dat |>
    count(Status) |>
    mutate(
      pct   = round(100 * n / sum(n), 1),
      Label = status_labels[Status]
    ) |>
    select(Status, Label, n, pct)

  # ── 3. Missing data ──────────────────────────────────────────────────────────
  n_missing_dbh     <- sum(is.na(dat$DBH))
  n_missing_prevdbh <- sum(is.na(dat$PrevDBH))
  n_missing_species <- sum(
    is.na(dat$Species) | dat$Species == "NA NA" | dat$Species == " "
  )

  # ── 4. DBH summary ───────────────────────────────────────────────────────────
  dbh_hist_path <- tempfile("dbh_hist_", fileext = ".png")
  png(dbh_hist_path, width = 600, height = 400, res = 96)
  invisible(capture.output(
    dbh_sum_obj <- suppressMessages(dbh_summary(dat))
  ))
  dev.off()
  dbh_sum <- capture.output(print(dbh_sum_obj))

  # ── 5. Growth summary ────────────────────────────────────────────────────────
  hist_path <- tempfile("growth_hist_", fileext = ".png")
  png(hist_path, width = 600, height = 400, res = 96)
  invisible(capture.output(
    growth_sum_obj <- suppressMessages(growth_summary(dat))
  ))
  dev.off()
  growth_sum <- capture.output(print(growth_sum_obj))

  # ── 6. Negative / zero growth ────────────────────────────────────────────────
  n_neg_growth <- dat |>
    filter(!is.na(DBH), !is.na(PrevDBH)) |>
    summarise(n = sum((DBH - PrevDBH) < 0)) |>
    pull(n)

  n_zero_growth <- dat |>
    filter(!is.na(DBH), !is.na(PrevDBH)) |>
    summarise(n = sum((DBH - PrevDBH) == 0)) |>
    pull(n)

  # ── 7. Suspiciously fast growth (>5 cm/yr) ───────────────────────────────────
  n_fast <- dat |>
    filter(!is.na(DBH), !is.na(PrevDBH), !is.na(PrevYR), YR != PrevYR) |>
    mutate(ann_growth = (DBH - PrevDBH) / (YR - PrevYR)) |>
    summarise(n = sum(ann_growth > 5)) |>
    pull(n)

  # ── 8. DBH < 10 cm ───────────────────────────────────────────────────────────
  n_small <- sum(dat$DBH < 10, na.rm = TRUE)

  # ── 9. Basal area per plot x census ──────────────────────────────────────────
  # DBH in cm: radius in m = DBH / (2 * 100) = DBH / 200
  # BA (m2/ha) = sum(pi * (DBH / 200)^2) / PA
  ba_tbl <- dat |>
    filter(Status == "0", !is.na(DBH), !is.na(PA), PA > 0) |>
    group_by(PlotID, YR) |>
    summarise(
      BA = sum(pi * (DBH / 200)^2, na.rm = TRUE) / first(PA),
      .groups = "drop"
    ) |>
    mutate(
      BA      = round(BA, 3),
      BA_flag = case_when(
        BA >= 100 ~ "critical",
        BA >= 50  ~ "warning",
        TRUE      ~ "ok"
      )
    )

  n_ba_warning  <- sum(ba_tbl$BA_flag == "warning")
  n_ba_critical <- sum(ba_tbl$BA_flag == "critical")

  # ── 10. Duplicate TreeID x YR ────────────────────────────────────────────────
  n_dups <- dat |>
    count(PlotID, TreeID, YR, Status) |>
    filter(n > 1) |>
    nrow()

  # ── 11. Dead trees with subsequent records ───────────────────────────────────
  n_zombie <- dat |>
    arrange(PlotID, TreeID, YR) |>
    group_by(PlotID, TreeID) |>
    mutate(ever_dead = cumany(Status == "1")) |>
    filter(ever_dead, Status %in% c("0", "2")) |>
    nrow()

  # ── Flags table (for export) ─────────────────────────────────────────────────
  flags_tbl <- tibble::tibble(
    Flag = c(
      "Missing DBH",
      "Missing PrevDBH",
      "Missing/malformed Species",
      "DBH < 10 cm",
      "Negative DBH growth",
      "Zero DBH growth",
      "Annual growth > 5 cm/yr",
      "Duplicate TreeID x YR",
      "Zombie trees (alive after death)",
      "BA 60-100 m2/ha (higher than typical)",
      "BA >= 100 m2/ha (implausible)"
    ),
    Count = c(
      n_missing_dbh, n_missing_prevdbh, n_missing_species,
      n_small, n_neg_growth, n_zero_growth, n_fast,
      n_dups, n_zombie,
      n_ba_warning, n_ba_critical
    ),
    Severity = c(
      "warning", "info", "warning",
      "warning", "warning", "info", "warning",
      "critical", "critical",
      "warning", "critical"
    )
  )

  hard_fails <- c(n_dups, n_ba_critical)
  soft_flags <- c(n_missing_dbh, n_small, n_neg_growth, n_fast, n_zombie, n_ba_warning)

  verdict <- dplyr::case_when(
    any(hard_fails > 0) ~ "FAIL: critical issues must be resolved before use.",
    any(soft_flags > 0) ~ "WARN: passed critical checks but has warnings worth reviewing.",
    TRUE                ~ "PASS: dataset looks clean."
  )

  # ── 12. Plot metadata (one row per PlotID) ───────────────────────────────────
  plot_meta_tbl <- NULL
  if (!is.null(plot_metadata)) {
    expected_cols <- c("Country", "Site", "PI", "PIe",
                       "Censuses", "PlotID", "Size", "Latitude", "Longitude")
    missing_cols <- setdiff(expected_cols, names(plot_metadata))
    if (length(missing_cols) > 0) {
      cli::cli_alert_warning(
        "plot_metadata is missing columns: {paste(missing_cols, collapse = ', ')}. \\
        Ignoring plot_metadata."
      )
    } else {
      plot_meta_tbl <- plot_metadata |>
        distinct(PlotID, .keep_all = TRUE) |>
        select(all_of(expected_cols))
    }
  }

  # ── CONSOLE REPORT ───────────────────────────────────────────────────────────
  cli::cli_h1("GFB3 Format Diagnostic Report")

  cli::cli_h2("Overview")

  overview_bullets <- c(
    "*" = "Dataset: {dataset_name}",
    "*" = "Rows: {n_rows}",
    "*" = "Trees: {n_trees}",
    "*" = "Plots: {n_plots}",
    "*" = "Year range: {yr_range[1]} - {yr_range[2]}"
  )

  if (!is.null(plot_meta_tbl)) {
    country  <- paste(unique(plot_meta_tbl$Country), collapse = ", ")
    sites    <- paste(unique(plot_meta_tbl$Site),    collapse = ", ")
    pi_name  <- paste(unique(plot_meta_tbl$PI),      collapse = ", ")
    pi_email <- paste(unique(plot_meta_tbl$PIe),     collapse = ", ")

    overview_bullets <- c(
      overview_bullets,
      "*" = "Country: {country}",
      "*" = "Site(s): {sites}",
      "*" = "PI: {pi_name}",
      "*" = "PI email: {pi_email}"
    )
  }

  cli::cli_bullets(overview_bullets)

  # ── Curation notes (printed only when provided) ──────────────────────────────
  if (!is.null(curation_log) && nzchar(trimws(curation_log))) {
    cli::cli_h2("Curation notes")
    cli::cli_verbatim(curation_log)
  }

  # ── Plot metadata (printed only when provided) ───────────────────────────────
  if (!is.null(plot_meta_tbl)) {
    cli::cli_h2("Plot Locations")
    print(plot_meta_tbl)
  }

  cli::cli_h2("Status Distribution")
  print(status_tbl)

  cli::cli_h2("DBH Summary")
  cat(dbh_sum, sep = "\n")

  cli::cli_h2("Growth Summary")
  cat(growth_sum, sep = "\n")

  cli::cli_h2("Basal Area by Plot x Census")
  ba_flagged <- ba_tbl |> filter(BA_flag != "ok")
  if (nrow(ba_flagged) == 0) {
    cli::cli_alert_success("All plot x census BA values are within the expected range (< 50 m2/ha).")
  } else {
    print(ba_flagged)
  }

  cli::cli_h2("Data Quality Flags")

  flag <- function(n, msg, warn_thresh = 0) {
    if (n > warn_thresh) cli::cli_alert_warning("{n} {msg}")
    else cli::cli_alert_success("0 {msg}")
  }

  flag(n_missing_dbh,     "rows with missing DBH")
  flag(n_missing_prevdbh, "rows with missing PrevDBH (expected for recruits/first intervals)")
  flag(n_missing_species, "rows with missing or malformed Species")
  flag(n_small,           "rows with DBH < 10 cm (below GFB3 threshold)")
  flag(n_neg_growth,      "rows with negative DBH growth")
  flag(n_zero_growth,     "rows with zero DBH growth")
  flag(n_fast,            "rows with annual growth > 5 cm/yr (possible errors)")
  flag(n_dups,            "duplicate TreeID x YR combinations")
  flag(n_zombie,          "alive/recruit records following a death (zombie trees)")
  flag(n_ba_warning,      "plot x census combinations with BA 60-100 m2/ha (higher than typical)")
  flag(n_ba_critical,     "plot x census combinations with BA >= 100 m2/ha (implausible)")

  cli::cli_h2("Verdict")
  if (any(hard_fails > 0)) {
    cli::cli_alert_danger(verdict)
  } else if (any(soft_flags > 0)) {
    cli::cli_alert_warning(verdict)
  } else {
    cli::cli_alert_success(verdict)
  }

  # ── EXPORTS ──────────────────────────────────────────────────────────────────
  if (export_pdf || export_xlsx || (export_map && !is.null(plot_meta_tbl))) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    tmp_dir <- tempfile("gfb3_report_")
    dir.create(tmp_dir)

    on.exit({
      unlink(tmp_dir, recursive = TRUE)
      unlink(hist_path)
      unlink(dbh_hist_path)
    }, add = TRUE)

    readr::write_csv(status_tbl,  file.path(tmp_dir, "status.csv"))
    readr::write_csv(flags_tbl,   file.path(tmp_dir, "flags.csv"))
    readr::write_csv(ba_tbl,      file.path(tmp_dir, "ba.csv"))
    readr::write_csv(dbh_sum_obj, file.path(tmp_dir, "dbh.csv"))
    readr::write_csv(as.data.frame(growth_sum_obj), file.path(tmp_dir, "growth.csv"))
    file.copy(hist_path,     file.path(tmp_dir, "growth_hist.png"))
    file.copy(dbh_hist_path, file.path(tmp_dir, "dbh_hist.png"))

    meta <- data.frame(
      key   = c("dataset_name", "n_rows", "n_trees", "n_plots",
                "yr_min", "yr_max", "verdict"),
      value = c(dataset_name, n_rows, n_trees, n_plots,
                yr_range[1], yr_range[2], verdict)
    )
    readr::write_csv(meta, file.path(tmp_dir, "meta.csv"))

    # Write curation notes (empty string when NULL so Python scripts can always
    # read the file without extra existence checks)
    notes_text <- if (!is.null(curation_log)) curation_log else ""
    writeLines(notes_text, file.path(tmp_dir, "curation_log.txt"))

    # ── Plot metadata export ──────────────────────────────────────────────────
    if (!is.null(plot_meta_tbl)) {
      plot_meta_export <- plot_meta_tbl |>
        dplyr::mutate(
          Latitude  = round(Latitude,  2),
          Longitude = round(Longitude, 2)
        )
      readr::write_csv(plot_meta_export, file.path(tmp_dir, "plot_metadata.csv"))

      map_path     <- file.path(tmp_dir, "plot_map.png")
      leaflet_map  <- .render_plot_map(plot_meta_tbl, map_path)

      if (export_map) {
        if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
          cli::cli_alert_warning(
            "export_map requires the {.pkg htmlwidgets} package. \\
            Install it with {.code install.packages('htmlwidgets')}."
          )
        } else if (is.null(leaflet_map)) {
          cli::cli_alert_warning(
            "Interactive map could not be saved: Leaflet map object unavailable \\
            (leaflet / mapview / webshot2 not installed). \\
            Run {.fn facai_setup_map} to enable this feature."
          )
        } else {
          html_path <- file.path(output_dir,
                                 paste0(dataset_name, "_plot_map.html"))
          htmlwidgets::saveWidget(leaflet_map, file = html_path, selfcontained = TRUE)
          cli::cli_alert_success("Interactive map written to: {html_path}")
        }
      }
    }

    # Resolve Python executable
    python <- Sys.which("python")
    if (python == "") python <- Sys.which("python3")
    if (python == "") cli::cli_abort("No Python executable found on PATH.")
  }

  if (export_pdf) {
    pdf_path <- file.path(output_dir, paste0(dataset_name, "_gfb3_report.pdf"))
    py_pdf   <- system.file("python", "gfb3_report_pdf.py", package = "facai")

    if (py_pdf == "") {
      cli::cli_alert_danger(
        "PDF export failed: gfb3_report_pdf.py not found. \\
        Reinstall facai after placing the script in inst/python/."
      )
    } else {
      ret <- system2(python,
                     args   = c(shQuote(py_pdf),
                                shQuote(tmp_dir),
                                shQuote(pdf_path)),
                     stdout = TRUE, stderr = TRUE)

      if (!file.exists(pdf_path)) {
        cli::cli_alert_danger(
          "PDF export failed:\n{paste(ret, collapse = '\n')}"
        )
      } else {
        cli::cli_alert_success("PDF report written to: {pdf_path}")
      }
    }
  }

  if (export_xlsx) {
    xlsx_path <- file.path(output_dir, paste0(dataset_name, "_gfb3_report.xlsx"))
    py_xlsx   <- system.file("python", "gfb3_report_xlsx.py", package = "facai")

    if (py_xlsx == "") {
      cli::cli_alert_danger(
        "Excel export failed: gfb3_report_xlsx.py not found. \\
        Reinstall facai after placing the script in inst/python/."
      )
    } else {
      ret <- system2(python,
                     args   = c(shQuote(py_xlsx),
                                shQuote(tmp_dir),
                                shQuote(xlsx_path)),
                     stdout = TRUE, stderr = TRUE)

      if (!file.exists(xlsx_path)) {
        cli::cli_alert_danger(
          "Excel export failed:\n{paste(ret, collapse = '\n')}"
        )
      } else {
        cli::cli_alert_success("Excel workbook written to: {xlsx_path}")
      }
    }
  }

  # ── RETURN ───────────────────────────────────────────────────────────────────
  invisible(list(
    status        = status_tbl,
    ba            = ba_tbl,
    dbh           = dbh_sum_obj,
    growth        = growth_sum_obj,
    curation_log  = curation_log,
    plot_metadata = plot_meta_tbl,
    flags = list(
      missing_dbh     = n_missing_dbh,
      missing_prevdbh = n_missing_prevdbh,
      missing_species = n_missing_species,
      small_dbh       = n_small,
      neg_growth      = n_neg_growth,
      zero_growth     = n_zero_growth,
      fast_growth     = n_fast,
      duplicates      = n_dups,
      zombies         = n_zombie,
      ba_warning      = n_ba_warning,
      ba_critical     = n_ba_critical
    )
  ))
}


# ── Internal helper: render static plot-location map ─────────────────────────
#
# Renders a Leaflet map and saves it as a PNG screenshot using mapview::mapshot2
# (which drives a headless Chromium via webshot2). No labels on the map —
# plot locations are shown as plain circle markers only.
#
# Tile provider: Esri.WorldTopoMap (no API key; reliable; looks great).
#
# Falls back to an rnaturalearth ggplot2 map if mapview / webshot2 are not
# installed, and to base-R as a last resort.
#
# @param tbl  One-row-per-plot tibble with Latitude, Longitude columns.
# @param path Output PNG path (must end in .png).
# @noRd
.render_plot_map <- function(tbl, path) {

  has_leaflet  <- requireNamespace("leaflet",  quietly = TRUE)
  has_mapview  <- requireNamespace("mapview",  quietly = TRUE)
  has_webshot2 <- requireNamespace("webshot2", quietly = TRUE)

  # ── Primary: Leaflet screenshot via mapshot2 ──────────────────────────────
  if (!has_leaflet || !has_mapview || !has_webshot2) {
    missing_pkgs <- c(
      if (!has_leaflet)  "leaflet",
      if (!has_mapview)  "mapview",
      if (!has_webshot2) "webshot2"
    )
    cli::cli_alert_info(
      "Map export: {paste(missing_pkgs, collapse = ', ')} not found. \\
      Run {.fn facai_setup_map} for a better Leaflet map; falling back to ggplot2."
    )
  }

  if (has_leaflet && has_mapview && has_webshot2) {

    m <- leaflet::leaflet(data = tbl) |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery) |>
      leaflet::addTiles(
        urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}",
        options     = leaflet::tileOptions(opacity = 0.7)
      ) |>
      leaflet::addCircleMarkers(
        lng         = ~Longitude,
        lat         = ~Latitude,
        radius      = 7,
        color       = "#FFFFFF",
        fillColor   = "#E84855",
        fillOpacity = 0.9,
        weight      = 1.5,
        opacity     = 1
      ) |>
      leaflet::fitBounds(
        lng1    = min(tbl$Longitude),
        lat1    = min(tbl$Latitude),
        lng2    = max(tbl$Longitude),
        lat2    = max(tbl$Latitude),
        options = list(maxZoom = 13)
      )

    mapview::mapshot2(
      x       = m,
      file    = path,
      vwidth  = 900,
      vheight = 600,
      zoom    = 1
    )

    if (file.exists(path)) return(invisible(m))
    cli::cli_alert_warning(
      "mapshot2 did not produce a file; falling back to ggplot2 map."
    )
  }

  # ── Fallback 1: rnaturalearth + ggplot2 (no tile, no labels) ────────────────
  has_gg  <- requireNamespace("ggplot2",       quietly = TRUE)
  has_rne <- requireNamespace("rnaturalearth", quietly = TRUE)
  has_sf  <- requireNamespace("sf",            quietly = TRUE)

  lat_range <- range(tbl$Latitude,  na.rm = TRUE)
  lon_range <- range(tbl$Longitude, na.rm = TRUE)
  pad       <- max(2, diff(lat_range) * 0.3, diff(lon_range) * 0.3)
  xlim      <- c(lon_range[1] - pad, lon_range[2] + pad)
  ylim      <- c(lat_range[1] - pad, lat_range[2] + pad)

  if (has_gg && has_rne && has_sf) {
    world  <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    pts_sf <- sf::st_as_sf(tbl,
                           coords = c("Longitude", "Latitude"),
                           crs    = 4326)
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data      = world,
                       fill      = "#EAE7DC",
                       color     = "#AAAAAA",
                       linewidth = 0.25) +
      ggplot2::geom_sf(data        = pts_sf,
                       color       = "#2E4057",
                       fill        = "#E84855",
                       shape       = 21,
                       size        = 3,
                       stroke      = 0.8) +
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      ggplot2::labs(x = "Longitude", y = "Latitude") +
      ggplot2::theme_minimal(base_size = 9) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#C8E6F5", color = NA),
        panel.grid       = ggplot2::element_line(color = "#CCCCCC66",
                                                 linewidth = 0.2)
      )
    png(path, width = 700, height = 480, res = 96)
    on.exit(dev.off(), add = TRUE)
    print(p)
    return(invisible(NULL))
  }

  # ── Fallback 2: base-R ────────────────────────────────────────────────────────
  png(path, width = 700, height = 480, res = 96)
  on.exit(dev.off(), add = TRUE)
  plot(tbl$Longitude, tbl$Latitude,
       xlim = xlim, ylim = ylim,
       pch  = 21, bg = "#E84855", col = "#2E4057", cex = 1.4,
       xlab = "Longitude", ylab = "Latitude",
       main = "Plot Locations")
  if (requireNamespace("maps", quietly = TRUE))
    maps::map("world", add = TRUE, col = "#AAAAAA")

  invisible(NULL)
}
