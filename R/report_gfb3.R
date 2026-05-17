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
#' @param unidentified_label Character. Species label used to mark unidentified
#'   trees. Rows matching this value are excluded from the malformed-species
#'   flag and summarised separately. Default \code{"Unidentified sp."}.
#' @param curation_log Character string (or \code{NULL}). A free-text narrative
#'   describing the curation and data-wrangling decisions applied to the dataset
#'   before formatting (e.g., outlier removal, coordinate corrections, species
#'   name harmonisation). When provided, the notes are printed in the console
#'   report under a dedicated "Curation notes" section and, if exports are
#'   requested, written as a plain-text file (\code{curation_log.txt}) inside
#'   the temporary staging directory so that the Python export scripts can embed
#'   them in the PDF and/or Excel workbook. Default \code{NULL} (no notes).
#' @param metadata A tibble as returned by \code{make_plot_metadata()}, or
#'   \code{NULL} (default). When provided, plot-level metadata (Country, Site,
#'   PI, PIe, Censuses, PlotID, Size, Latitude, Longitude) are included in the
#'   console report and, if exports are requested, a "Plot Locations" section
#'   with a summary table and a static map is added to the PDF / Excel workbook.
#'   Only the first occurrence of each PlotID is used (i.e. one row per plot).
#' @param export Character vector controlling which outputs are exported.
#'   Any combination of \code{"pdf"}, \code{"xlsx"}, and \code{"map"}.
#'   Defaults to all three. Pass \code{character(0)} or \code{NULL} to skip
#'   all exports and run diagnostics only.
#' @param output_dir Character. Directory where exported files are saved.
#'   Defaults the exports folder in the parent directory.
#' @param dataset_name Character. Label used in export filenames and report
#'   headers. Defaults to \code{"gfb3_dataset"}.
#'
#' @return Invisibly returns a named list with elements \code{status},
#'   \code{ba}, \code{tph}, \code{dbh}, \code{growth}, \code{flags},
#'   \code{na_pa}, \code{na_species}, \code{curation_log}, and \code{metadata}.
#'
#' @importFrom dplyr count mutate select filter summarise pull arrange group_by
#'   ungroup n_distinct distinct left_join cumany first lag
#' @importFrom cli cli_h1 cli_h2 cli_bullets cli_alert_warning cli_alert_success
#'   cli_alert_danger cli_verbatim
#' @export
report_gfb3 <- function(data,
                        unidentified_label = "Unidentified sp.",
                        curation_log  = NULL,
                        metadata      = NULL,
                        export        = c("pdf", "xlsx", "map"),
                        output_dir    = "../exports",
                        dataset_name  = NULL) {

  export_pdf  <- "pdf"  %in% export
  export_xlsx <- "xlsx" %in% export
  export_map  <- "map"  %in% export

  if (is.null(dataset_name)) {
    if (!is.null(metadata) && all(c("Country", "Site") %in% names(metadata))) {
      country_raw <- unique(metadata$Country)[1]
      iso3 <- tryCatch({
        result <- get_country_code(country_raw)
        if (nrow(result) > 0) tolower(result$iso3c[1]) else tolower(country_raw)
      }, error = function(e) tolower(country_raw))
      site <- unique(metadata$Site)[1] |>
        tolower() |>
        gsub("[^a-z0-9]+", "_", x = _) |>
        gsub("^_|_$", "", x = _)
      dataset_name <- paste0("in_", iso3, "_", site)
    } else {
      dataset_name <- ""
    }
  }

  # ── 1. Basic counts ──────────────────────────────────────────────────────────
  n_rows   <- nrow(data)
  n_trees  <- n_distinct(data$TreeID)
  n_plots  <- n_distinct(data$PlotID)
  yr_range <- range(c(data$YR, data$PrevYR), na.rm = TRUE)

  # ── 2. Status distribution ───────────────────────────────────────────────────
  status_labels <- c(
    "0" = "alive",
    "1" = "dead between inventories",
    "2" = "new recruit",
    "9" = "missing"
  )

  data <- data |>
    mutate(across(where(is.character), trimws)) |>
    mutate(Status = as.character(Status))

  status_tbl <- data |>
    count(Status) |>
    mutate(
      pct   = round(100 * n / sum(n), 1),
      Label = status_labels[Status]
    ) |>
    select(Status, Label, n, pct)

  # ── 3. Missing data (scalar counts) ─────────────────────────────────────────
  n_missing_yr      <- sum(is.na(data$YR))
  n_missing_dbh     <- sum(is.na(data$DBH))
  n_missing_prevdbh <- sum(is.na(data$PrevDBH))

  n_missing_species <- data |>
    filter(is.na(Species) |
             (!grepl("^[A-Z][a-z]+ [a-z]", Species) &
                Species != unidentified_label)) |>
    distinct(TreeID) |>
    nrow()

  # ── 3b. NA summaries by PlotID ───────────────────────────────────────────────
  na_pa_tbl <- data |>
    group_by(PlotID) |>
    summarise(n_missing_PA = sum(is.na(PA)), .groups = "drop") |>
    filter(n_missing_PA > 0) |>
    arrange(desc(n_missing_PA))

  na_species_tbl <- data |>
    filter(is.na(Species) |
             (!grepl("^[A-Z][a-z]+ [a-z]", Species) &
                Species != unidentified_label)) |>
    group_by(PlotID) |>
    summarise(n_missing_Species = n_distinct(TreeID), .groups = "drop") |>
    filter(n_missing_Species > 0) |>
    arrange(desc(n_missing_Species))

  unid_tbl <- data |>
    filter(!is.na(unidentified_label) &
             nzchar(unidentified_label) &
             Species == unidentified_label) |>
    group_by(PlotID) |>
    summarise(n_unidentified = n_distinct(TreeID), .groups = "drop") |>
    arrange(desc(n_unidentified))

  # ── 3c. Prev consistency check (lagged-based) ────────────────────────────────
  # For each row, compute the lagged DBH and YR within PlotID x TreeID.
  # Flag rows where:
  #   (a) PrevDBH is populated but doesn't match lag(DBH)
  #   (b) PrevYR  is populated but doesn't match lag(YR)
  #   (c) PrevDBH is NA but a valid lag(DBH) exists and current DBH is valid
  #       (genuine orphan — missing linkage to previous census)
  #   (d) PrevYR  is NA but a valid lag(YR)  exists and current DBH is valid
  #
  # The Status == "0" guard on (c)/(d) avoids false positives on dead/missing
  # rows where NA PrevDBH may be structurally expected.

  prev_check <- data |>
    arrange(PlotID, TreeID, YR) |>
    group_by(PlotID, TreeID) |>
    mutate(
      lag_DBH = lag(DBH),
      lag_YR  = lag(YR)
    ) |>
    ungroup()

  n_prevdbh_mismatch <- nrow(filter(prev_check, Status == "0",
                                    !is.na(PrevDBH) & !is.na(lag_DBH) & PrevDBH != lag_DBH))
  n_prevyr_mismatch <- nrow(filter(prev_check, Status == "0",
                                   !is.na(PrevYR) & !is.na(lag_YR) & PrevYR != lag_YR))

  n_prevdbh_orphan <- prev_check |>
    filter(Status == "0", is.na(PrevDBH) & !is.na(lag_DBH) & !is.na(DBH)) |>
    nrow()

  n_prevyr_orphan <- prev_check |>
    filter(Status == "0", is.na(PrevYR) & !is.na(lag_YR) & !is.na(DBH)) |>
    nrow()

  # Combined count of any Prev inconsistency (union — rows flagged by any condition)
  n_inconsistent_prev <- nrow(filter(prev_check,
                                     Status == "0" & (
                                       (!is.na(PrevDBH) & !is.na(lag_DBH) & PrevDBH != lag_DBH) |
                                         (!is.na(PrevYR)  & !is.na(lag_YR)  & PrevYR  != lag_YR)  |
                                         (is.na(PrevDBH)  & !is.na(lag_DBH) & !is.na(DBH))        |
                                         (is.na(PrevYR)   & !is.na(lag_YR)  & !is.na(DBH))
                                     )
  ))

  # ── 4. DBH summary ───────────────────────────────────────────────────────────
  dbh_hist_path <- tempfile("dbh_hist_", fileext = ".png")
  png(dbh_hist_path, width = 600, height = 400, res = 96)
  invisible(capture.output(
    dbh_sum_obj <- suppressMessages(dbh_summary(data))
  ))
  dev.off()
  dbh_sum <- capture.output(print(dbh_sum_obj))

  # ── 5. Growth summary ────────────────────────────────────────────────────────
  hist_path <- tempfile("growth_hist_", fileext = ".png")
  png(hist_path, width = 600, height = 400, res = 96)
  invisible(capture.output(
    growth_sum_obj <- suppressMessages(growth_summary(data))
  ))
  dev.off()
  growth_sum <- capture.output(print(growth_sum_obj))

  # ── 6. Negative / zero growth ────────────────────────────────────────────────
  n_neg_growth <- data |>
    filter(!is.na(DBH), !is.na(PrevDBH)) |>
    summarise(n = sum((DBH - PrevDBH) < 0)) |>
    pull(n)

  n_zero_growth <- data |>
    filter(!is.na(DBH), !is.na(PrevDBH)) |>
    summarise(n = sum((DBH - PrevDBH) == 0)) |>
    pull(n)

  # ── 7. Suspiciously fast growth (>5 cm/yr) ───────────────────────────────────
  n_fast <- data |>
    filter(!is.na(DBH), !is.na(PrevDBH), !is.na(PrevYR), YR != PrevYR) |>
    mutate(ann_growth = (DBH - PrevDBH) / (YR - PrevYR)) |>
    summarise(n = sum(ann_growth > 5)) |>
    pull(n)

  # ── 8. DBH < 10 cm ───────────────────────────────────────────────────────────
  n_small <- sum(data$DBH < 10, na.rm = TRUE)

  # ── 9. Basal area per plot x census ──────────────────────────────────────────
  # Group by Census if available (handles multi-year censuses), otherwise by YR.
  # When only one census group is present and PrevDBH/PrevYR are populated,
  # reconstruct the previous census from Prev columns and prepend it.
  has_census   <- "Census" %in% names(data)
  has_prev     <- all(c("PrevDBH", "PrevYR") %in% names(data))
  n_census_grps <- if (has_census) {
    dplyr::n_distinct(data$Census, na.rm = TRUE)
  } else {
    dplyr::n_distinct(floor(data$YR), na.rm = TRUE)
  }

  # Reconstruct previous census rows from Prev columns when only one census
  # group is present in the data (i.e. the first census was folded into PrevDBH/PrevYR)
  data_for_ba <- if (has_prev && n_census_grps == 1L) {
    prev_census_rows <- data |>
      filter(!is.na(PrevDBH), !is.na(PrevYR), !is.na(PA)) |>
      mutate(
        DBH    = PrevDBH,
        YR     = PrevYR,
        Status = "0",
        Census = if (has_census) min(Census, na.rm = TRUE) - 1L else NA_integer_
      )
    bind_rows(prev_census_rows, data)
  } else {
    data
  }

  ba_tbl <- data_for_ba |>
    filter(Status == "0", !is.na(DBH), !is.na(PA), PA > 0) |>
    group_by(PlotID, YR_grp = if (has_census) Census else floor(YR)) |>
    summarise(
      BA  = sum(pi * (DBH / 200)^2, na.rm = TRUE) / first(PA),
      YR  = mean(YR, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(Census_grp = YR_grp) |>
    mutate(
      BA      = round(BA, 3),
      BA_flag = case_when(
        BA >= 100 ~ "critical",
        BA >  50  ~ "warning",
        TRUE      ~ "ok"
      )
    )

  n_ba_warning  <- sum(ba_tbl$BA_flag == "warning")
  n_ba_critical <- sum(ba_tbl$BA_flag == "critical")

  # ── 9b. Basal area barplots (chunked) ────────────────────────────────────────
  ba_bar <- ba_tbl |>
    arrange(PlotID, YR) |>
    mutate(
      fill_col  = case_when(
        BA_flag == "critical" ~ "critical",
        BA_flag == "warning"  ~ "warning",
        TRUE                  ~ "ok"
      )
    )

  .ba_total     <- nrow(ba_bar)
  .n_ba_figs    <- max(1L, ceiling(.ba_total / 25L))
  chunk_size    <- ceiling(.ba_total / .n_ba_figs)

  ba_plot_groups <- local({
    plots   <- unique(ba_bar$PlotID)
    n_bars  <- sapply(plots, function(p) sum(ba_bar$PlotID == p))
    groups  <- list(); current <- c(); current_n <- 0L
    for (k in seq_along(plots)) {
      if (current_n + n_bars[k] > chunk_size && length(current) > 0) {
        groups  <- c(groups, list(current))
        current <- plots[k]; current_n <- n_bars[k]
      } else {
        current   <- c(current, plots[k])
        current_n <- current_n + n_bars[k]
      }
    }
    if (length(current) > 0) groups <- c(groups, list(current))
    groups
  })

  n_ba_chunks   <- length(ba_plot_groups)
  ba_plot_paths <- lapply(seq_len(n_ba_chunks), function(i) {
    chunk <- ba_bar |>
      filter(PlotID %in% ba_plot_groups[[i]]) |>
      mutate(
        bar_label = paste0(PlotID, "\n", round(YR, 2)),
        bar_label = factor(bar_label, levels = unique(bar_label)),
        bar_text  = paste0(PlotID, "-", floor(YR))
      )

    p <- ggplot2::ggplot(chunk,
                         ggplot2::aes(x = bar_label, y = BA, fill = fill_col)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = bar_text, y = BA + max(BA, na.rm = TRUE) * 0.01),
        angle = 90, hjust = 0, vjust = 0.5,
        size = 2.2, color = "grey30"
      ) +
      ggplot2::scale_x_discrete(labels = function(x) rep("", length(x))) +
      ggplot2::scale_fill_manual(
        name   = "BA flag",
        values = c("ok" = "#4DAF7C", "warning" = "#F5A623", "critical" = "#E84855"),
        labels = c("ok" = "OK (\u226450)", "warning" = "Warning (51\u2013100)",
                   "critical" = "Critical (\u2265100)")
      ) +
      ggplot2::geom_hline(yintercept = 50,  linetype = "dashed",
                          colour = "#F5A623", linewidth = 0.6) +
      ggplot2::geom_hline(yintercept = 100, linetype = "dashed",
                          colour = "#E84855", linewidth = 0.6) +
      ggplot2::labs(x = NULL, y = expression(BA ~ (m^2 ~ ha^{-1}))) +
      ggplot2::expand_limits(y = max(chunk$BA, na.rm = TRUE) * 1.6) +
      ggplot2::theme_minimal(base_size = 9) +
      ggplot2::theme(
        axis.text.x        = ggplot2::element_blank(),
        axis.ticks.x       = ggplot2::element_blank(),
        legend.position    = "bottom",
        panel.grid.major.x = ggplot2::element_blank()
      )

    path <- tempfile(paste0("ba_bar_part", i, "_"), fileext = ".png")
    ggplot2::ggsave(path, plot = p, width = 8, height = 4,
                    dpi = 200, limitsize = FALSE)
    path
  })
  ba_plot_paths <- unlist(ba_plot_paths)

  # ── 9c. Trees per hectare by plot x census ────────────────────────────────────
  # Uses data_for_ba which already includes reconstructed previous census rows
  # when only one census group was present in the original data.
  tph_tbl <- data_for_ba |>
    filter(Status == "0", !is.na(DBH), !is.na(PA)) |>
    group_by(PlotID, YR_grp = if (has_census) Census else floor(YR)) |>
    summarise(
      n_trees = n(),
      PA      = first(PA),
      TPH     = n_trees / PA,
      YR      = mean(YR, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(Census_grp = YR_grp) |>
    arrange(PlotID, YR)

  tph_bar <- tph_tbl |>
    arrange(PlotID, YR)

  tph_plot_groups <- local({
    plots   <- unique(tph_bar$PlotID)
    n_bars  <- sapply(plots, function(p) sum(tph_bar$PlotID == p))
    groups  <- list(); current <- c(); current_n <- 0L
    for (k in seq_along(plots)) {
      if (current_n + n_bars[k] > chunk_size && length(current) > 0) {
        groups  <- c(groups, list(current))
        current <- plots[k]; current_n <- n_bars[k]
      } else {
        current   <- c(current, plots[k])
        current_n <- current_n + n_bars[k]
      }
    }
    if (length(current) > 0) groups <- c(groups, list(current))
    groups
  })

  n_tph_chunks   <- length(tph_plot_groups)
  tph_plot_paths <- lapply(seq_len(n_tph_chunks), function(i) {
    chunk <- tph_bar |>
      filter(PlotID %in% tph_plot_groups[[i]]) |>
      mutate(
        bar_label = paste0(PlotID, "\n", round(YR, 2)),
        bar_label = factor(bar_label, levels = unique(bar_label)),
        bar_text  = paste0(PlotID, "-", floor(YR))
      )

    p <- ggplot2::ggplot(chunk, ggplot2::aes(x = bar_label, y = TPH)) +
      ggplot2::geom_col(width = 0.7, fill = "#4DAF7C") +
      ggplot2::geom_text(
        ggplot2::aes(label = bar_text, y = TPH + max(TPH, na.rm = TRUE) * 0.01),
        angle = 90, hjust = 0, vjust = 0.5,
        size = 2.2, color = "grey30"
      ) +
      ggplot2::scale_x_discrete(labels = function(x) rep("", length(x))) +
      ggplot2::labs(x = NULL, y = "TPH (stems ha\u207b\u00b9)") +
      ggplot2::expand_limits(y = max(chunk$TPH, na.rm = TRUE) * 1.6) +
      ggplot2::theme_minimal(base_size = 9) +
      ggplot2::theme(
        axis.text.x        = ggplot2::element_blank(),
        axis.ticks.x       = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank()
      )

    path <- tempfile(paste0("tph_bar_part", i, "_"), fileext = ".png")
    ggplot2::ggsave(path, plot = p, width = 8, height = 4,
                    dpi = 200, limitsize = FALSE)
    path
  })
  tph_plot_paths <- unlist(tph_plot_paths)

  # ── 10. Duplicate TreeID x YR ────────────────────────────────────────────────
  n_dups <- data |>
    count(PlotID, TreeID, YR, Status) |>
    filter(n > 1) |>
    nrow()

  # ── 11. Dead trees with subsequent records ───────────────────────────────────
  n_zombie <- data |>
    arrange(PlotID, TreeID, YR) |>
    group_by(PlotID, TreeID) |>
    mutate(ever_dead = cumany(Status == "1")) |>
    filter(ever_dead, Status %in% c("0", "2")) |>
    nrow()

  # ── Flags table (for export) ─────────────────────────────────────────────────
  flags_tbl <- tibble::tibble(
    Flag = c(
      "Missing YR",
      "Missing DBH",
      "Missing PrevDBH (all; expected for first intervals)",
      "Missing/malformed Species",
      "DBH < 10 cm",
      "Negative DBH growth",
      "Zero DBH growth",
      "Annual growth > 5 cm/yr",
      "Duplicate TreeID x YR",
      "Zombie trees (alive after death)",
      "BA 51-100 m2/ha (higher than typical)",
      "BA >= 100 m2/ha (implausible)",
      "PrevDBH value mismatches lag(DBH)",
      "PrevYR value mismatches lag(YR)",
      "PrevDBH missing but valid lag(DBH) exists (alive rows)",
      "PrevYR missing but valid lag(YR) exists (alive rows)"
    ),
    Count = c(
      n_missing_yr,
      n_missing_dbh,
      n_missing_prevdbh,
      n_missing_species,
      n_small,
      n_neg_growth,
      n_zero_growth,
      n_fast,
      n_dups,
      n_zombie,
      n_ba_warning,
      n_ba_critical,
      n_prevdbh_mismatch,
      n_prevyr_mismatch,
      n_prevdbh_orphan,
      n_prevyr_orphan
    ),
    Severity = c(
      "critical",
      "warning",
      "info",
      "warning",
      "warning",
      "warning",
      "info",
      "warning",
      "critical",
      "critical",
      "warning",
      "critical",
      "critical",
      "critical",
      "critical",
      "critical"
    )
  )

  hard_fails <- c(
    n_missing_yr,
    n_dups,
    n_ba_critical,
    n_zombie,
    n_prevdbh_mismatch,
    n_prevyr_mismatch,
    n_prevdbh_orphan,
    n_prevyr_orphan
  )
  soft_flags <- c(
    n_missing_dbh,
    n_small,
    n_neg_growth,
    n_fast,
    n_ba_warning
  )

  verdict <- dplyr::case_when(
    any(hard_fails > 0) ~ "FAIL: critical issues must be resolved before use.",
    any(soft_flags > 0) ~ "WARN: passed critical checks but has warnings worth reviewing.",
    TRUE                ~ "PASS: dataset looks clean."
  )

  # ── 12. Plot metadata (one row per PlotID) ───────────────────────────────────
  plot_meta_tbl <- NULL
  if (!is.null(metadata)) {
    expected_cols <- c("Country", "Site", "PI", "PIe",
                       "Censuses", "PlotID", "Size", "Latitude", "Longitude")
    missing_cols <- setdiff(expected_cols, names(metadata))
    if (length(missing_cols) > 0) {
      cli::cli_alert_warning(
        "metadata is missing columns: {paste(missing_cols, collapse = ', ')}. \\
        Ignoring metadata."
      )
    } else {
      plot_meta_tbl <- metadata |>
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

  if (!is.null(curation_log) && nzchar(trimws(curation_log))) {
    cli::cli_h2("Curation notes")
    cli::cli_verbatim(curation_log)
  }

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
    cli::cli_alert_success(
      "All plot x census BA values are within the expected range (\u2264 50 m\u00b2/ha)."
    )
  } else {
    print(ba_flagged)
  }

  cli::cli_h2("Trees per Hectare by Plot x Census")
  print(tph_tbl)

  cli::cli_h2("Data Quality Flags")

  flag <- function(n, msg, warn_thresh = 0) {
    if (n > warn_thresh) cli::cli_alert_warning("{n} {msg}")
    else cli::cli_alert_success("0 {msg}")
  }

  flag_critical <- function(n, msg) {
    if (n > 0) cli::cli_alert_danger("{n} {msg}")
    else cli::cli_alert_success("0 {msg}")
  }

  flag_critical(n_missing_yr,         "rows with missing YR — no temporal placement possible")
  flag(n_missing_dbh,                 "rows with missing DBH")
  flag(n_missing_prevdbh,             "rows with missing PrevDBH (expected for recruits/first intervals)")
  flag(n_missing_species,             "trees with missing or malformed Species")
  flag(n_small,                       "rows with DBH < 10 cm (below GFB3 threshold)")
  flag(n_neg_growth,                  "rows with negative DBH growth")
  flag(n_zero_growth,                 "rows with zero DBH growth")
  flag(n_fast,                        "rows with annual growth > 5 cm/yr (possible errors)")
  flag_critical(n_dups,               "duplicate TreeID x YR combinations")
  flag_critical(n_zombie,             "alive/recruit records following a death")
  flag(n_ba_warning,                  "plot x census combinations with BA 51-100 m\u00b2/ha (higher than typical)")
  flag_critical(n_ba_critical,        "plot x census combinations with BA \u2265 100 m\u00b2/ha (implausible)")
  flag_critical(n_prevdbh_mismatch,   "rows where PrevDBH does not match lag(DBH)")
  flag_critical(n_prevyr_mismatch,    "rows where PrevYR does not match lag(YR)")
  flag_critical(n_prevdbh_orphan,     "alive rows where PrevDBH is NA but lag(DBH) exists")
  flag_critical(n_prevyr_orphan,      "alive rows where PrevYR is NA but lag(YR) exists")

  cli::cli_h2("Prev Consistency Summary")
  if (n_inconsistent_prev == 0) {
    cli::cli_alert_success("All PrevDBH/PrevYR values are consistent with lagged records.")
  } else {
    cli::cli_alert_danger(
      "{n_inconsistent_prev} rows have at least one Prev inconsistency \\
      (see flags above for breakdown)."
    )
  }

  cli::cli_h2("Missing PA by Plot")
  if (nrow(na_pa_tbl) == 0) {
    cli::cli_alert_success("No missing PA values.")
  } else {
    cli::cli_alert_warning("{nrow(na_pa_tbl)} plot(s) have missing PA values:")
    print(na_pa_tbl)
  }

  cli::cli_h2("Missing Species by Plot")
  if (nrow(na_species_tbl) == 0) {
    cli::cli_alert_success("No missing or malformed Species values.")
  } else {
    cli::cli_alert_warning("{nrow(na_species_tbl)} plot(s) have missing or malformed Species values:")
    print(na_species_tbl)
  }

  cli::cli_h2("Unidentified Species by Plot")
  if (nrow(unid_tbl) == 0) {
    cli::cli_alert_success("No trees labelled '{unidentified_label}'.")
  } else {
    n_unid <- sum(unid_tbl$n_unidentified)
    cli::cli_alert_info("{n_unid} tree(s) labelled '{unidentified_label}':")
    print(unid_tbl)
  }

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
      unlink(ba_plot_paths)
      unlink(tph_plot_paths)
    }, add = TRUE)

    readr::write_csv(status_tbl,     file.path(tmp_dir, "status.csv"))
    readr::write_csv(flags_tbl,      file.path(tmp_dir, "flags.csv"))
    readr::write_csv(ba_tbl,         file.path(tmp_dir, "ba.csv"))
    readr::write_csv(tph_tbl,        file.path(tmp_dir, "tph.csv"))
    readr::write_csv(na_pa_tbl,      file.path(tmp_dir, "na_pa.csv"))
    readr::write_csv(na_species_tbl, file.path(tmp_dir, "na_species.csv"))
    readr::write_csv(unid_tbl,       file.path(tmp_dir, "unidentified.csv"))
    readr::write_csv(dbh_sum_obj,    file.path(tmp_dir, "dbh.csv"))
    readr::write_csv(as.data.frame(growth_sum_obj), file.path(tmp_dir, "growth.csv"))

    file.copy(hist_path,     file.path(tmp_dir, "growth_hist.png"))
    file.copy(dbh_hist_path, file.path(tmp_dir, "dbh_hist.png"))

    for (i in seq_along(ba_plot_paths)) {
      file.copy(ba_plot_paths[i],
                file.path(tmp_dir, paste0("ba_bar_part", i, ".png")))
    }
    for (i in seq_along(tph_plot_paths)) {
      file.copy(tph_plot_paths[i],
                file.path(tmp_dir, paste0("tph_bar_part", i, ".png")))
    }

    meta_df <- data.frame(
      key   = c("dataset_name", "n_rows", "n_trees", "n_plots",
                "yr_min", "yr_max", "verdict"),
      value = c(dataset_name, n_rows, n_trees, n_plots,
                yr_range[1], yr_range[2], verdict)
    )
    readr::write_csv(meta_df, file.path(tmp_dir, "meta.csv"))

    notes_text <- if (!is.null(curation_log)) curation_log else ""
    writeLines(notes_text, file.path(tmp_dir, "curation_log.txt"))

    if (!is.null(plot_meta_tbl)) {
      plot_meta_export <- plot_meta_tbl |>
        dplyr::mutate(
          Latitude  = round(Latitude,  2),
          Longitude = round(Longitude, 2)
        )
      readr::write_csv(plot_meta_export, file.path(tmp_dir, "metadata.csv"))

      map_path    <- file.path(tmp_dir, "plot_map.png")
      leaflet_map <- .render_plot_map(plot_meta_tbl, map_path)

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
          tmp_html <- tempfile(fileext = ".html")
          htmlwidgets::saveWidget(leaflet_map, file = tmp_html,
                                  selfcontained = TRUE, libdir = NULL)
          file.copy(tmp_html, html_path, overwrite = TRUE)
          unlink(tmp_html)
          cli::cli_alert_success("Interactive map written to: {html_path}")
        }
      }
    }

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
    status       = status_tbl,
    ba           = ba_tbl,
    tph          = tph_tbl,
    dbh          = dbh_sum_obj,
    growth       = growth_sum_obj,
    na_pa        = na_pa_tbl,
    na_species   = na_species_tbl,
    unidentified = unid_tbl,
    curation_log = curation_log,
    metadata     = plot_meta_tbl,
    flags = list(
      missing_yr          = n_missing_yr,
      missing_dbh         = n_missing_dbh,
      missing_prevdbh     = n_missing_prevdbh,
      missing_species     = n_missing_species,
      small_dbh           = n_small,
      neg_growth          = n_neg_growth,
      zero_growth         = n_zero_growth,
      fast_growth         = n_fast,
      duplicates          = n_dups,
      zombies             = n_zombie,
      ba_warning          = n_ba_warning,
      ba_critical         = n_ba_critical,
      prevdbh_mismatch    = n_prevdbh_mismatch,
      prevyr_mismatch     = n_prevyr_mismatch,
      prevdbh_orphan      = n_prevdbh_orphan,
      prevyr_orphan       = n_prevyr_orphan,
      inconsistent_prev   = n_inconsistent_prev
    )
  ))
}


# ── Internal helper: render static plot-location map ─────────────────────────
.render_plot_map <- function(tbl, path) {

  has_leaflet  <- requireNamespace("leaflet",  quietly = TRUE)
  has_mapview  <- requireNamespace("mapview",  quietly = TRUE)
  has_webshot2 <- requireNamespace("webshot2", quietly = TRUE)

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

    mapview::mapshot2(x = m, file = path, vwidth = 900, vheight = 600, zoom = 1)

    if (file.exists(path)) return(invisible(m))
    cli::cli_alert_warning(
      "mapshot2 did not produce a file; falling back to ggplot2 map."
    )
  }

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
    pts_sf <- sf::st_as_sf(tbl, coords = c("Longitude", "Latitude"), crs = 4326)
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = world, fill = "#EAE7DC", color = "#AAAAAA",
                       linewidth = 0.25) +
      ggplot2::geom_sf(data = pts_sf, color = "#2E4057", fill = "#E84855",
                       shape = 21, size = 3, stroke = 0.8) +
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      ggplot2::labs(x = "Longitude", y = "Latitude") +
      ggplot2::theme_minimal(base_size = 9) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#C8E6F5", color = NA),
        panel.grid       = ggplot2::element_line(color = "#CCCCCC66", linewidth = 0.2)
      )
    png(path, width = 700, height = 480, res = 96)
    on.exit(dev.off(), add = TRUE)
    print(p)
    return(invisible(NULL))
  }

  png(path, width = 700, height = 480, res = 96)
  on.exit(dev.off(), add = TRUE)
  plot(tbl$Longitude, tbl$Latitude,
       xlim = xlim, ylim = ylim,
       pch = 21, bg = "#E84855", col = "#2E4057", cex = 1.4,
       xlab = "Longitude", ylab = "Latitude", main = "Plot Locations")
  if (requireNamespace("maps", quietly = TRUE))
    maps::map("world", add = TRUE, col = "#AAAAAA")

  invisible(NULL)
}
