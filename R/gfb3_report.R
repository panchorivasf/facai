#' GFB3 Format Diagnostic Report
#'
#' Runs a suite of diagnostic checks on a GFB3-formatted dataset and prints a
#' structured report to the console. Optionally exports a narrative PDF report
#' and/or an Excel workbook with diagnostic tables.
#'
#' @param dat A tibble in GFB3 format, as produced by \code{convert_paracou_gfb3()}
#'   or equivalent. Must contain columns: \code{PlotID}, \code{PA},
#'   \code{Latitude}, \code{Longitude}, \code{TreeID}, \code{Species},
#'   \code{Status}, \code{DBH}, \code{YR}, \code{PrevDBH}, \code{PrevYR}.
#' @param export_pdf Logical. If \code{TRUE}, a narrative PDF report is written
#'   to \code{output_dir}. Requires Python with \code{reportlab} installed.
#'   Default \code{FALSE}.
#' @param export_xlsx Logical. If \code{TRUE}, an Excel workbook with diagnostic
#'   tables is written to \code{output_dir}. Requires Python with
#'   \code{openpyxl} installed. Default \code{FALSE}.
#' @param output_dir Character. Directory where exported files are saved.
#'   Defaults to the current working directory.
#' @param dataset_name Character. Label used in export filenames and report
#'   headers. Defaults to \code{"gfb3_dataset"}.
#'
#' @return Invisibly returns a named list with elements \code{status},
#'   \code{dbh}, \code{growth}, and \code{flags}.
#'
#' @examples
#' \dontrun{
#' result <- gfb3_report(paracou_gfb3,
#'                               export_pdf  = TRUE,
#'                               export_xlsx = TRUE,
#'                               output_dir  = "reports/",
#'                               dataset_name = "paracou_01")
#' }
#'
#' @importFrom dplyr count mutate select filter summarise pull arrange group_by
#'   ungroup n_distinct distinct left_join cumany
#' @importFrom cli cli_h1 cli_h2 cli_bullets cli_alert_warning cli_alert_success
#'   cli_alert_danger
#' @export
gfb3_report <- function(dat,
                        export_pdf   = FALSE,
                        export_xlsx  = FALSE,
                        output_dir   = ".",
                        dataset_name = "data") {

  # ── 1. Basic counts ──────────────────────────────────────────────────────────
  n_rows   <- nrow(dat)
  n_trees  <- n_distinct(dat$TreeID)
  n_plots  <- n_distinct(dat$PlotID)
  yr_range <- range(dat$YR, na.rm = TRUE)

  # ── 2. Status distribution ───────────────────────────────────────────────────
  status_labels <- c(
    "0" = "alive",
    "1" = "dead between inventories",
    "2" = "new recruit",
    "9" = "missing"
  )

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

  # ── 9. Duplicate TreeID x YR ─────────────────────────────────────────────────
  n_dups <- dat |>
    count(TreeID, YR) |>
    filter(n > 1) |>
    nrow()

  # ── 10. Dead trees with subsequent records ───────────────────────────────────
  n_zombie <- dat |>
    arrange(TreeID, YR) |>
    group_by(TreeID) |>
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
      "Zombie trees (alive after death)"
    ),
    Count = c(
      n_missing_dbh, n_missing_prevdbh, n_missing_species,
      n_small, n_neg_growth, n_zero_growth, n_fast,
      n_dups, n_zombie
    ),
    Severity = c(
      "warning", "info", "warning",
      "warning", "warning", "info", "warning",
      "critical", "critical"
    )
  )

  hard_fails <- c(n_dups, n_zombie)
  soft_flags <- c(n_missing_dbh, n_small, n_neg_growth, n_fast)

  verdict <- dplyr::case_when(
    any(hard_fails > 0) ~ "FAIL: critical issues must be resolved before use.",
    any(soft_flags > 0) ~ "WARN: passed critical checks but has warnings worth reviewing.",
    TRUE                ~ "PASS: dataset looks clean."
  )

  # ── CONSOLE REPORT ───────────────────────────────────────────────────────────
  cli::cli_h1("GFB3 Format Diagnostic Report")

  cli::cli_h2("Overview")
  cli::cli_bullets(c(
    "*" = "Dataset: {dataset_name}",
    "*" = "Rows: {n_rows}",
    "*" = "Trees: {n_trees}",
    "*" = "Plots: {n_plots}",
    "*" = "Year range: {yr_range[1]} - {yr_range[2]}"
  ))

  cli::cli_h2("Status Distribution")
  print(status_tbl)

  cli::cli_h2("DBH Summary")
  cat(dbh_sum, sep = "\n")

  cli::cli_h2("Growth Summary")
  cat(growth_sum, sep = "\n")

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

  cli::cli_h2("Verdict")
  if (any(hard_fails > 0)) {
    cli::cli_alert_danger(verdict)
  } else if (any(soft_flags > 0)) {
    cli::cli_alert_warning(verdict)
  } else {
    cli::cli_alert_success(verdict)
  }

  # ── EXPORTS ──────────────────────────────────────────────────────────────────
  if (export_pdf || export_xlsx) {
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
    status = status_tbl,
    dbh    = dbh_sum_obj,
    growth = growth_sum_obj,
    flags  = list(
      missing_dbh     = n_missing_dbh,
      missing_prevdbh = n_missing_prevdbh,
      missing_species = n_missing_species,
      small_dbh       = n_small,
      neg_growth      = n_neg_growth,
      zero_growth     = n_zero_growth,
      fast_growth     = n_fast,
      duplicates      = n_dups,
      zombies         = n_zombie
    )
  ))
}
