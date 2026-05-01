#' Plot a growth histogram and return a summary table
#'
#' @description
#' Computes DBH increment (DBH - PrevDBH) for each observation, plots a
#' histogram, and returns a summary table with counts of negative and positive
#' growth alongside standard descriptive statistics. Optionally exports the
#' summary table to an Excel file.
#'
#' @param df A data frame containing at least \code{DBH} and \code{PrevDBH}
#'   columns (numeric, in the same units).
#' @param export Logical. If \code{TRUE}, exports the summary table to an
#'   \code{.xlsx} file. Defaults to \code{FALSE}.
#' @param file_name Character. Name of the output file (without extension).
#'   Defaults to \code{"growth_summary"}. Ignored if \code{export = FALSE}.
#' @param dir Character. Output directory for the exported file. Defaults to
#'   the current working directory (\code{"."}). Ignored if
#'   \code{export = FALSE}.
#'
#' @return Invisibly returns the summary table as a tibble. The histogram is
#'   drawn as a side effect. If \code{export = TRUE}, an \code{.xlsx} file is
#'   written to \code{dir}.
#'
#' @importFrom dplyr mutate summarise
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' \dontrun{
#' # Plot and print summary
#' growth_summary(ituri_gfb3)
#'
#' # Export summary to Excel
#' growth_summary(ituri_gfb3, export = TRUE,
#'             file_name = "ituri_growth", dir = "output/diagnostics")
#' }
#'
#' @export
growth_summary <- function(df, export = FALSE,
                        file_name = "growth_summary",
                        dir = ".") {

  df <- df |>
    dplyr::filter(!is.na(DBH), !is.na(PrevDBH)) |>
    dplyr::mutate(DBHdiff = DBH - PrevDBH)

  histogram <- hist(df$DBHdiff,
       main   = "Tree Growth",
       xlab   = "DBH Difference (cm)",
       ylab   = "Frequency",
       breaks = 100)

  summary_tbl <- df |>
    dplyr::summarise(
      n          = sum(!is.na(DBHdiff)),
      n_negative = sum(DBHdiff < 0,  na.rm = TRUE),
      n_positive = sum(DBHdiff > 0,  na.rm = TRUE),
      mean       = round(mean(DBHdiff,     na.rm = TRUE),2),
      median     = round(median(DBHdiff,   na.rm = TRUE),2),
      sd         = round(sd(DBHdiff,       na.rm = TRUE),2),
      q25        = round(quantile(DBHdiff, 0.25, na.rm = TRUE),2),
      q75        = round(quantile(DBHdiff, 0.75, na.rm = TRUE),2),
      min        = round(min(DBHdiff,      na.rm = TRUE),2),
      max        = round(max(DBHdiff,      na.rm = TRUE),2)
    )

  print(summary_tbl)

  if (export) {
    if (!requireNamespace("openxlsx", quietly = TRUE))
      stop("Package 'openxlsx' is required for export. Please install it.",
           call. = FALSE)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    out_path <- file.path(dir, paste0(file_name, ".xlsx"))
    openxlsx::write.xlsx(summary_tbl, out_path)
    message(sprintf("Summary exported to: %s", out_path))
  }

  invisible(summary_tbl)
}
