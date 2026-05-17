#' Impute Missing Fractional Year by Group
#'
#' Converts a \code{Date} column to fractional year and imputes missing values
#' using the modal date within each group (typically \code{Quadrat x Census} or
#' \code{PlotID x Census}). Imputed rows are flagged in an optional notes column.
#'
#' @param data A data frame containing at least a \code{Date} column and the
#'   grouping columns specified in \code{group_vars}.
#' @param date_col Character. Name of the date column to convert and impute.
#'   Default \code{"Date"}.
#' @param yr_col Character. Name of the output fractional year column to create.
#'   Default \code{"YR"}.
#' @param group_vars Character vector of column names to group by when computing
#'   the modal date. Default \code{c("Quadrat", "Census")}. Use
#'   \code{c("PlotID", "Census")} for plot-level imputation.
#' @param note_col Character. Name of the column in which to store imputation
#'   notes. Default \code{"note"}. Set to \code{NULL} to suppress note creation.
#'
#' @return The input data frame with a new fractional year column (\code{yr_col})
#'   and, if \code{note_col} is not \code{NULL}, an imputation note column.
#'   Rows where the date was already present have \code{NA} in the note column.
#'
#' @examples
#' \dontrun{
#' # Impute by Quadrat x Census (default)
#' c1 <- impute_yr(c1)
#'
#' # Impute by PlotID x Census
#' c1 <- impute_yr(c1, group_vars = c("PlotID", "Census"))
#'
#' # Custom column names
#' c1 <- impute_yr(c1, date_col = "ExactDate", yr_col = "YR", note_col = "curation_note")
#' }
#'
#' @importFrom dplyr mutate group_by ungroup if_else
#' @export
impute_yr <- function(data,
                      date_col   = "Date",
                      yr_col     = "YR",
                      group_vars = c("Quadrat", "Census"),
                      note_col   = "note") {

  # ── Internal helpers ────────────────────────────────────────────────────────
  .date_to_frac_yr <- function(d) {
    d            <- as.Date(d)
    year         <- as.integer(format(d, "%Y"))
    day_of_year  <- as.integer(format(d, "%j"))
    days_in_year <- ifelse(
      (year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0, 366L, 365L
    )
    year + (day_of_year - 1) / days_in_year
  }

  .mode_val <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    as.numeric(names(sort(table(x), decreasing = TRUE))[1])
  }

  # ── Validate inputs ─────────────────────────────────────────────────────────
  missing_cols <- setdiff(c(date_col, group_vars), names(data))
  if (length(missing_cols) > 0)
    cli::cli_abort("Column(s) not found in data: {paste(missing_cols, collapse = ', ')}")

  # ── Compute fractional year ──────────────────────────────────────────────────
  data[[yr_col]] <- .date_to_frac_yr(data[[date_col]])

  # ── Build note column (before grouping, using base ifelse) ───────────────────
  if (!is.null(note_col)) {
    group_label <- do.call(paste, c(
      lapply(group_vars, function(v) as.character(data[[v]])),
      list(sep = " ")
    ))
    data[[note_col]] <- ifelse(
      is.na(data[[yr_col]]),
      paste0("YR imputed from modal date of ", group_label),
      NA_character_
    )
  }

  # ── Impute by group ──────────────────────────────────────────────────────────
  data <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::mutate(
      .modal_yr  = .mode_val(.data[[yr_col]]),
      .imputed   = is.na(.data[[yr_col]]),
      "{yr_col}" := dplyr::if_else(.imputed, .modal_yr, .data[[yr_col]])
    ) |>
    dplyr::select(-.modal_yr, -.imputed) |>
    dplyr::ungroup()

  # ── Warn if any YR still NA after imputation ─────────────────────────────────
  n_still_na <- sum(is.na(data[[yr_col]]))
  if (n_still_na > 0)
    cli::cli_alert_warning(
      "{n_still_na} rows still have NA {yr_col} after imputation — \\
      entire group(s) may have no valid dates. \\
      Consider using broader group_vars (e.g. c('PlotID', 'Census'))."
    )

  data
}
