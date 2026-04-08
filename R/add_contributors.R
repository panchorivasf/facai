#' Add one or more contributors to the InterNodes master contributor table
#'
#' @param x A data frame with contributor data, or a single-row tibble.
#'   Required columns: `full_name`, `first_name`, `last_name`, `email`,
#'   `affiliation_1`. Optional: `affiliation_2`, `affiliation_3`.
#' @param master_path Path to the master contributor Parquet file.
#' @param region Two-letter region code for ID prefix (e.g. `"af"`, `"sa"`,
#'   `"as"`). Default `NULL` uses plain `in_ctb_NNNN` with no region infix.
#' @param dry_run If `TRUE`, returns the rows that *would* be inserted without
#'   writing anything. Useful for inspection before committing.
#'
#' @return Invisibly returns the newly inserted rows as a data frame, with
#'   their assigned `contributor_id` values.
#'
#' @details
#' Duplicate detection is based on `email`. Any incoming row whose email
#' already exists in the master is skipped with a message. Rows with a
#' missing or `"?"` email are matched on `full_name` instead, with a warning.
#'
#' IDs are assigned by querying `MAX(contributor_id)` from the master via
#' DuckDB and incrementing from there, so parallel ingestion from different
#' scripts is safe as long as they do not run truly simultaneously on the same
#' file.
#'
#' @examples
#' \dontrun{
#' # Single contributor
#' add_contributors(
#'   data.frame(
#'     full_name     = "Jane Doe",
#'     first_name    = "Jane",
#'     last_name     = "Doe",
#'     email         = "jane.doe@uni.edu",
#'     affiliation_1 = "University of Example, Country"
#'   ),
#'   master_path = "data/in_master_contributor.parquet",
#'   region = "af"
#' )
#'
#' # Batch ingest from a formatted table
#' add_contributors(
#'   my_contributors_df,
#'   master_path = "data/in_master_contributor.parquet",
#'   region = "af",
#'   dry_run = TRUE   # inspect first
#' )
#' }
#' @export
add_contributors <- function(x,
                            master_path = "/in_master_contributor.parquet",
                            region = NULL,
                            dry_run = FALSE) {

  stopifnot(is.data.frame(x))

  required_cols <- c("full_name", "first_name", "last_name", "email", "affiliation_1")
  missing_cols  <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns: {.field {missing_cols}}")
  }

  # Ensure optional affiliation columns exist
  x <- .ensure_cols(x, c("affiliation_2", "affiliation_3"), fill = NA_character_)

  # Normalise: trim whitespace, standardise missing email sentinel
  x <- x |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), stringr::str_trim),
      email = dplyr::if_else(email %in% c("", "?", "-", NA_character_),
                             NA_character_, email)
    )

  # ---- Load or initialise master ----------------------------------------
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  if (file.exists(master_path)) {
    master <- duckdb::dbGetQuery(
      con,
      glue::glue("SELECT * FROM read_parquet('{master_path}')")
    )
  } else {
    cli::cli_alert_info("Master file not found — creating new master at {.path {master_path}}")
    master <- .empty_master()
  }

  # ---- Duplicate detection ----------------------------------------------
  new_rows   <- list()
  skipped    <- 0L

  for (i in seq_len(nrow(x))) {
    row <- x[i, ]

    is_dup <- if (!is.na(row$email)) {
      # Primary key: email
      any(!is.na(master$email) & master$email == row$email)
    } else {
      # Fallback: full_name match, with warning
      match_name <- any(master$full_name == row$full_name)
      if (match_name) {
        cli::cli_warn(
          "No email for {.val {row$full_name}} — matched on full_name. Skipping."
        )
      }
      match_name
    }

    if (is_dup) {
      cli::cli_alert_warning("Duplicate skipped: {.val {row$full_name}} ({row$email})")
      skipped <- skipped + 1L
    } else {
      new_rows[[length(new_rows) + 1L]] <- row
    }
  }

  if (length(new_rows) == 0L) {
    cli::cli_alert_info("No new contributors to add ({skipped} duplicate(s) skipped).")
    return(invisible(NULL))
  }

  new_df <- dplyr::bind_rows(new_rows)

  # ---- Assign IDs -------------------------------------------------------
  next_n <- .next_id_n(master$contributor_id)

  id_prefix <- if (!is.null(region)) {
    paste0("in_ctb_", tolower(region), "_")
  } else {
    "in_ctb_"
  }

  new_df <- new_df |>
    dplyr::mutate(
      contributor_id = sprintf(
        paste0(id_prefix, "%04d"),
        seq(next_n, next_n + dplyr::n() - 1L)
      ),
      .before = 1
    )

  # ---- Dry run ----------------------------------------------------------
  if (dry_run) {
    cli::cli_alert_info(
      "dry_run = TRUE: {nrow(new_df)} row(s) would be inserted, \\
       {skipped} duplicate(s) skipped."
    )
    return(invisible(new_df))
  }

  # ---- Write ------------------------------------------------------------
  updated <- dplyr::bind_rows(master, new_df)

  arrow::write_parquet(updated, master_path)

  cli::cli_alert_success(
    "Added {nrow(new_df)} contributor(s) to {.path {master_path}} \\
     ({skipped} duplicate(s) skipped)."
  )

  invisible(new_df)
}


# ---- Private helpers -----------------------------------------------------

#' @keywords internal
.ensure_cols <- function(df, cols, fill = NA_character_) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- fill
  }
  df
}

#' @keywords internal
.empty_master <- function() {
  data.frame(
    contributor_id = character(),
    full_name      = character(),
    first_name     = character(),
    last_name      = character(),
    email          = character(),
    affiliation_1  = character(),
    affiliation_2  = character(),
    affiliation_3  = character(),
    stringsAsFactors = FALSE
  )
}

#' Extract the numeric suffix from the highest existing contributor_id
#' and return the next integer to use.
#' @keywords internal
.next_id_n <- function(existing_ids) {
  if (length(existing_ids) == 0L) return(1L)
  nums <- suppressWarnings(
    as.integer(stringr::str_extract(existing_ids, "\\d{4}$"))
  )
  max(nums, na.rm = TRUE) + 1L
}
