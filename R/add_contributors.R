#' Add or update contributors in the FACAI contributors mastersheet
#'
#' @description
#' Registers one or more contributors into the centralized
#' \code{contributors_mastersheet.parquet} stored in the data depot. Each
#' contributor is assigned a unique \code{contributor_id}. A file-based lock
#' prevents simultaneous writes from multiple sessions.
#'
#' @param entry A data frame (one or more rows) or a named list (single entry).
#'   Required fields:
#'   \describe{
#'     \item{full_name}{Character. Full name of the contributor.}
#'     \item{first_name}{Character. First name.}
#'     \item{mid_init}{Character. Middle initial(s). \code{NA} if none.}
#'     \item{last_names}{Character. Last name(s).}
#'     \item{email_1}{Character. Primary email address.}
#'     \item{email_2}{Character. Secondary email address. \code{NA} if none.}
#'     \item{affiliation_1}{Character. Primary institutional affiliation.}
#'     \item{affiliation_2}{Character. Secondary affiliation. \code{NA} if none.}
#'     \item{affiliation_3}{Character. Tertiary affiliation. \code{NA} if none.}
#'     \item{affiliation_4}{Character. Quaternary affiliation. \code{NA} if none.}
#'   }
#' @param depot_path Character. Path to the shared data depot directory
#'   containing \code{contributors_mastersheet.parquet}.
#' @param download Logical. If \code{TRUE} (default), copies the updated
#'   mastersheet to the current working directory as
#'   \code{contributors_mastersheet_<YYYYMMDD>.parquet}.
#'
#' @return Invisibly returns the updated mastersheet as a data frame.
#'
#' @details
#' Each contributor is assigned a unique \code{contributor_id} of the form
#' \code{ctb_<NNNN>}, where \code{<NNNN>} is a zero-padded 4-digit integer
#' unique within the full mastersheet.
#'
#' A \code{.lock} file is created in \code{depot_path} for the duration of the
#' write operation. If a lock file younger than 5 minutes is found, the function
#' stops with an informative error. Stale locks (older than 5 minutes) are
#' removed automatically with a warning.
#'
#' If \code{contributors_mastersheet.parquet} does not exist in
#' \code{depot_path}, a new one is created automatically.
#'
#' Legacy column names (\code{middle_init}, \code{last_name}, \code{email})
#' are silently remapped to the current names for backwards compatibility.
#'
#' @importFrom arrow read_parquet write_parquet
#' @importFrom dplyr bind_rows
#' @importFrom fs path
#'
#' @examples
#' \dontrun{
#' # Single entry as a named list
#' add_contributors(
#'   entry = list(
#'     full_name     = "Jane A. Doe",
#'     first_name    = "Jane",
#'     mid_init      = "A.",
#'     last_names    = "Doe",
#'     email_1       = "jane.doe@university.edu",
#'     email_2       = NA_character_,
#'     affiliation_1 = "University of Example",
#'     affiliation_2 = NA_character_,
#'     affiliation_3 = NA_character_,
#'     affiliation_4 = NA_character_
#'   ),
#'   depot_path = "/path/to/depot"
#' )
#'
#' # Multiple entries as a data frame
#' entries <- data.frame(
#'   full_name     = c("Jane A. Doe", "John B. Smith"),
#'   first_name    = c("Jane", "John"),
#'   mid_init      = c("A.", "B."),
#'   last_names    = c("Doe", "Smith"),
#'   email_1       = c("jane@uni.edu", "john@uni.edu"),
#'   email_2       = c(NA, NA),
#'   affiliation_1 = c("University of Example", "Institute of Science"),
#'   affiliation_2 = c(NA, NA),
#'   affiliation_3 = c(NA, NA),
#'   affiliation_4 = c(NA, NA),
#'   stringsAsFactors = FALSE
#' )
#' add_contributors(entries, depot_path = "/path/to/depot")
#' }
#'
#' @export
add_contributors <- function(entry, depot_path, download = TRUE) {

  # --- 0. Dependencies ------------------------------------------------------
  required_pkgs <- c("arrow", "dplyr", "fs")
  invisible(lapply(required_pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' is required but not installed.", pkg),
           call. = FALSE)
  }))

  # --- 1. Normalize input to data frame -------------------------------------
  if (is.list(entry) && !is.data.frame(entry)) {
    entry <- as.data.frame(entry, stringsAsFactors = FALSE)
  }
  stopifnot(is.data.frame(entry))

  # --- 2. Backwards compatibility: remap legacy column names ----------------
  col_renames <- c(
    middle_init = "mid_init",
    last_name   = "last_names",
    email       = "email_1"
  )
  for (old in names(col_renames)) {
    new <- col_renames[[old]]
    if (old %in% names(entry) && !new %in% names(entry)) {
      names(entry)[names(entry) == old] <- new
    }
  }

  # --- 3. Fill optional fields if absent ------------------------------------
  optional_fields <- c(
    "mid_init", "email_2",
    "affiliation_2", "affiliation_3", "affiliation_4"
  )
  for (f in optional_fields) {
    if (!f %in% names(entry)) entry[[f]] <- NA_character_
  }

  # --- 4. Validate required fields ------------------------------------------
  required_fields <- c(
    "full_name", "first_name", "mid_init", "last_names",
    "email_1", "email_2",
    "affiliation_1", "affiliation_2", "affiliation_3", "affiliation_4"
  )
  missing_fields <- setdiff(required_fields, names(entry))
  if (length(missing_fields) > 0) {
    stop(
      "Missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  # --- 5. Acquire file lock -------------------------------------------------
  mastersheet_file <- "contributors_mastersheet.parquet"
  mastersheet_path <- fs::path(depot_path, mastersheet_file)
  lock_path        <- fs::path(depot_path, paste0(mastersheet_file, ".lock"))

  if (file.exists(lock_path)) {
    lock_age <- as.numeric(
      difftime(Sys.time(), file.mtime(lock_path), units = "mins")
    )
    if (lock_age < 5) {
      lock_info <- tryCatch(readLines(lock_path), error = function(e) "unknown")
      stop(
        "Contributors mastersheet is locked by another process ",
        "(user: ", lock_info[1], ", since: ", lock_info[2], "). ",
        "Please wait and try again.",
        call. = FALSE
      )
    } else {
      warning("Removing stale lock file (> 5 minutes old).", call. = FALSE)
      file.remove(lock_path)
    }
  }

  writeLines(
    c(Sys.getenv("USER"), as.character(Sys.time())),
    lock_path
  )
  on.exit(
    if (file.exists(lock_path)) file.remove(lock_path),
    add = TRUE
  )

  # --- 6. Load or initialize mastersheet ------------------------------------
  if (file.exists(mastersheet_path)) {
    master <- arrow::read_parquet(mastersheet_path)
  } else {
    message("No existing contributors mastersheet found. Initializing a new one.")
    master <- data.frame(
      contributor_id = character(),
      full_name      = character(),
      first_name     = character(),
      mid_init       = character(),
      last_names     = character(),
      email_1        = character(),
      email_2        = character(),
      affiliation_1  = character(),
      affiliation_2  = character(),
      affiliation_3  = character(),
      affiliation_4  = character(),
      stringsAsFactors = FALSE
    )
  }

  # --- 7. Generate unique contributor_id values -----------------------------
  existing_nums <- if (nrow(master) > 0 && "contributor_id" %in% names(master)) {
    as.integer(regmatches(
      master$contributor_id,
      regexpr("[0-9]{4}$", master$contributor_id)
    ))
  } else {
    integer(0)
  }

  next_id <- if (length(existing_nums) > 0) max(existing_nums) + 1L else 1L

  entry$contributor_id <- vapply(seq_len(nrow(entry)), function(i) {
    sprintf("ctb_%04d", next_id + i - 1L)
  }, character(1))

  if (any(entry$contributor_id %in% master$contributor_id)) {
    stop(
      "Duplicate contributor_id detected — please inspect the mastersheet.",
      call. = FALSE
    )
  }

  # --- 8. Enforce column order and bind -------------------------------------
  col_order <- c(
    "contributor_id",
    "full_name", "first_name", "mid_init", "last_names",
    "email_1", "email_2",
    "affiliation_1", "affiliation_2", "affiliation_3", "affiliation_4"
  )
  entry  <- entry[, col_order, drop = FALSE]
  master <- master[, col_order, drop = FALSE]
  master <- dplyr::bind_rows(master, entry)

  # --- 9. Write to depot ----------------------------------------------------
  arrow::write_parquet(master, mastersheet_path)
  message(sprintf(
    "Contributors mastersheet updated: %d new entry/entries added.", nrow(entry)
  ))

  # --- 10. Optionally copy to working directory -----------------------------
  if (download) {
    local_name <- sprintf(
      "contributors_mastersheet_%s.parquet",
      format(Sys.Date(), "%Y%m%d")
    )
    local_path <- file.path(getwd(), local_name)
    file.copy(mastersheet_path, local_path, overwrite = TRUE)
    message(sprintf("Local copy saved: %s", local_path))
  }

  invisible(master)
}
