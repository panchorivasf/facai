
#' Add or update data units in the FACAI data unit mastersheets
#'
#' @description
#' Registers one or more data units into the centralized remeasured or single
#' measurement mastersheet stored in the data depot. A data unit is defined as
#' a plot cluster sharing the same country and institution/PI team, typically
#' associated with a conservation area or geographic subregion. UN region fields
#' are derived automatically from the country name using \pkg{countrycode}. A
#' file-based lock prevents simultaneous writes from multiple sessions.
#'
#' @param entry A data frame (one or more rows) or a named list (single entry).
#'   Required fields:
#'   \describe{
#'     \item{country}{Character. Country name following UN M.49 standard.}
#'     \item{original_name}{Character. Dataset name as it appears on the data
#'       portal or source spreadsheet. \code{NA} if unknown.}
#'     \item{coordinator}{Character. Name of the person responsible for data entry.}
#'     \item{coordinator_email}{Character. Email of the data entry coordinator.}
#'     \item{notes}{Character. Free-text notes. \code{NA} if none.}
#'     \item{confidential}{Integer. \code{1} = confidential, \code{0} = not specified.}
#'     \item{citation}{Character. Citation string for this dataset.}
#'     \item{acknowledgement}{Character. Names to acknowledge. Usually \code{NA}.}
#'     \item{parent_in_dsn}{Character. Optional. For single-measurement entries
#'       derived from a remeasured unit, the \code{in_dsn} of the parent record
#'       in the remeasured mastersheet. \code{NA} otherwise.}
#'   }
#' @param type Character. Either \code{"remeasured"} or \code{"single"},
#'   controlling which mastersheet is updated and which frequency code
#'   (\code{"r"} or \code{"s"}) is used in the \code{in_dsn} identifier.
#' @param depot_path Character. Path to the shared data depot directory.
#' @param download Logical. If \code{TRUE} (default), copies the updated
#'   mastersheet to the current working directory as
#'   \code{in_<type>_mastersheet_<YYYYMMDD>.parquet}.
#'
#' @return Invisibly returns the updated mastersheet as a data frame.
#'
#' @details
#' Each entry is assigned a unique identifier \code{in_dsn} with the format
#' \code{in_<ISO3>_<freq>_<NNNN>}, where \code{<ISO3>} is the uppercase 3-letter
#' ISO country code, \code{<freq>} is \code{r} (remeasured) or \code{s}
#' (single), and \code{<NNNN>} is a zero-padded 4-digit integer unique within
#' the full mastersheet.
#'
#' UN geographic fields (\code{continent}, \code{un_region},
#' \code{un_subregion}) are derived automatically and should not be supplied
#' manually.
#'
#' A \code{.lock} file is created in \code{depot_path} for the duration of the
#' write operation. If a lock file younger than 5 minutes is found, the function
#' stops with an informative error. Stale locks (older than 5 minutes) are
#' removed automatically with a warning.
#'
#' If the target mastersheet does not exist in \code{depot_path}, a new one is
#' created automatically.
#'
#' @importFrom arrow read_parquet write_parquet
#' @importFrom countrycode countrycode
#' @importFrom dplyr bind_rows
#' @importFrom fs path
#'
#' @examples
#' \dontrun{
#' # Register a remeasured data unit
#' add_data_units(
#'   entry = list(
#'     country           = "Peru",
#'     original_name     = "SERFOR National Forest Inventory",
#'     coordinator       = "Fran Palma",
#'     coordinator_email = "fpalma@purdue.edu",
#'     notes             = "Shared via SERFOR-Purdue agreement, May 2026",
#'     confidential      = 0L,
#'     citation          = "SERFOR (2024). Inventario Nacional Forestal.",
#'     acknowledgement   = NA_character_,
#'     parent_in_dsn     = NA_character_
#'   ),
#'   type       = "remeasured",
#'   depot_path = "/path/to/depot"
#' )
#'
#' # Register a single-measurement unit derived from a remeasured parent
#' add_data_units(
#'   entry = list(
#'     country           = "Peru",
#'     original_name     = "SERFOR NFI - Census 2018",
#'     coordinator       = "Lab Member",
#'     coordinator_email = "labmember@purdue.edu",
#'     notes             = "Derived from in_PER_r_0001",
#'     confidential      = 0L,
#'     citation          = "SERFOR (2024). Inventario Nacional Forestal.",
#'     acknowledgement   = NA_character_,
#'     parent_in_dsn     = "in_PER_r_0001"
#'   ),
#'   type       = "single",
#'   depot_path = "/path/to/depot"
#' )
#' }
#'
#' @export
add_data_units <- function(entry, type, depot_path, download = TRUE) {

  # --- 0. Dependencies ------------------------------------------------------
  required_pkgs <- c("arrow", "countrycode", "dplyr", "fs")
  invisible(lapply(required_pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' is required but not installed.", pkg),
           call. = FALSE)
  }))

  # --- 1. Validate type argument --------------------------------------------
  type <- match.arg(type, choices = c("remeasured", "single"))
  freq_code        <- switch(type, remeasured = "r", single = "s")
  mastersheet_file <- switch(
    type,
    remeasured = "remeasured_mastersheet.parquet",
    single     = "single_mastersheet.parquet"
  )

  # --- 2. Normalize input to data frame -------------------------------------
  if (is.list(entry) && !is.data.frame(entry)) {
    entry <- as.data.frame(entry, stringsAsFactors = FALSE)
  }
  stopifnot(is.data.frame(entry))

  # Fill optional fields if absent
  if (!"parent_in_dsn" %in% names(entry)) entry$parent_in_dsn <- NA_character_

  # Stamp frequency from type (not user-supplied)
  entry$frequency <- freq_code

  # --- 3. Validate required fields ------------------------------------------
  required_fields <- c(
    "country", "original_name", "coordinator", "coordinator_email",
    "notes", "confidential", "citation", "acknowledgement", "parent_in_dsn"
  )
  missing_fields <- setdiff(required_fields, names(entry))
  if (length(missing_fields) > 0) {
    stop(
      "Missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  if (any(!entry$confidential %in% c(0L, 1L))) {
    stop("'confidential' must be 0 or 1.", call. = FALSE)
  }

  # --- 4. Derive UN geographic fields and ISO3 ------------------------------
  entry$continent    <- countrycode::countrycode(
    entry$country, "country.name", "continent",         warn = TRUE
  )
  entry$un_region    <- countrycode::countrycode(
    entry$country, "country.name", "un.region.name",    warn = TRUE
  )
  entry$un_subregion <- countrycode::countrycode(
    entry$country, "country.name", "un.regionsub.name", warn = TRUE
  )
  entry$iso3 <- toupper(countrycode::countrycode(
    entry$country, "country.name", "iso3c", warn = FALSE
  ))

  geo_fail <- is.na(entry$continent) | is.na(entry$un_region)
  if (any(geo_fail)) {
    warning(
      "Could not resolve UN geographic fields for: ",
      paste(unique(entry$country[geo_fail]), collapse = ", "),
      ". Check spelling against UN M.49 standard.",
      call. = FALSE
    )
  }

  # --- 5. Acquire file lock -------------------------------------------------
  mastersheet_path <- fs::path(depot_path, mastersheet_file)
  lock_path        <- fs::path(depot_path, paste0(mastersheet_file, ".lock"))

  if (file.exists(lock_path)) {
    lock_age <- as.numeric(
      difftime(Sys.time(), file.mtime(lock_path), units = "mins")
    )
    if (lock_age < 5) {
      lock_info <- tryCatch(readLines(lock_path), error = function(e) "unknown")
      stop(
        tools::toTitleCase(type), " mastersheet is locked by another process ",
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
    message(sprintf(
      "No existing %s mastersheet found. Initializing a new one.", type
    ))
    master <- data.frame(
      in_dsn            = character(),
      frequency         = character(),
      country           = character(),
      iso3              = character(),
      continent         = character(),
      un_region         = character(),
      un_subregion      = character(),
      original_name     = character(),
      coordinator       = character(),
      coordinator_email = character(),
      notes             = character(),
      confidential      = integer(),
      citation          = character(),
      acknowledgement   = character(),
      parent_in_dsn     = character(),
      stringsAsFactors  = FALSE
    )
  }

  # --- 7. Generate unique in_dsn identifiers --------------------------------
  existing_nums <- if (nrow(master) > 0 && "in_dsn" %in% names(master)) {
    as.integer(regmatches(
      master$in_dsn,
      regexpr("[0-9]{4}$", master$in_dsn)
    ))
  } else {
    integer(0)
  }

  next_id <- if (length(existing_nums) > 0) max(existing_nums) + 1L else 1L

  entry$in_dsn <- vapply(seq_len(nrow(entry)), function(i) {
    sprintf("in_%s_%s_%04d", entry$iso3[i], freq_code, next_id + i - 1L)
  }, character(1))

  if (any(entry$in_dsn %in% master$in_dsn)) {
    stop(
      "Duplicate in_dsn detected — please inspect the mastersheet.",
      call. = FALSE
    )
  }

  # --- 8. Enforce column order and bind -------------------------------------
  col_order <- c(
    "in_dsn", "frequency", "country", "iso3",
    "continent", "un_region", "un_subregion",
    "original_name", "coordinator", "coordinator_email",
    "notes", "confidential", "citation", "acknowledgement",
    "parent_in_dsn"
  )
  entry  <- entry[, col_order, drop = FALSE]
  master <- master[, col_order, drop = FALSE]
  master <- dplyr::bind_rows(master, entry)

  # --- 9. Write to depot ----------------------------------------------------
  arrow::write_parquet(master, mastersheet_path)
  message(sprintf(
    "%s mastersheet updated: %d new entry/entries added.",
    tools::toTitleCase(type), nrow(entry)
  ))

  # --- 10. Optionally copy to working directory -----------------------------
  if (download) {
    local_name <- sprintf(
      "in_%s_mastersheet_%s.parquet",
      type, format(Sys.Date(), "%Y%m%d")
    )
    local_path <- file.path(getwd(), local_name)
    file.copy(mastersheet_path, local_path, overwrite = TRUE)
    message(sprintf("Local copy saved: %s", local_path))
  }

  invisible(master)
}
