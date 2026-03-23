#' Add Raster Covariates to Point or Grid Locations
#'
#' Extracts values from a set of raster files (e.g., bioclimatic variables)
#' at spatial point locations provided either as a data frame or a CSV/Parquet
#' file path. Works with any point data — forest inventory plots, mapping grid
#' cells, or arbitrary coordinate tables — as long as longitude and latitude
#' columns are present.
#'
#' @param points_df A data frame containing at minimum coordinate columns. If
#'   \code{NULL}, \code{points_file} must be provided. Default: \code{NULL}.
#' @param points_file Character. Path to a CSV or Parquet file to load if
#'   \code{points_df} is \code{NULL}. Default: \code{NULL}.
#' @param cov_raster Character. Path to a directory containing \code{.tif}
#'   raster files to extract. All \code{.tif} files in the directory are
#'   loaded; see \code{n_vars} to control how many are used.
#' @param output_file Character or \code{NULL}. Base path (without extension)
#'   for output files. Extensions are added automatically depending on
#'   \code{write_parq} and \code{write_csv}. If \code{NULL}, no file is
#'   written. Default: \code{NULL}.
#' @param write_parq Logical. Whether to write output as a Parquet file
#'   (requires the \pkg{arrow} package). Default: \code{TRUE}.
#' @param write_csv Logical. Whether to write output as a CSV file (via
#'   \pkg{data.table}). Default: \code{TRUE}.
#' @param coord_cols Character vector of length 2 giving the names of the
#'   longitude and latitude columns, in that order. Default:
#'   \code{c("LON", "LAT")}.
#' @param keep_original_cols Logical. If \code{TRUE} (default), all columns
#'   from the input data frame are retained in the output and the extracted
#'   covariate columns are appended (or used to overwrite existing columns with
#'   the same names). If \code{FALSE}, only the coordinate columns and the
#'   extracted covariates are returned.
#' @param var_prefix Character. Prefix for the names of the extracted covariate
#'   columns (e.g., \code{"C"} produces \code{C1}, \code{C2}, ...).
#'   Default: \code{"C"}.
#' @param n_vars Integer or \code{NULL}. Number of raster files to use, taken
#'   in sorted order. If \code{NULL}, all discovered \code{.tif} files are
#'   used. Useful when the directory contains exactly 19 BioClim layers and you
#'   want to enforce that count. Default: \code{NULL}.
#' @param crs Character. Coordinate reference system of the input points,
#'   passed to \code{terra::vect()}. Must match the CRS of the raster files.
#'   Default: \code{"EPSG:4326"} (WGS84 geographic coordinates).
#' @param drop_na_coords Logical. If \code{TRUE} (default), rows with
#'   \code{NA} in either coordinate column are silently removed before
#'   extraction and a warning is issued reporting how many were dropped. If
#'   \code{FALSE}, the function stops with an error when \code{NA} coordinates
#'   are found.
#' @param verbose Logical. If \code{TRUE} (default), progress messages and a
#'   summary of extracted variable ranges are printed to the console.
#'
#' @return A data frame (invisibly) with the extracted covariate columns
#'   appended. Rows with \code{NA} coordinates are excluded when
#'   \code{drop_na_coords = TRUE}.
#'
#' @details
#' Raster files are sorted by the numeric suffix extracted from their base
#' filename using the pattern \code{"(?<=\\D)(\\d+)(?=\\.tif$)"}, which
#' targets only the trailing number immediately before the \code{.tif}
#' extension. This handles common naming conventions such as \code{bio_1.tif},
#' \code{BIO01.tif}, or \code{chelsa_bio1_2071.tif} correctly.
#'
#' Extraction uses \code{terra::extract()} with the default method (nearest
#' cell, equivalent to \code{method = "simple"} in the older \pkg{raster}
#' package). No spatial interpolation or downscaling is performed: each point
#' receives the value of the raster cell whose centre is closest to it.
#'
#' When \code{keep_original_cols = TRUE} and the input already contains
#' columns named \code{var_prefix1}, \code{var_prefix2}, etc., those columns
#' are overwritten with the newly extracted values. This makes the function
#' suitable for replacing static climate covariates with future projections
#' while preserving all other plot attributes.
#'
#' @examples
#' \dontrun{
#' # Minimal usage — plot locations only
#' plots <- data.frame(PlotID = 1:5,
#'                     LON = c(-70, -75, -80, -85, -90),
#'                     LAT = c(10, 20, 30, 40, 50))
#' result <- add_covariates(points_df = plots,
#'                          cov_raster = "path/to/bioclim/tifs/",
#'                          n_vars = 19)
#'
#' # Grid usage, writing both Parquet and CSV output
#' result <- add_covariates(points_file  = "map_grid_3km.csv",
#'                          cov_raster   = "path/to/future/bioclim/",
#'                          output_file  = "map_grid_3km_fut",
#'                          n_vars       = 19,
#'                          var_prefix   = "C",
#'                          keep_original_cols = TRUE)
#' }
#'
#' @importFrom data.table fread fwrite
#' @importFrom terra rast vect extract
#' @export
add_covariates <- function(points_df          = NULL,
                           points_file        = NULL,
                           cov_raster,
                           output_file        = NULL,
                           write_parq         = TRUE,
                           write_csv          = TRUE,
                           coord_cols         = c("LON", "LAT"),
                           keep_original_cols = TRUE,
                           var_prefix         = "C",
                           n_vars             = NULL,
                           crs                = "EPSG:4326",
                           drop_na_coords     = TRUE,
                           verbose            = TRUE) {

  # ── 0. Package availability checks ─────────────────────────────────────────
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required. Install it with install.packages('terra').")
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required. Install it with install.packages('data.table').")
  if (write_parq && !requireNamespace("arrow", quietly = TRUE))
    stop("Package 'arrow' is required for Parquet output. ",
         "Install it with install.packages('arrow'), or set write_parq = FALSE.")

  # ── 1. Load point data ──────────────────────────────────────────────────────
  if (is.null(points_df)) {
    if (is.null(points_file))
      stop("Provide either 'points_df' or 'points_file'.")
    if (!file.exists(points_file))
      stop("File not found: ", points_file)
    if (verbose) message("Loading point data from: ", points_file)
    ext <- tools::file_ext(points_file)
    if (ext == "parquet") {
      points_df <- as.data.frame(arrow::read_parquet(points_file))
    } else {
      points_df <- as.data.frame(data.table::fread(points_file,
                                                   stringsAsFactors = FALSE))
    }
  }

  if (verbose)
    message("Input: ", nrow(points_df), " rows, ", ncol(points_df), " columns")

  # ── 2. Validate and extract coordinates ────────────────────────────────────
  missing_cols <- setdiff(coord_cols, names(points_df))
  if (length(missing_cols) > 0)
    stop("Coordinate column(s) not found in input: ",
         paste(missing_cols, collapse = ", "))

  coords <- data.frame(x = points_df[[coord_cols[1]]],
                       y = points_df[[coord_cols[2]]])

  na_rows <- !stats::complete.cases(coords)
  n_na    <- sum(na_rows)

  if (n_na > 0) {
    if (!drop_na_coords)
      stop(n_na, " row(s) have NA coordinates. ",
           "Set drop_na_coords = TRUE to remove them automatically.")
    warning(n_na, " row(s) with NA coordinates were removed.")
  }

  coords_clean <- coords[!na_rows, ]
  df_clean     <- points_df[!na_rows, ]

  if (verbose)
    message(nrow(coords_clean), " valid coordinate rows")

  # ── 3. Discover and load rasters ───────────────────────────────────────────
  if (!dir.exists(cov_raster))
    stop("Raster directory not found: ", cov_raster)

  tif_files <- list.files(cov_raster, pattern = "\\.tif$", full.names = TRUE)

  if (length(tif_files) == 0)
    stop("No .tif files found in: ", cov_raster)

  # Sort by the trailing numeric suffix immediately before .tif
  trailing_nums <- suppressWarnings(
    as.numeric(regmatches(basename(tif_files),
                          regexpr("(?<=bio)(\\d+)",
                                  basename(tif_files), perl = TRUE)))
  )
  tif_files <- tif_files[order(trailing_nums)]

  if (!is.null(n_vars)) {
    if (length(tif_files) < n_vars)
      stop("Expected ", n_vars, " raster files, found only ", length(tif_files))
    tif_files <- tif_files[seq_len(n_vars)]
  }

  n_rasters  <- length(tif_files)
  var_names  <- paste0(var_prefix, seq_len(n_rasters))

  if (verbose)
    message("Loading ", n_rasters, " raster(s) from: ", cov_raster)

  cov_stack <- terra::rast(tif_files)
  names(cov_stack) <- var_names

  # ── 4. Extract raster values at point locations ────────────────────────────
  if (verbose) message("Extracting covariate values...")

  pts      <- terra::vect(coords_clean, geom = c("x", "y"), crs = crs)
  ext_vals <- terra::extract(cov_stack, pts)[, -1, drop = FALSE]
  ext_vals <- as.data.frame(ext_vals)
  names(ext_vals) <- var_names

  # ── 5. Assemble output data frame ──────────────────────────────────────────
  if (keep_original_cols) {
    out <- df_clean
    out[, var_names] <- ext_vals      # overwrites existing columns if present
  } else {
    out <- cbind(df_clean[, coord_cols, drop = FALSE], ext_vals)
  }

  # ── 6. Write output ────────────────────────────────────────────────────────
  if (!is.null(output_file)) {
    if (write_parq) {
      parq_path <- paste0(output_file, ".parquet")
      if (verbose) message("Writing Parquet: ", parq_path)
      arrow::write_parquet(out, parq_path)
    }
    if (write_csv) {
      csv_path <- paste0(output_file, ".csv")
      if (verbose) message("Writing CSV: ", csv_path)
      data.table::fwrite(out, csv_path)
    }
  }

  # ── 7. Summary ─────────────────────────────────────────────────────────────
  if (verbose) {
    message("\nSummary:")
    message("  Output rows    : ", nrow(out))
    message("  Output columns : ", ncol(out))
    message("  Covariates     : ", var_names[1], " to ", var_names[n_rasters])
    message("\nCovariate ranges:")
    rng <- t(sapply(ext_vals, function(x)
      c(min  = round(min(x,  na.rm = TRUE), 3),
        mean = round(mean(x, na.rm = TRUE), 3),
        max  = round(max(x,  na.rm = TRUE), 3),
        NAs  = sum(is.na(x)))))
    print(rng)
    message("\nDone.")
  }

  invisible(out)
}
