#' Add ISRIC-WISE30sec Soil Covariates to Plot or Grid Locations
#'
#' Extracts five soil properties from the ISRIC WISE30sec v1 dataset at point
#' locations using a two-step lookup: (1) the integer soil mapping unit value
#' is extracted from the ArcInfo grid raster, (2) that integer is joined to
#' the raster attribute table to retrieve the string NEWSUID code, which is
#' then used to look up soil property values from \code{HW30s_FULL.txt}.
#'
#' @param points_df A data frame containing at minimum coordinate columns.
#' @param wise_dir Character. Path to the root WISE30sec directory — the folder
#'   that contains \code{GISfiles/} and \code{Interchangeable_format/}.
#' @param coord_cols Character vector of length 2 giving the longitude and
#'   latitude column names, in that order. Default: \code{c("LON", "LAT")}.
#' @param keep_original_cols Logical. If \code{TRUE} (default), all input
#'   columns are retained and soil columns are appended. If \code{FALSE}, only
#'   the coordinate columns and soil variables are returned.
#' @param crs Character. CRS of input coordinates. Default: \code{"EPSG:4326"}
#'   (WGS84 geographic coordinates).
#' @param verbose Logical. If \code{TRUE} (default), progress messages and a
#'   summary of extracted soil variable ranges are printed to the console.
#'
#' @return A data frame (invisibly) with five soil columns appended:
#'   \itemize{
#'     \item \code{O1} — Bulk density (kg dm\eqn{^{-3}})
#'     \item \code{O2} — pH in water
#'     \item \code{O3} — Electrical conductivity (dS m\eqn{^{-1}})
#'     \item \code{O4} — C/N ratio
#'     \item \code{O5} — Total nitrogen content (g kg\eqn{^{-1}})
#'   }
#'   Points with no matching soil code receive \code{NA} for all five
#'   variables.
#'
#' @details
#' The WISE30sec dataset stores soil properties in a separate attribute table
#' rather than directly in the raster, requiring a two-step join:
#' \enumerate{
#'   \item The ArcInfo grid (\code{GISfiles/wise30sec_fin}) is used for
#'     extraction because it retains the full attribute table linking integer
#'     raster values to string NEWSUID codes. The GeoTIFF version
#'     (\code{wise_30sec_v1.tif}) loses this attribute table and cannot be
#'     used for the join.
#'   \item The string NEWSUID codes are matched against column 1 of
#'     \code{HW30s_FULL.txt} to retrieve soil properties. Column indices
#'     follow the original MATRIX \code{cov_func2} implementation: bulk
#'     density (col 20), pH (col 44), electrical conductivity (col 50),
#'     C/N ratio (col 28), total nitrogen (col 26).
#' }
#'
#' Soil covariates are static and do not vary between climate scenarios.
#' Extract them once from the historic baseline data frame and carry the
#' O1-O5 columns forward into future scenario data frames rather than
#' re-extracting for each SSP.
#'
#' @examples
#' \dontrun{
#' pine_plots_historic <- add_soil_covars(
#'   points_df  = pine_plots_historic,
#'   wise_dir   = "Z:/data/Resources/Covariates/Soil_WISE30sec",
#'   coord_cols = c("Longitude", "Latitude")
#' )
#'
#' # Carry soil columns forward to future scenarios
#' pine_plots_ssp126_2011_2040[, c("O1","O2","O3","O4","O5")] <-
#'   pine_plots_historic[, c("O1","O2","O3","O4","O5")]
#' }
#'
#' @importFrom terra rast vect extract cats
#' @export
add_soil_covars <- function(points_df,
                                wise_dir,
                                coord_cols         = c("LON", "LAT"),
                                keep_original_cols = TRUE,
                                crs                = "EPSG:4326",
                                verbose            = TRUE) {

  # ── 0. Package check ───────────────────────────────────────────────────────
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required. Install with install.packages('terra').")

  # ── 1. Validate paths ──────────────────────────────────────────────────────
  adf_path   <- file.path(wise_dir, "GISfiles", "wise30sec_fin")
  table_path <- file.path(wise_dir, "Interchangeable_format", "HW30s_FULL.txt")

  if (!dir.exists(adf_path))
    stop("WISE ArcInfo grid not found: ", adf_path)
  if (!file.exists(table_path))
    stop("WISE attribute table not found: ", table_path)

  # ── 2. Validate coordinates ────────────────────────────────────────────────
  missing_cols <- setdiff(coord_cols, names(points_df))
  if (length(missing_cols) > 0)
    stop("Coordinate column(s) not found: ",
         paste(missing_cols, collapse = ", "))

  coords <- data.frame(x = points_df[[coord_cols[1]]],
                       y = points_df[[coord_cols[2]]])
  na_rows <- !stats::complete.cases(coords)
  if (any(na_rows))
    warning(sum(na_rows),
            " row(s) with NA coordinates — soil values will be NA.")

  # ── 3. Extract integer mapping unit values from ArcInfo grid ──────────────
  if (verbose) message("Loading WISE30sec raster...")
  wise_adf  <- terra::rast(adf_path)

  pts       <- terra::vect(coords, geom = c("x", "y"), crs = crs)
  extracted <- terra::extract(wise_adf, pts)
  # Column 2 contains the integer VALUE (MU_GLOBAL)
  newsuid_int <- extracted[, 2]

  # ── 4. Two-step join: integer -> NEWSUID string -> soil properties ─────────
  if (verbose) message("Loading WISE30sec attribute table...")

  # Step 1: integer VALUE -> string NEWSUID via raster attribute table
  cat_table   <- terra::cats(wise_adf)[[1]]
  newsuid_str <- cat_table$NEWSUID[match(newsuid_int, cat_table$VALUE)]

  # Step 2: string NEWSUID -> soil properties via HW30s_FULL.txt
  soil_table <- read.table(table_path, sep = ",", header = TRUE)
  match_idx  <- match(newsuid_str, soil_table[, 1])

  # Column indices follow original cov_func2 implementation
  O1 <- soil_table[match_idx, 20]   # Bulk density
  O2 <- soil_table[match_idx, 44]   # pH in water
  O3 <- soil_table[match_idx, 50]   # Electrical conductivity
  O4 <- soil_table[match_idx, 28]   # C/N ratio
  O5 <- soil_table[match_idx, 26]   # Total nitrogen

  n_na <- sum(is.na(O1))
  if (n_na > 0 && verbose)
    message(n_na, " point(s) had no matching soil code — O1-O5 set to NA.")

  # ── 5. Assemble output ────────────────────────────────────────────────────
  soil_df <- data.frame(O1 = O1, O2 = O2, O3 = O3, O4 = O4, O5 = O5)

  if (keep_original_cols) {
    out <- points_df
    out[, c("O1","O2","O3","O4","O5")] <- soil_df
  } else {
    out <- cbind(points_df[, coord_cols, drop = FALSE], soil_df)
  }

  # ── 6. Summary ────────────────────────────────────────────────────────────
  if (verbose) {
    message("\nSoil variable ranges:")
    rng <- t(sapply(soil_df, function(x)
      c(min  = round(min(x,  na.rm = TRUE), 3),
        mean = round(mean(x, na.rm = TRUE), 3),
        max  = round(max(x,  na.rm = TRUE), 3),
        NAs  = sum(is.na(x)))))
    print(rng)
    message("\nDone.")
  }

  invisible(out)
}
