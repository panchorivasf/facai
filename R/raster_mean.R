#' Calculate Mean Raster from All Rasters in a Folder
#'
#' @description
#' Takes all `.tif` raster files in a specified folder and computes their
#' pixel-wise mean, saving the result to an output file.
#'
#' @param input_dir Character. Path to the folder containing `.tif` raster files.
#' @param output_file Character. Full path (including filename and `.tif` extension)
#'   for the output mean raster. If \code{NULL}, the file is saved as
#'   \code{"mean.tif"} inside \code{input_dir}.
#' @param na.rm Logical. Whether to ignore \code{NA} values when computing the
#'   mean. Default is \code{TRUE}.
#' @param datatype Character. Data type for the output raster passed to
#'   \code{\link[terra]{writeRaster}}. Default is \code{"FLT4S"}
#'   (32-bit signed float).
#' @param overwrite Logical. Whether to overwrite \code{output_file} if it
#'   already exists. Default is \code{TRUE}.
#'
#' @return Invisibly returns a named list with components:
#' \describe{
#'   \item{output_file}{Character. Full path to the saved mean raster.}
#'   \item{n_rasters}{Integer. Number of raster files averaged.}
#'   \item{input_files}{Character vector. Paths of all input files used.}
#'   \item{mean_raster}{A \code{\link[terra]{SpatRaster}} object of the result.}
#' }
#'
#' @details
#' All rasters in \code{input_dir} must share the same extent, resolution, and
#' coordinate reference system, as they are stacked via \code{\link[terra]{rast}}
#' before computing the mean with \code{\link[terra]{mean}}.
#'
#' @examples
#' \dontrun{
#' result <- raster_mean(
#'   input_dir   = "data/climate/ssp126",
#'   output_file = "data/climate/ssp126_mean.tif"
#' )
#' }
#'
#' @importFrom terra rast mean writeRaster
#' @export
raster_mean <- function(input_dir,
                        output_file = NULL,
                        na.rm       = TRUE,
                        datatype    = "FLT4S",
                        overwrite   = TRUE) {

  library(terra)

  # --- Validate inputs -------------------------------------------------------
  if (!dir.exists(input_dir)) {
    stop("Folder does not exist: ", input_dir)
  }

  tif_files <- list.files(input_dir, pattern = "\\.tif$",
                          full.names = TRUE)

  if (length(tif_files) == 0) {
    stop("No .tif files found in: ", input_dir)
  }

  message("Found ", length(tif_files), " raster(s) in: ", input_dir)

  # --- Resolve output path ---------------------------------------------------
  if (is.null(output_file)) {
    output_file <- file.path(input_dir, "mean.tif")
  }

  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    message("Created output directory: ", out_dir)
  }

  # --- Stack and average -----------------------------------------------------
  message("Stacking rasters...")
  rast_stack <- rast(tif_files)

  message("Computing pixel-wise mean (na.rm = ", na.rm, ")...")
  mean_rast <- mean(rast_stack, na.rm = na.rm)

  # --- Write output ----------------------------------------------------------
  writeRaster(mean_rast, output_file, overwrite = overwrite,
              wopt = list(datatype = datatype))

  message("Mean raster saved to: ", output_file)

  # --- Return ----------------------------------------------------------------
  result <- list(
    output_file  = output_file,
    n_rasters    = length(tif_files),
    input_files  = tif_files,
    mean_raster  = mean_rast
  )

  return(invisible(result))
}
