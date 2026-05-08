#' Export a table as Parquet, CSV, and Excel files
#'
#' @param data A data frame or tibble to export.
#' @param name A character string used as the dataset identifier in the filename
#'   (e.g., `"honduras"`). Output files will be named
#'   `"<prefix><name>[_YYYYMMDD].<ext>"`.
#' @param output_dir Output directory. Defaults to the working directory (`"."`).
#' @param prefix File name prefix. Defaults to `"in_"`.
#' @param date_suffix Logical. If \code{TRUE}, appends the current date as
#'   \code{_YYYYMMDD} to the file stem before the extension. Defaults to
#'   \code{TRUE}.
#' @param dry_run Logical. If \code{TRUE}, prints the file paths that would be
#'   written without actually writing any files. Defaults to \code{FALSE}.
#'
#' @return Invisibly returns a named character vector of the three output paths.
#' @export
#'
#' @examples
#' \dontrun{
#' export_data_multi(honduras_clean, "honduras")
#' export_data_multi(honduras_clean, "honduras", date_suffix = TRUE)
#' export_data_multi(honduras_clean, "honduras", output_dir = "output/gfb3", dry_run = TRUE)
#' }
export_data_multi <- function(data, name, output_dir = "../exports", prefix = "in_",
                              date_suffix = TRUE, dry_run = FALSE) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(name), length(name) == 1, nzchar(name))

  stem <- paste0(prefix, name)
  if (date_suffix) stem <- paste0(stem, "_", format(Sys.Date(), "%Y%m%d"))

  paths <- c(
    parquet = file.path(output_dir, paste0(stem, ".parquet")),
    csv     = file.path(output_dir, paste0(stem, ".csv")),
    xlsx    = file.path(output_dir, paste0(stem, ".xlsx"))
  )

  if (dry_run) {
    message("[Dry run] Files that would be written:\n",
            paste0("  ", paths, collapse = "\n"))
    return(invisible(paths))
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  arrow::write_parquet(data,  paths[["parquet"]])
  readr::write_csv(data,      paths[["csv"]])
  openxlsx::write.xlsx(data,  paths[["xlsx"]])

  message("Exported:\n", paste0("  ", paths, collapse = "\n"))
  invisible(paths)
}
