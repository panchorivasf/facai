#' Export a table as Parquet, CSV, and Excel files
#'
#' @param data A data frame or tibble to export.
#' @param name A character string used as the dataset identifier in the filename
#'   (e.g., `"honduras"`). Output files will be named
#'   `"<prefix><name>[_YYYYMMDD].<ext>"`.
#' @param dir Output directory. Defaults to the working directory (`"."`).
#' @param prefix File name prefix. Defaults to `"in_"`.
#' @param date_suffix Logical. If \code{TRUE}, appends the current date as
#'   \code{_YYYYMMDD} to the file stem before the extension. Defaults to
#'   \code{FALSE}.
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
#' export_data_multi(honduras_clean, "honduras", dir = "output/gfb3", dry_run = TRUE)
#' }
export_data_multi <- function(data, name, dir = ".", prefix = "in_",
                              date_suffix = FALSE, dry_run = FALSE) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(name), length(name) == 1, nzchar(name))

  stem <- paste0(prefix, name)
  if (date_suffix) stem <- paste0(stem, "_", format(Sys.Date(), "%Y%m%d"))

  paths <- c(
    parquet = file.path(dir, paste0(stem, ".parquet")),
    csv     = file.path(dir, paste0(stem, ".csv")),
    xlsx    = file.path(dir, paste0(stem, ".xlsx"))
  )

  if (dry_run) {
    message("[Dry run] Files that would be written:\n",
            paste0("  ", paths, collapse = "\n"))
    return(invisible(paths))
  }

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  arrow::write_parquet(data,  paths[["parquet"]])
  readr::write_csv(data,      paths[["csv"]])
  openxlsx::write.xlsx(data,  paths[["xlsx"]])

  message("Exported:\n", paste0("  ", paths, collapse = "\n"))
  invisible(paths)
}
