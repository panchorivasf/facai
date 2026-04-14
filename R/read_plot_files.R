#' Import multiple delimited text files into a single table
#'
#' @description
#' Reads all matching text files in a directory into a single data frame,
#' adding the file name (without extension) as a \code{plot_name} column.
#'
#' @param dir Character. Directory containing the files. Defaults to the
#'   current working directory (\code{"."}).
#' @param pattern Character. Regex pattern to match files. Defaults to
#'   \code{"\\\\.txt$"}.
#' @param delim Character. Field delimiter passed to \code{readr::read_delim}.
#'   Defaults to \code{"\t"} (tab). Use \code{","} for CSV, \code{" "} for
#'   space-separated, etc.
#' @param ... Additional arguments passed to \code{readr::read_delim} (e.g.
#'   \code{skip}, \code{col_types}, \code{locale}).
#'
#' @return A tibble with all files row-bound and a \code{plot_name} column
#'   containing the file name without extension.
#'
#' @importFrom readr read_delim
#' @importFrom purrr map list_rbind
#' @importFrom dplyr mutate
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' \dontrun{
#' # Tab-delimited files in working directory
#' dat <- read_plot_files()
#'
#' # CSV files in a subdirectory
#' dat <- read_plot_files(dir = "raw/congo", pattern = "\\.csv$", delim = ",")
#'
#' # Pass extra arguments to read_delim
#' dat <- read_plot_files(skip = 1, col_types = cols(.default = "c"))
#' }
#'
#' @export
read_plot_files <- function(dir = ".", pattern = "\\.txt$", delim = "\t",
                            col_types = readr::cols(.default = "c"), ...) {

  files <- list.files(dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("No files matching '", pattern, "' found in '", dir, "'.",
         call. = FALSE)
  }

  message(sprintf("Reading %d file(s) from '%s'.", length(files), dir))

  purrr::map(files, \(f) {
    readr::read_delim(f, delim = delim, show_col_types = FALSE,
                      col_types = col_types, ...) |>
      dplyr::mutate(
        plot_name = tools::file_path_sans_ext(basename(f)),
        .before = 1
      )
  }) |>
    purrr::list_rbind()
}
