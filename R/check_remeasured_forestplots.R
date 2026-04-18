#' Check remeasurement status of ForestPlots Excel files
#'
#' Reads a list of ForestPlots-formatted Excel files and returns a tibble
#' indicating whether each plot has been remeasured (i.e., contains more than
#' one diameter census column).
#'
#' @param files Character vector of full file paths to Excel files, typically
#'   from \code{list.files(..., full.names = TRUE)}.
#' @param sheet Numeric or character. Sheet to read from each workbook.
#'   Default is \code{2}.
#' @param startRow Numeric. Row to start reading from. Default is \code{2}.
#'
#' @return A tibble with two columns:
#'   \describe{
#'     \item{plot_name}{Plot name derived from the filename without extension.}
#'     \item{remeasured}{Character. \code{"YES"} if more than one diameter
#'       column (matching \code{^D\\.}) is found, \code{"NO"} otherwise.
#'       \code{NA} if the file could not be read.}
#'   }
#'
#' @details
#' Diameter census columns are identified by the regex \code{^D\\.}, which
#' matches column names of the form \code{D.2008}, \code{D.2013}, etc., as
#' used in the ForestPlots data format. Files that fail to load produce a
#' warning and return \code{NA} for \code{remeasured}.
#'
#' @examples
#' \dontrun{
#' files <- list.files("data/Brazil", pattern = "\\.xlsx$", full.names = TRUE)
#' check_remeasured_forestplots(files)
#' }
#'
#' @importFrom purrr map_dfr
#' @importFrom openxlsx read.xlsx
#' @importFrom tibble tibble
#' @importFrom tools file_path_sans_ext
#'
#' @export
check_remeasured_forestplots <- function(files, sheet = 2, startRow = 2) {

  purrr::map_dfr(files, function(f) {

    plot_name <- tools::file_path_sans_ext(basename(f))

    tryCatch({
      df   <- openxlsx::read.xlsx(f, sheet = sheet, startRow = startRow)
      d_cols <- sum(grepl("^D\\.", names(df)))

      tibble::tibble(
        plot_name  = plot_name,
        remeasured = ifelse(d_cols > 1, "YES", "NO")
      )
    },
    error = function(e) {
      warning("Failed to read '", basename(f), "': ", e$message)
      tibble::tibble(plot_name = plot_name, remeasured = NA_character_)
    })
  })
}
