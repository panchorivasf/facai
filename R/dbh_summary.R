#' Summarize DBH distribution of a forest plot data frame
#'
#' @description
#' Computes descriptive statistics for the diameter at breast height (DBH)
#' column of a forest inventory data frame, including minimum, first quartile,
#' median, mean, third quartile, maximum, and standard deviation.
#'
#' @param df A data frame containing a numeric column named \code{DBH}.
#' @param export Logical. If \code{TRUE}, exports the summary table to an Excel
#'   file. Defaults to \code{FALSE}.
#' @param file_name Character. Name of the output file without extension.
#'   Defaults to \code{"dbh_summary"}.
#' @param dir Character. Output directory. Defaults to the current working
#'   directory (\code{"."}).
#'
#' @return A one-row data frame with the following columns:
#' \describe{
#'   \item{min}{Minimum DBH value.}
#'   \item{q1}{First quartile (25th percentile), rounded to 2 decimal places.}
#'   \item{median}{Median DBH, rounded to 2 decimal places.}
#'   \item{mean}{Mean DBH, rounded to 2 decimal places.}
#'   \item{q3}{Third quartile (75th percentile), rounded to 2 decimal places.}
#'   \item{max}{Maximum DBH value.}
#'   \item{SD}{Standard deviation of DBH, rounded to 2 decimal places.}
#' }
#'
#' @importFrom dplyr rename mutate across
#'
#' @examples
#' \dontrun{
#' dbh_summary(honduras_gfb3_clean)
#' dbh_summary(ituri_gfb3, export = TRUE, file_name = "ituri_dbh", dir = "output")
#' }
#'
#' @export
dbh_summary <- function(df,
                        main = "DBH Distribution",
                        xlab   = "DBH (cm)",
                        ylab   = "Frequency",
                        export = FALSE,
                        file_name = "dbh_summary",
                        dir = ".") {
  x <- as.numeric(df$DBH)

  histogram <-  hist(x,
                     main   = main,
                     xlab   = xlab,
                     ylab   = ylab,
                     breaks = 100)

  s <- c(summary(x), sd = sd(x, na.rm = TRUE))
  result <- as.data.frame(t(as.matrix(s))) |>
    dplyr::rename(min = `Min.`, q1 = `1st Qu.`, median = Median,
                  mean = Mean, q3 = `3rd Qu.`, max = `Max.`) |>
    dplyr::mutate(dplyr::across(-c(min, max), \(x) round(x, 2)))

  result <- result |>
    mutate(min = round(min, 2),
           q1 = round(q1, 2),
           median = round(median,2),
           mean = round(mean,2),
           q3 = round(q3,2),
           max = round(max,2))

  print(result)

  if (export) {
    if (!requireNamespace("openxlsx", quietly = TRUE))
      stop("Package 'openxlsx' is required for export. Please install it.",
           call. = FALSE)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    out_path <- file.path(dir, paste0(file_name, ".xlsx"))
    openxlsx::write.xlsx(result, out_path)
    message(sprintf("Summary exported to: %s", out_path))
  }

  invisible(result)
}
