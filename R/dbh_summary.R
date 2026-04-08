#' Summarize DBH distribution of a forest plot data frame
#'
#' @description
#' Computes descriptive statistics for the diameter at breast height (DBH)
#' column of a forest inventory data frame, including minimum, first quartile,
#' median, mean, third quartile, maximum, and standard deviation.
#'
#' @param df A data frame containing a numeric column named \code{DBH}.
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
#' dbh_summary(honduras_gfb3_clean)
#'
#' @export
dbh_summary <- function(df) {
  x <- df$DBH
  s <- c(summary(x), sd = sd(x, na.rm = TRUE))

  result <- as.data.frame(t(as.matrix(s))) |>
    dplyr::rename(min = `Min.`, q1 = `1st Qu.`, median = Median,
                  mean = Mean, q3 = `3rd Qu.`, max = `Max.`) |>
    dplyr::mutate(dplyr::across(-c(min, max), \(x) round(x, 2)))

  result
}
