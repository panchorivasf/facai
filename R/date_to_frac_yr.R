#' Convert a Date to Fractional Year
#'
#' Converts a \code{Date} or date-coercible object to a numeric fractional year
#' (e.g. 2012-07-01 -> 2012.498), accounting for leap years. Useful for
#' constructing the \code{YR} and \code{PrevYR} columns required by the GFB3
#' format.
#'
#' @param d A \code{Date} vector, or a character vector coercible to \code{Date}
#'   via \code{as.Date()}.
#'
#' @return A numeric vector of fractional years the same length as \code{d}.
#'   \code{NA} inputs return \code{NA_real_}.
#'
#' @examples
#' date_to_frac_yr(as.Date("2012-07-01"))  # 2012.498
#' date_to_frac_yr(as.Date("2000-01-01"))  # 2000.000 (leap year)
#' date_to_frac_yr(c(as.Date("2010-03-15"), NA))
#'
#' @export
date_to_frac_yr <- function(d) {
  d <- as.Date(d)
  year        <- as.integer(format(d, "%Y"))
  day_of_year <- as.integer(format(d, "%j"))
  days_in_year <- ifelse(
    (year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0, 366L, 365L
  )
  year + (day_of_year - 1) / days_in_year
}
