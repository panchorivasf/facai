#' Look up country codes by name
#'
#' Searches the countrycode package codelist for countries matching a name
#' pattern, returning UN and ISO codes for use in GFB3 contributor metadata.
#'
#' @param country Character. A search string matched against country names
#'   (case-insensitive partial match). Examples: \code{"congo"}, \code{"france"}.
#'
#' @return A tibble with columns \code{country.name.en}, \code{un.name.en},
#'   \code{iso2c}, \code{iso3c}, and \code{un} (UN M.49 numeric code).
#'
#' @examples
#' get_country_code("congo")
#' get_country_code("guiana")
#'
#' @importFrom dplyr select filter
#' @export
get_country_code <- function(country){
  countrycode::codelist |>
    select(country.name.en, un.name.en, iso2c, iso3c, un) |>
    filter(grepl(country, country.name.en, ignore.case = TRUE))
}
