#' Abbreviate a Species Binomial
#'
#' Produces a compact abbreviation from a Latin species name by taking the
#' first 3 letters of the genus (title-cased) and the first 3 letters of the
#' species epithet (lowercased). Useful for constructing \code{TreeID_GFB3}
#' identifiers. Tokens shorter than 2 characters (e.g. \code{"sp."}, \code{"cf."})
#' are skipped. Genus-only names return a 3-letter code with no species suffix.
#'
#' @param name Character. A Latin species name, e.g. \code{"Coula edulis"}.
#'   \code{NA} and blank strings return \code{NA_character_}.
#'
#' @return A character string of 3--6 characters, e.g. \code{"Couedu"} for
#'   \code{"Coula edulis"} or \code{"Cou"} for \code{"Coula sp."}.
#'   Returns \code{NA_character_} for missing or empty input.
#'
#' @examples
#' abb_binomial("Coula edulis")       # "Couedu"
#' abb_binomial("Phyllanthus diandrus") # "Phyian"
#' abb_binomial("Coula sp.")          # "Cou"
#' abb_binomial(NA)                   # NA
#' abb_binomial("")                   # NA
#'
#' @importFrom stringr str_extract_all str_to_title
#' @export
abb_binomial <- function(name) {
  vapply(name, function(n) {
    if (is.na(n) || !nzchar(trimws(n))) return(NA_character_)
    parts <- stringr::str_extract_all(n, "[A-Za-z]{2,}")[[1]]
    if (length(parts) == 0) return(NA_character_)
    genus <- stringr::str_to_title(substr(parts[1], 1, 3))
    sp    <- if (length(parts) >= 2) tolower(substr(parts[2], 1, 3)) else ""
    paste0(genus, sp)
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}
