#' Get the path of the currently active script
#'
#' @description
#' Returns the file path of the script currently open and active in RStudio.
#' Intended for use with \code{\link{rm_from_script}} and
#' \code{\link{rm_except_script}}.
#'
#' @return A character string with the full path to the active script.
#'
#' @note Only works inside RStudio. Will fail if called from a non-RStudio
#'   environment or when a script is run via \code{source()}.
#'
#' @importFrom rstudioapi getSourceEditorContext
#'
#' @examples
#' \dontrun{
#' this_script()
#' rm_from_script(this_script())
#' }
#'
#' @export
this_script <- function() {
  rstudioapi::getSourceEditorContext()$path
}
