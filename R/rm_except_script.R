#' Remove objects not defined in a script from the R environment
#'
#' @description
#' Parses a script file for assignment expressions (\code{<-}) and removes all
#' objects from the specified environment whose names do \emph{not} appear in
#' the script. Useful for isolating only the outputs of a given script while
#' clearing unrelated objects. Pair with \code{\link{this_script}} to
#' self-reference the active script.
#'
#' @param script_path Character. Path to the R script file to parse.
#' @param envir Environment from which to remove objects. Defaults to
#'   \code{.GlobalEnv}.
#'
#' @return Invisibly returns a character vector of the removed object names.
#'
#' @note Only detects assignments made with \code{<-}. Objects created with
#'   \code{=} or \code{assign()} will not be detected and will be treated as
#'   non-script objects, making them candidates for removal.
#'
#' @seealso \code{\link{rm_from_script}}, \code{\link{this_script}}
#'
#' @examples
#' \dontrun{
#' rm_except_script("scripts/ituri_formatting.R")
#' rm_except_script(this_script())
#' }
#'
#' @export
rm_except_script <- function(script_path, envir = .GlobalEnv) {
  code    <- readLines(script_path)
  matches <- regmatches(code, gregexpr("\\b[a-zA-Z_.][a-zA-Z0-9_.]*(?=\\s*<-)",
                                       code, perl = TRUE))
  keep    <- unique(unlist(matches))
  all_obj <- ls(envir = envir)
  to_rm   <- setdiff(all_obj, keep)
  rm(list = to_rm, envir = envir)
  message(sprintf("Removed %d object(s): %s", length(to_rm),
                  paste(to_rm, collapse = ", ")))
  invisible(to_rm)
}
