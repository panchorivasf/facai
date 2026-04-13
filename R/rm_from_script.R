#' Remove objects defined in a script from the R environment
#'
#' @description
#' Parses a script file for assignment expressions (\code{<-}) and removes all
#' objects with matching names from the specified environment. Useful for
#' cleaning up after a formatting or processing script without manually listing
#' object names. Pair with \code{\link{this_script}} to self-reference the
#' active script.
#'
#' @param script_path Character. Path to the R script file to parse.
#' @param envir Environment from which to remove objects. Defaults to
#'   \code{.GlobalEnv}.
#'
#' @return Invisibly returns a character vector of the removed object names.
#'
#' @note Only detects assignments made with \code{<-}. Objects created with
#'   \code{=} or \code{assign()} will not be detected.
#'
#' @seealso \code{\link{rm_except_script}}, \code{\link{this_script}}
#'
#' @examples
#' \dontrun{
#' rm_from_script("scripts/ituri_formatting.R")
#' rm_from_script(this_script())
#' }
#'
#' @export
rm_from_script <- function(script_path, envir = .GlobalEnv) {
  code    <- readLines(script_path)
  matches <- regmatches(code, gregexpr("\\b[a-zA-Z_.][a-zA-Z0-9_.]*(?=\\s*<-)",
                                       code, perl = TRUE))
  objects <- unique(unlist(matches))
  exists  <- objects[sapply(objects, exists, envir = envir)]
  rm(list = exists, envir = envir)
  message(sprintf("Removed %d object(s): %s", length(exists),
                  paste(exists, collapse = ", ")))
  invisible(exists)
}
