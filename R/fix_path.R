#' Fix file path separators from clipboard
#'
#' Reads a file path from the clipboard, replaces all backslashes with forward
#' slashes, and writes the result back to the clipboard.
#'
#' @return A character string with the corrected file path (invisibly).
#'
#' @note This function uses \code{readClipboard()} and \code{writeClipboard()},
#'   which are only available on Windows.
#'
#' @examples
#' \dontrun{
#' # With "C:\Users\fran\Documents" in clipboard:
#' fix_path()
#' # Clipboard now contains "C:/Users/fran/Documents"
#' }
#'
#' @export
fix_path <- function() {

  path <- readClipboard()
  fixed <- gsub("\\\\", "/", path)
  writeClipboard(fixed)
  return(fixed)

  }
