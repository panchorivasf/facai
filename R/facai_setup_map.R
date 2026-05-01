#' Set up map export dependencies for \code{gfb3_report()}
#'
#' Installs the R packages (\pkg{leaflet}, \pkg{mapview}, \pkg{webshot2}) and
#' the headless Chromium browser required to produce Leaflet-based map
#' screenshots in \code{\link{gfb3_report}()} when \code{export_pdf = TRUE} or
#' \code{export_xlsx = TRUE} and a \code{plot_metadata} table is supplied.
#'
#' These dependencies are optional — \code{gfb3_report()} will fall back to a
#' simpler static map if they are absent — but the Leaflet screenshot produces
#' a much better result. Run this function once after installing \pkg{facai}.
#'
#' @param chromium Logical. If \code{TRUE} (default), also installs the headless
#'   Chromium browser via \code{webshot2::install_chromium()} after the R
#'   packages are installed. Set to \code{FALSE} if Chromium is already present
#'   on the system or if you are working in an environment where the download is
#'   blocked (e.g., an HPC node without internet access).
#' @param force Logical. If \code{FALSE} (default), packages that are already
#'   installed are skipped. Set to \code{TRUE} to reinstall regardless.
#'
#' @return Invisibly returns a named logical vector indicating which components
#'   were already installed (\code{TRUE}) or newly installed (\code{FALSE}) at
#'   the start of the call.
#'
#' @examples
#' \dontrun{
#' # Typical first-time setup
#' facai_setup_map()
#'
#' # Skip Chromium (e.g., already installed, or offline HPC environment)
#' facai_setup_map(chromium = FALSE)
#' }
#'
#' @importFrom cli cli_h1 cli_h2 cli_alert_success cli_alert_info
#'   cli_alert_warning cli_bullets
#' @export
facai_setup_map <- function(chromium = TRUE, force = FALSE) {

  pkgs <- c("leaflet", "mapview", "webshot2")

  already <- vapply(pkgs, \(p) requireNamespace(p, quietly = TRUE),
                    logical(1))

  cli::cli_h1("facai map export setup")

  # ── R packages ───────────────────────────────────────────────────────────────
  cli::cli_h2("R packages")

  to_install <- if (force) pkgs else pkgs[!already]

  if (length(to_install) == 0L) {
    cli::cli_alert_success(
      "All required packages are already installed: {paste(pkgs, collapse = ', ')}."
    )
  } else {
    if (!force && any(already)) {
      cli::cli_alert_success(
        "Already installed: {paste(pkgs[already], collapse = ', ')}."
      )
    }
    cli::cli_alert_info(
      "Installing: {paste(to_install, collapse = ', ')} ..."
    )
    utils::install.packages(to_install, quiet = TRUE)

    # Verify
    ok <- vapply(to_install, \(p) requireNamespace(p, quietly = TRUE),
                 logical(1))
    if (all(ok)) {
      cli::cli_alert_success("Successfully installed: {paste(to_install, collapse = ', ')}.")
    } else {
      failed <- to_install[!ok]
      cli::cli_alert_warning(
        "The following package(s) could not be installed: {paste(failed, collapse = ', ')}. \\
        Check your internet connection or CRAN mirror and try again."
      )
    }
  }

  # ── Chromium ─────────────────────────────────────────────────────────────────
  cli::cli_h2("Headless Chromium")

  if (!chromium) {
    cli::cli_alert_info(
      "Chromium installation skipped (chromium = FALSE). \\
      Make sure a compatible browser is available for webshot2."
    )
    return(invisible(already))
  }

  if (!requireNamespace("webshot2", quietly = TRUE)) {
    cli::cli_alert_warning(
      "webshot2 is not installed; cannot install Chromium. \\
      Re-run {.fn facai_setup_map} after resolving the package installation above."
    )
    return(invisible(already))
  }

  # Check whether Chromium is already available
  chromium_present <- tryCatch({
    # webshot2 exposes chromote::find_chrome() internally; we probe it
    chrome_path <- chromote::find_chrome()
    !is.null(chrome_path) && nzchar(chrome_path)
  }, error = function(e) FALSE)

  if (chromium_present && !force) {
    cli::cli_alert_success("Chromium is already installed.")
  } else {
    cli::cli_alert_info("Downloading and installing Chromium ...")
    tryCatch(
      webshot2::install_chromium(),
      error = function(e) {
        cli::cli_alert_warning(
          "Chromium installation failed: {conditionMessage(e)}. \\
          You can try again with {.code webshot2::install_chromium()} or \\
          set {.code chromium = FALSE} and install a system browser manually."
        )
      }
    )
    chromium_present2 <- tryCatch({
      chrome_path <- chromote::find_chrome()
      !is.null(chrome_path) && nzchar(chrome_path)
    }, error = function(e) FALSE)

    if (chromium_present2) {
      cli::cli_alert_success("Chromium installed successfully.")
    }
  }

  # ── Summary ──────────────────────────────────────────────────────────────────
  cli::cli_h2("Next steps")
  cli::cli_bullets(c(
    "i" = "Pass a {.code plot_metadata} table from {.fn make_plot_metadata} to \\
           {.fn gfb3_report} together with {.code export_pdf = TRUE}.",
    "i" = "The map will be rendered automatically — no further setup needed.",
    "i" = "On HPC nodes without internet access, set {.code chromium = FALSE} \\
           and ensure a system Chromium or Chrome binary is on PATH."
  ))

  invisible(already)
}
