#' Plot Annual Climate Covariate Time Series Across SSP Scenarios
#'
#' Visualizes interpolated annual climate covariates for plot locations across
#' one or more SSP scenarios and one or more BioClim variables. For each
#' variable, individual plot trajectories are shown as faint lines and the
#' scenario mean as a bold line, allowing comparison of both within-scenario
#' spread and between-scenario divergence.
#'
#' @param ... One or more named interpolated data frames produced by
#'   \code{interpolate_covariates()}, passed as \code{name = data frame} pairs
#'   (e.g. \code{SSP126 = pine_ssp126_annual, SSP585 = pine_ssp585_annual}).
#'   Names are used as scenario labels in the legend.
#' @param covariates Character vector of covariate base names to plot (e.g.
#'   \code{c("C1", "C12")}). Each variable produces one panel. Columns in the
#'   data frames must follow the \code{cov_YYYY} naming convention produced by
#'   \code{interpolate_covariates()}.
#' @param id_col Character. Name of the plot identifier column. Default:
#'   \code{"PlotID"}.
#' @param ylab Named character vector mapping covariate names to y-axis labels.
#'   If a covariate is not found in the vector, the covariate name is used as
#'   the label. Example: \code{c(C1 = "Mean Annual Temp (°C x 10)",
#'   C12 = "Annual Precip (mm)")}. Default: \code{NULL}.
#' @param colors Character vector of colors, one per scenario. If \code{NULL}
#'   (default), a colorblind-friendly palette is used automatically.
#' @param alpha_plots Numeric in [0, 1]. Transparency of individual plot
#'   trajectories. Default: \code{0.15}.
#' @param lwd_mean Numeric. Line width of the scenario mean trajectory.
#'   Default: \code{2.5}.
#' @param show_plots Logical. If \code{TRUE} (default), individual plot
#'   trajectories are drawn behind the scenario mean.
#' @param title Character. Overall plot title. Default: \code{NULL} (no title).
#' @param ncol Integer. Number of columns in the panel layout when plotting
#'   multiple covariates. Default: \code{1L}.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its
#'   side effect of producing a plot.
#'
#' @details
#' Columns are detected automatically by matching \code{paste0(cov, "_")} as
#' a prefix, so the function is robust to different year ranges across data
#' frames as long as the naming convention is consistent.
#'
#' When multiple covariates are requested, panels are arranged in a grid using
#' \code{par(mfrow)}. The layout is restored to \code{c(1, 1)} on exit.
#'
#' @examples
#' \dontrun{
#' # Single variable, two scenarios
#' plot_bioclim_timeseries(
#'   SSP126 = pine_ssp126_annual,
#'   SSP585 = pine_ssp585_annual,
#'   covariates = "C1",
#'   ylab = c(C1 = "Mean Annual Temp (°C x 10)")
#' )
#'
#' # Multiple variables, two scenarios
#' plot_bioclim_timeseries(
#'   SSP126 = pine_ssp126_annual,
#'   SSP585 = pine_ssp585_annual,
#'   covariates = c("C1", "C12", "C21", "C20"),
#'   ylab = c(C1  = "Mean Annual Temp (°C x 10)",
#'             C12 = "Annual Precip (mm)",
#'             C21 = "PET (mm/year)",
#'             C20 = "Aridity Index (x 10000)"),
#'   ncol = 2
#' )
#' }
#'
#' @export
plot_bioclim_timeseries <- function(...,
                                      covariates  = "C1",
                                      id_col      = "PlotID",
                                      ylab        = NULL,
                                      colors      = NULL,
                                      alpha_plots = 0.15,
                                      lwd_mean    = 2.5,
                                      show_plots  = TRUE,
                                      title       = NULL,
                                      ncol        = 1L) {

  # ── 0. Collect scenario data frames ───────────────────────────────────────
  scenarios <- list(...)
  if (length(scenarios) == 0)
    stop("Provide at least one named data frame (e.g. SSP126 = pine_ssp126_annual).")

  ssp_names <- names(scenarios)
  if (is.null(ssp_names) || any(ssp_names == ""))
    stop("All data frames must be named (e.g. SSP126 = df, SSP585 = df).")

  n_ssp <- length(scenarios)

  # ── 1. Colors ─────────────────────────────────────────────────────────────
  # Colorblind-friendly palette (Wong 2011)
  default_colors <- c("#E69F00", "#56B4E9", "#009E73",
                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  if (is.null(colors)) {
    if (n_ssp > length(default_colors))
      stop("More scenarios than available colors. Supply a 'colors' vector.")
    colors <- default_colors[seq_len(n_ssp)]
  } else {
    if (length(colors) < n_ssp)
      stop("'colors' must have at least one color per scenario.")
  }

  # ── 2. Panel layout ───────────────────────────────────────────────────────
  n_cov  <- length(covariates)
  nrow   <- ceiling(n_cov / ncol)
  old_par <- par(mfrow = c(nrow, ncol),
                 mar   = c(4, 4.5, 3, 1),
                 oma   = if (!is.null(title)) c(0, 0, 3, 0) else c(0, 0, 0, 0))
  on.exit(par(old_par), add = TRUE)

  # ── 3. Helper: add alpha to a color ───────────────────────────────────────
  add_alpha <- function(col, alpha) {
    rgb_vals <- col2rgb(col) / 255
    rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha = alpha)
  }

  # ── 4. One panel per covariate ────────────────────────────────────────────
  for (cov in covariates) {

    # Detect year columns for this covariate across all scenarios
    all_year_cols <- lapply(scenarios, function(df) {
      grep(paste0("^", cov, "_\\d{4}$"), names(df), value = TRUE)
    })

    # Check the covariate exists in all scenarios
    missing_in <- ssp_names[sapply(all_year_cols, length) == 0]
    if (length(missing_in) > 0)
      stop("Covariate '", cov, "' not found in: ",
           paste(missing_in, collapse = ", "))

    # Union of years across all scenarios, sorted
    all_years <- sort(unique(as.integer(
      sub(paste0("^", cov, "_"), "",
          unlist(all_year_cols))
    )))

    # Y-axis label
    y_label <- if (!is.null(ylab) && cov %in% names(ylab)) ylab[[cov]] else cov

    # Compute global y range across all scenarios
    all_vals <- unlist(lapply(seq_len(n_ssp), function(i) {
      df   <- scenarios[[i]]
      cols <- all_year_cols[[i]]
      unlist(df[, cols], use.names = FALSE)
    }))
    y_rng <- range(all_vals, na.rm = TRUE)
    y_pad <- diff(y_rng) * 0.05
    y_rng <- c(y_rng[1] - y_pad, y_rng[2] + y_pad)

    # Initialise empty plot
    plot(NULL,
         xlim = range(all_years),
         ylim = y_rng,
         xlab = "Year",
         ylab = y_label,
         main = cov,
         las  = 1)
    grid(col = "grey85", lty = 1)

    # Draw each scenario
    for (i in seq_len(n_ssp)) {
      df        <- scenarios[[i]]
      yr_cols   <- all_year_cols[[i]]
      years_i   <- as.integer(sub(paste0("^", cov, "_"), "", yr_cols))
      mat       <- as.matrix(df[, yr_cols])
      col_mean  <- colors[i]
      col_plot  <- add_alpha(col_mean, alpha_plots)

      # Individual plot trajectories
      if (show_plots) {
        for (r in seq_len(nrow(mat))) {
          lines(years_i, mat[r, ], col = col_plot, lwd = 0.5)
        }
      }

      # Scenario mean
      scen_mean <- colMeans(mat, na.rm = TRUE)
      lines(years_i, scen_mean, col = col_mean, lwd = lwd_mean)
    }

    # # Legend on first panel only, or every panel — here every panel for clarity
    # legend("topleft",
    #        legend = ssp_names,
    #        col    = colors,
    #        lwd    = lwd_mean,
    #        bty    = "n",
    #        cex    = 0.85)
    #

    if (n_ssp > 1) {
      legend("topleft",
             legend = ssp_names,
             col    = colors,
             lwd    = lwd_mean,
             bty    = "n",
             cex    = 0.85)
    }

  }

  # Overall title
  if (!is.null(title))
    mtext(title, outer = TRUE, cex = 1.1, font = 2, line = 1)

  invisible(NULL)
}
