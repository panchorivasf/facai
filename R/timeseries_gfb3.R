#' Plot GFB3 time series metrics
#'
#' Computes and plots trees per hectare, basal area, or aboveground biomass
#' over time from a GFB3-formatted dataset.
#'
#' @param data A data frame in GFB3 format with columns \code{PlotID},
#'   \code{PA}, \code{DBH}, \code{YR}, and \code{Status}.
#' @param metric Character. Metric to plot. One of:
#'   \describe{
#'     \item{\code{"tph"}}{Trees per hectare.}
#'     \item{\code{"ba"}}{Basal area (m\eqn{^2} ha\eqn{^{-1}}).}
#'     \item{\code{"agb"}}{Aboveground biomass (Mg ha\eqn{^{-1}}).}
#'   }
#'   Default is \code{"tph"}.
#' @param type Character. Plot type. One of \code{"line"}, \code{"bar"}, or
#'   \code{"loess"} (points with loess smooth and confidence band).
#'   Default is \code{"line"}.
#' @param by_plot Logical. If \code{TRUE} (default), plots are coloured by
#'   \code{PlotID}. If \code{FALSE}, all plots are aggregated into a single
#'   series.
#' @param agb_fn Optional function. A function of DBH (cm) returning AGB in Mg
#'   per stem, used when \code{metric = "agb"}. If \code{NULL} (default), the
#'   Chave et al. pantropical wet forest allometry is used:
#'   \eqn{0.0673 \times (0.0673 \times DBH^2)^{0.976}}.
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' Only alive stems (\code{Status == 0}) with valid \code{DBH} and \code{PA}
#' are included in the computation. Basal area is computed as
#' \eqn{\pi (DBH / 200)^2} per stem and summed per plot x year, then divided
#' by plot area. Trees per hectare is the stem count divided by plot area.
#' AGB is summed per plot x year and divided by plot area.
#'
#' @examples
#' \dontrun{
#' # Line plot of basal area by plot
#' timeseries_gfb3(paracou, metric = "ba", type = "line")
#'
#' # Loess smooth of TPH across all plots combined
#' timeseries_gfb3(paracou, metric = "tph", type = "loess",
#'                      by_plot = FALSE)
#'
#' # Bar chart of AGB with custom allometry
#' timeseries_gfb3(paracou, metric = "agb", type = "bar",
#'                      agb_fn = function(dbh) 0.0509 * dbh^2.5)
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise first n
#' @importFrom ggplot2 ggplot aes geom_line geom_col geom_point geom_smooth
#'   labs theme_minimal theme
#' @export
timeseries_gfb3 <- function(data,
                            metric  = c("tph", "ba", "agb"),
                            type    = c("line", "bar", "loess"),
                            by_plot = TRUE,
                            agb_fn  = NULL) {
  metric <- match.arg(metric)
  type   <- match.arg(type)

  all_na_years <- data |>
    dplyr::filter(Status == "0" | Status == 0) |>
    dplyr::group_by(PlotID, YR) |>
    dplyr::summarise(n_valid = sum(!is.na(DBH)), .groups = "drop") |>
    dplyr::filter(n_valid == 0)

  alive <- data |>
    dplyr::filter(Status == "0" | Status == 0, !is.na(DBH), !is.na(PA)) |>
    dplyr::mutate(
      BA  = pi * (DBH / 200)^2,
      AGB = if (!is.null(agb_fn)) agb_fn(DBH) else 0.0673 * (0.0673 * DBH^2)^0.976
    )

  summary_df <- alive |>
    dplyr::group_by(PlotID, YR) |>
    dplyr::summarise(
      tph = dplyr::n() / dplyr::first(PA),
      ba  = sum(BA,  na.rm = TRUE) / dplyr::first(PA),
      agb = sum(AGB, na.rm = TRUE) / dplyr::first(PA),
      .groups = "drop"
    )

  if (!by_plot) {
    summary_df <- summary_df |>
      dplyr::group_by(YR) |>
      dplyr::summarise(
        tph = mean(tph, na.rm = TRUE),
        ba  = mean(ba,  na.rm = TRUE),
        agb = mean(agb, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(PlotID = "All plots")
    all_na_years <- all_na_years |> dplyr::mutate(PlotID = "All plots")
  }

  y_lab <- switch(metric,
                  tph = "Trees per hectare",
                  ba  = "Basal area (m\u00b2 ha\u207b\u00b9)",
                  agb = "AGB (Mg ha\u207b\u00b9)"
  )

  plots <- unique(summary_df$PlotID)
  y_max <- max(summary_df[[metric]], na.rm = TRUE)
  bar_color <- "#378ADD"  # clean blue

  if (type == "bar") {
    all_yrs <- sort(unique(c(summary_df$YR, all_na_years$YR)))

    census_labels <- tibble::tibble(YR = all_yrs) |>
      dplyr::mutate(
        census_n     = dplyr::row_number(),
        census_label = paste0("Census ", census_n, "\n(", YR, ")")
      )

    full_grid <- tidyr::expand_grid(
      PlotID = plots,
      YR     = all_yrs
    ) |>
      dplyr::left_join(census_labels, by = "YR") |>
      dplyr::left_join(summary_df, by = c("PlotID", "YR")) |>
      dplyr::left_join(
        all_na_years |> dplyr::select(PlotID, YR) |> dplyr::mutate(is_na_census = TRUE),
        by = c("PlotID", "YR")
      ) |>
      dplyr::mutate(
        is_na_census = dplyr::coalesce(is_na_census, FALSE),
        census_label = factor(census_label, levels = census_labels$census_label)
      )

    summary_df <- full_grid
  }

  p <- plotly::plot_ly()

  for (pid in plots) {
    df_p <- summary_df |> dplyr::filter(PlotID == pid)
    na_p <- all_na_years |> dplyr::filter(PlotID == pid)

    if (type == "line") {
      p <- p |>
        plotly::add_trace(
          data          = df_p,
          x             = ~YR,
          y             = ~.data[[metric]],
          type          = "scatter",
          mode          = "lines+markers",
          name          = pid,
          showlegend    = FALSE,
          line          = list(color = bar_color),
          marker        = list(color = bar_color, size = 5),
          hovertemplate = paste0(
            "<b>", pid, "</b><br>Year: %{x}<br>", y_lab, ": %{y:.2f}<extra></extra>"
          )
        )

    } else if (type == "bar") {
      df_valid <- df_p |> dplyr::filter(!is_na_census)
      df_blank <- df_p |> dplyr::filter( is_na_census)

      if (nrow(df_valid) > 0) {
        p <- p |>
          plotly::add_bars(
            data          = df_valid,
            x             = ~census_label,
            y             = ~.data[[metric]],
            name          = pid,
            legendgroup   = pid,
            showlegend    = FALSE,
            marker        = list(
              color = bar_color,
              line  = list(color = bar_color, width = 0)
            ),
            hovertemplate = paste0(
              "<b>", pid, "</b><br>%{x}<br>", y_lab, ": %{y:.2f}<extra></extra>"
            )
          )
      }

      if (nrow(df_blank) > 0) {
        p <- p |>
          plotly::add_bars(
            data          = df_blank,
            x             = ~census_label,
            y             = rep(y_max * 0.025, nrow(df_blank)),
            name          = pid,
            legendgroup   = pid,
            showlegend    = FALSE,
            marker        = list(
              color = "rgba(0,0,0,0)",
              line  = list(color = "#E24B4A", width = 2)
            ),
            hovertemplate = paste0(
              "<b>", pid, "</b><br>%{x}<br>All DBH NA<extra></extra>"
            )
          )
      }

    } else if (type == "loess") {
      lo     <- stats::loess(stats::as.formula(paste(metric, "~ YR")), data = df_p)
      yr_seq <- seq(min(df_p$YR, na.rm = TRUE), max(df_p$YR, na.rm = TRUE), length.out = 100)
      pred   <- stats::predict(lo, newdata = data.frame(YR = yr_seq), se = TRUE)

      p <- p |>
        plotly::add_trace(
          x             = df_p$YR,
          y             = df_p[[metric]],
          type          = "scatter",
          mode          = "markers",
          name          = pid,
          showlegend    = FALSE,
          marker        = list(size = 5, opacity = 0.5, color = bar_color),
          hovertemplate = paste0(
            "<b>", pid, "</b><br>Year: %{x}<br>", y_lab, ": %{y:.2f}<extra></extra>"
          )
        ) |>
        plotly::add_ribbons(
          x          = yr_seq,
          ymin       = pred$fit - 1.96 * pred$se.fit,
          ymax       = pred$fit + 1.96 * pred$se.fit,
          name       = paste(pid, "CI"),
          showlegend = FALSE,
          opacity    = 0.15,
          fillcolor  = bar_color,
          line       = list(color = "transparent")
        ) |>
        plotly::add_lines(
          x          = yr_seq,
          y          = pred$fit,
          name       = paste(pid, "smooth"),
          showlegend = FALSE,
          line       = list(width = 2, color = bar_color)
        )
    }
  }

  p |>
    plotly::layout(
      xaxis      = list(
        title     = if (type == "bar") "Census" else "Year",
        type      = if (type == "bar") "category" else "-",
        tickangle = if (type == "bar") -45 else 0
      ),
      yaxis      = list(title = y_lab),
      barmode    = if (type == "bar") "group" else NULL,
      showlegend = FALSE,
      hovermode  = "closest"
    )
}
