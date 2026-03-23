#' Interpolate Climate Covariates Annually Across Four CHELSA Periods
#'
#' Linearly interpolates climate covariates between the midpoints of four
#' climate periods (historic baseline + three future CHELSA periods), producing
#' one value per plot per year from \code{year_hist} to \code{year_end}. The
#' result is a wide-format data frame with columns named \code{C1_1995},
#' \code{C1_1996}, ..., \code{C21_2085}.
#'
#' The four anchor points and their default midpoint years are:
#' \itemize{
#'   \item Historic baseline (CHELSA 1981-2010): 1995
#'   \item Near future   (CHELSA 2011-2040):     2025
#'   \item Mid future    (CHELSA 2041-2070):      2055
#'   \item Far future    (CHELSA 2071-2100):      2085
#' }
#'
#' @param df_hist A data frame of historic baseline plot covariates (period
#'   centred on \code{year_hist}, i.e. CHELSA 1981-2010). Must contain
#'   \code{id_col} and all columns in \code{covariates}.
#' @param df_near A data frame of near-future plot covariates (period centred
#'   on \code{year_near}, i.e. CHELSA 2011-2040). Must contain \code{id_col}
#'   and all columns in \code{covariates}.
#' @param df_mid A data frame of mid-century plot covariates (period centred
#'   on \code{year_mid}, i.e. CHELSA 2041-2070). Must contain \code{id_col}
#'   and all columns in \code{covariates}.
#' @param df_far A data frame of far-future plot covariates (period centred
#'   on \code{year_end}, i.e. CHELSA 2071-2100). Must contain \code{id_col}
#'   and all columns in \code{covariates}.
#' @param covariates Character vector of covariate column names to interpolate.
#'   Default: \code{paste0("C", 1:21)}.
#' @param id_col Character. Name of the plot identifier column. Default:
#'   \code{"PlotID"}.
#' @param year_hist Integer. Midpoint year of the historic baseline period.
#'   Default: \code{1995L}.
#' @param year_near Integer. Midpoint year of the near-future period.
#'   Default: \code{2025L}.
#' @param year_mid Integer. Midpoint year of the mid-century period.
#'   Default: \code{2055L}.
#' @param year_end Integer. Midpoint year of the far-future period and last
#'   year of the output sequence. Default: \code{2085L}.
#'
#' @return A data frame (invisibly) in wide format with one row per plot and
#'   columns \code{id_col} followed by \code{cov_YYYY} columns for each
#'   covariate and year from \code{year_hist} to \code{year_end}.
#'
#' @details
#' Interpolation is performed in three segments:
#' \itemize{
#'   \item Segment 1: \code{year_hist} to \code{year_near} (1995–2025)
#'   \item Segment 2: \code{year_near} to \code{year_mid}  (2025–2055)
#'   \item Segment 3: \code{year_mid}  to \code{year_end}  (2055–2085)
#' }
#' Within each segment, values are linearly interpolated as:
#' \code{v_start + (v_end - v_start) * t}, where \code{t} runs from 0 to 1.
#' Anchor years at segment boundaries appear exactly once in the output.
#' No extrapolation is performed beyond \code{year_end}.
#'
#' All four input data frames must contain the same plot IDs in the same row
#' order. Sort by \code{id_col} before calling if necessary.
#'
#' @examples
#' \dontrun{
#' covariates <- paste0("C", 1:21)
#'
#' pine_ssp126_annual <- interpolate_covariates(
#'   df_hist    = pine_plots_historic,
#'   df_near    = pine_plots_ssp126_2011_2040,
#'   df_mid     = pine_plots_ssp126_2041_2070,
#'   df_far     = pine_plots_ssp126_2071_2100,
#'   covariates = covariates,
#'   id_col     = "PlotID"
#' )
#'
#' # SSP585
#' pine_ssp585_annual <- interpolate_covariates(
#'   df_hist    = pine_plots_historic,
#'   df_near    = pine_plots_ssp585_2011_2040,
#'   df_mid     = pine_plots_ssp585_2041_2070,
#'   df_far     = pine_plots_ssp585_2071_2100,
#'   covariates = covariates,
#'   id_col     = "PlotID"
#' )
#' }
#'
#' @importFrom stats setNames
#' @export
interpolate_covariates <- function(df_hist,
                                   df_near,
                                   df_mid,
                                   df_far,
                                   covariates = paste0("C", 1:21),
                                   id_col     = "PlotID",
                                   year_hist  = 1995L,
                                   year_near  = 2025L,
                                   year_mid   = 2055L,
                                   year_end   = 2085L) {

  # ── 0. Validate inputs ─────────────────────────────────────────────────────
  dfs <- list(df_hist = df_hist, df_near = df_near,
              df_mid  = df_mid,  df_far  = df_far)
  for (nm in names(dfs)) {
    missing <- setdiff(c(id_col, covariates), names(dfs[[nm]]))
    if (length(missing) > 0)
      stop("Column(s) not found in '", nm, "': ",
           paste(missing, collapse = ", "))
  }

  # ── 1. Align rows by plot ID ───────────────────────────────────────────────
  ids <- df_hist[[id_col]]
  if (!identical(ids, df_near[[id_col]]) ||
      !identical(ids, df_mid[[id_col]])  ||
      !identical(ids, df_far[[id_col]]))
    stop("Plot IDs differ across the four period data frames. ",
         "Sort all four by '", id_col, "' before calling this function.")

  # ── 2. Year sequences per segment ─────────────────────────────────────────
  seg1_years <- seq(year_hist, year_near)   # 1995:2025
  seg2_years <- seq(year_near, year_mid)    # 2025:2055
  seg3_years <- seq(year_mid,  year_end)    # 2055:2085
  all_years  <- c(seg1_years,
                  seg2_years[-1],           # drop duplicated 2025
                  seg3_years[-1])           # drop duplicated 2055

  # ── 3. Linear interpolation helper ────────────────────────────────────────
  # Returns a matrix: n_plots x n_years
  lininterp <- function(v_start, v_end, n_years) {
    t_seq <- seq(0, 1, length.out = n_years)
    outer(seq_along(v_start), t_seq,
          function(i, t) v_start[i] + (v_end[i] - v_start[i]) * t)
  }

  # ── 4. Interpolate each covariate ─────────────────────────────────────────
  interp_list <- lapply(covariates, function(cov) {

    v_hist <- df_hist[[cov]]
    v_near <- df_near[[cov]]
    v_mid  <- df_mid[[cov]]
    v_far  <- df_far[[cov]]

    seg1 <- lininterp(v_hist, v_near, length(seg1_years))
    seg2 <- lininterp(v_near, v_mid,  length(seg2_years))
    seg3 <- lininterp(v_mid,  v_far,  length(seg3_years))

    # Combine segments, dropping duplicated anchor columns
    mat  <- cbind(seg1, seg2[, -1, drop = FALSE], seg3[, -1, drop = FALSE])
    colnames(mat) <- paste0(cov, "_", all_years)
    as.data.frame(mat)
  })

  # ── 5. Assemble output ────────────────────────────────────────────────────
  out <- cbind(
    setNames(data.frame(ids), id_col),
    do.call(cbind, interp_list)
  )

  message("Done. Output: ", nrow(out), " plots x ", ncol(out) - 1L,
          " covariate-year columns (", year_hist, "-", year_end, ")")

  invisible(out)
}
