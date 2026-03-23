#' Add Predicted PET and Aridity Index to a Plot or Grid Dataframe
#'
#' Uses a pre-trained model to predict potential evapotranspiration (PET, C21)
#' at point locations, then computes the aridity index (AI, C20) as the ratio
#' of annual precipitation to PET. Both variables are appended to the input
#' data frame, with C21 placed immediately after C20.
#'
#' @param df A data frame containing at minimum the predictor columns used by
#'   \code{model} and an annual precipitation column (see \code{precip_col}).
#' @param model A trained model object accepted by \code{predict()} — typically
#'   a \code{randomForest} object fitted on BioClim predictors with C21 as the
#'   response, but any model with a \code{predict} method will work.
#' @param vars Character vector of predictor column names to pass to
#'   \code{predict()}. Must match the variables used during model training
#'   (e.g., \code{top_vars <- c("C7", "C18", "C4", "C1", ...)}).
#' @param precip_col Character. Name of the column containing mean annual
#'   precipitation (C12 in the MATRIX covariate convention), used as the
#'   numerator in the aridity index calculation. Default: \code{"C12"}.
#'
#' @return A data frame (invisibly) with two new columns appended:
#'   \itemize{
#'     \item \code{C21} — predicted PET (mm/year)
#'     \item \code{C20} — aridity index, computed as
#'       \code{(precip_col / C21) * 10000}; C21 is placed immediately after
#'       C20 in the column order
#'   }
#'
#' @details
#' The aridity index follows the CGIAR Global-AI_PET_v3 convention:
#' \code{AI = (P / PET) * 10000}, where values below 5000 indicate arid
#' conditions and values above 10000 indicate humid conditions. C21 is
#' positioned after C20 in the output to match the MATRIX covariate column
#' ordering convention.
#'
#' If \code{df} already contains columns named \code{C21} or \code{C20},
#' they will be overwritten without warning.
#'
#' @examples
#' \dontrun{
#' # Using a pre-trained RF model
#' top_vars <- c("C7", "C18", "C4", "C1", "C17", "C15", "C11", "C10")
#'
#' pine_plots_ssp126 <- add_pet_ai(pine_plots_ssp126,
#'                                 model = rf_pet_slim,
#'                                 vars  = top_vars)
#'
#' pine_plots_ssp585 <- add_pet_ai(pine_plots_ssp585,
#'                                 model = rf_pet_slim,
#'                                 vars  = top_vars)
#' }
#'
#' @importFrom dplyr relocate
#' @export
add_pet_ai <- function(df, model, vars, precip_col = "C12") {
  df$C21 <- predict(model, df[, vars])
  df$C20 <- (df[[precip_col]] / df$C21) * 10000
  df <- relocate(df, C21, .after = C20)
  invisible(df)
}
