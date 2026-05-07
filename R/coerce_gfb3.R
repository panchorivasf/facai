#' Coerce GFB3 columns to standard types
#'
#' Enforces the expected column types for a GFB3-formatted forest inventory
#' data frame and subsets to the standard set of columns.
#'
#' @param data A data frame containing raw GFB3 inventory data.
#' @param keep_all_cols Logical. Whether or not to keep all the columns. If
#' \code{FALSE}, only the mandatory GFB3 columns are kept. Default \code{TRUE}.
#'
#' @return A data frame with the following columns coerced to their expected
#'   types: \code{PlotID} (character), \code{PA} (numeric), \code{Latitude}
#'   (numeric), \code{Longitude} (numeric), \code{TreeID} (character),
#'   \code{Species} (character), \code{Status} (character), \code{DBH}
#'   (numeric), \code{YR} (numeric), \code{PrevDBH} (numeric), and
#'   \code{PrevYR} (numeric).
#'
#' @details Coercion of non-numeric strings in numeric columns (e.g. \code{PA}
#'   values entered as \code{"1-ha"}) will produce \code{NA} with a warning.
#'   Inspect and clean such values before calling this function.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @export
coerce_gfb3 <- function(data, keep_all_cols = TRUE){

 data <-  data |>
    mutate(PlotID     = as.character(PlotID),
           PA        = as.numeric(PA),
           Latitude  = as.numeric(Latitude),
           Longitude = as.numeric(Longitude),
           TreeID    = as.character(TreeID),
           Species   = as.character(Species),
           Status    = as.character(Status),
           DBH       = as.numeric(DBH),
           PrevDBH   = as.numeric(PrevDBH))

 if (!keep_all_cols){
   data <- data |>
     select(PlotID, PA, Latitude, Longitude,
            TreeID, Species, Status,
            DBH,YR, PrevDBH, PrevYR)
 }

 data

}
