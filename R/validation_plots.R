#' Format Validation Plots for MATRIX Simulations
#'
#' Translates tree-level data into plot-level abundance matrices for two
#' inventory periods, then appends calculated structural variables (basal area
#' \code{B}, stem density \code{N}) and bioclimate covariates. Output matrices
#' are written to disk and serve as inputs for MATRIX model validation.
#'
#' @param kfinal Integer. Number of species groups (the full species range used
#'   in the model will be \code{0:kfinal}, yielding \code{kfinal + 1} groups).
#' @param out_num Character or numeric. Region identifier used to locate the
#'   correct subdirectory under \code{data/gfb3-<out_num>/} and to label output
#'   files.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads \code{final_val.csv} (second-inventory tree records) and
#'         \code{tree_cleaned.csv} (first-inventory tree records) from
#'         \code{data/gfb3-<out_num>/}.
#'   \item Constructs a species-diameter class label \code{SP_D_GP} (e.g.,
#'         \code{"S2_D7"}) for each tree, covering 14 DBH classes and
#'         \code{kfinal + 1} species groups.
#'   \item Inserts zero-filled filler rows for any species–diameter combinations
#'         absent from the data so that both matrices share a complete,
#'         consistently ordered column set.
#'   \item Aggregates trees-per-hectare (\code{TPH}) via \code{tapply} to
#'         produce the plot-level abundance matrices \code{X} (inventory 2) and
#'         \code{Y} (inventory 1).
#'   \item Computes basal area \code{B} (m² ha⁻¹) using midpoint DBH values
#'         (DBH classes centred at \code{seq(5,31,2)*2.54 + 2.54} cm) and stem
#'         density \code{N} for each plot.
#'   \item Appends bioclimate covariates to the first-inventory matrix via
#'         \code{add_covs_todf()}.
#'   \item Calculates the inter-census interval \code{dYR} and attaches it to
#'         the first-inventory matrix.
#' }
#'
#' DBH class boundaries follow the US FIA convention (5–33 inch range in 2-inch
#' steps, converted to cm), giving 14 classes with a 2.54 cm half-width.
#'
#' @return \code{NULL} (invisibly). The function is called for its side effects:
#' \describe{
#'   \item{\code{val_inv1_abmat.csv}}{First-inventory plot abundance matrix with
#'     coordinates, survey year, \code{B}, \code{N}, bioclimate covariates, and
#'     \code{dYR}. Written to \code{data/gfb3-<out_num>/}.}
#'   \item{\code{val_inv2_abmat.csv}}{Second-inventory plot abundance matrix
#'     with \code{PLT_CN} link columns, coordinates, survey year, \code{B}, and
#'     \code{N}. Written to \code{data/gfb3-<out_num>/}.}
#' }
#'
#' @seealso \code{\link{add_covs_todf}} for bioclimate covariate attachment.
#'
#' @examples
#' \dontrun{
#' # Build validation matrices for region 3 with 9 species groups
#' validation_plots(kfinal = 9, out_num = 3)
#' }
validation_plots <- function(kfinal, out_num) {

  dat2 <- readr::read_csv(paste('data/gfb3-',out_num,'/final_val.csv', sep = ''))

  dat3 <- dat2[as.numeric(dat2$D_GP)>0,] |> drop_na('SPGP', 'D_GP')

  # Convert tree dataframe to plot abundance matrix
  dat3$PLT_CN_1_2 <- paste0(dat3$PLT_CN,"-",dat3$PREV_PLT_CN)   # link PLT_CN and PREV_PLT_CN

  dat3$SP_D_GP <- paste0("S",dat3$SPGP,"_D",dat3$D_GP)
  levels(as.factor(dat3$SP_D_GP))

  # Heading for the abundance matrix
  n <- 14       # 14 dbh classes
  Svec <- rep(0:kfinal, each=n)
  Svec <- paste0("S",Svec)

  Dvec <- paste0("D",seq(1,14))
  Dvec <- rep(Dvec,(kfinal+1))

  S_D_vec <- paste0(Svec,"_",Dvec)    # header for the full matrix

  # find difference between the header and actual S_D classes
  S_D_actual <- levels(as.factor(dat3$SP_D_GP))
  diff <- setdiff(S_D_vec,S_D_actual)

  remaining <- ((kfinal+1)*n) - length(S_D_actual)

  # add filler rows to dat3 representing the missing S_D classes found above
  filler_row_vec <- rep(0,length(dat3)-2)
  filler_row_vec <- t(as.matrix(replicate(length(diff), filler_row_vec)))
  filler_row_vec <- cbind.data.frame(filler_row_vec,"filler")
  filler_row_vec <- cbind.data.frame(filler_row_vec,diff)

  names(filler_row_vec) <- names(dat3)

  dim(filler_row_vec)
  dim(dat3)

  # merge dat3 with the filler dataframe
  dat4 <- rbind.data.frame(dat3,filler_row_vec)

  # Inventory #2
  X <- tapply(dat4$TPH, list(dat4$PLT_CN_1_2, dat4$SP_D_GP), sum)
  dim(X)
  # re-order columns by name
  X <- X[,S_D_vec]
  # remove filler row
  X <- X[!(row.names(X) %in% "filler"),]

  X[is.na(X)] <- 0
  dim(X)

  # Add current and previous PLT_CN to the dataframe
  PLT_ID <- row.names(X)
  PLT_ID <- data.frame(t(as.data.frame(strsplit(PLT_ID,"-"))))
  row.names(PLT_ID) <- NULL

  length(unique(PLT_ID$X1))
  length(unique(PLT_ID$X2))

  names(PLT_ID) <- c("PLT2","PLT1")

  X <- cbind.data.frame(PLT_ID,X)
  row.names(X) <- NULL


  ### Inventory #1
  PLT <- PLT_ID$PLT1

  tree <- read.csv(paste('data/gfb3-',out_num,'/tree_cleaned.csv', sep = ''))

  dat1 <- tree[tree$PLT_CN %in% PLT, ]

  dat1 <- dat1[as.numeric(dat1$D_GP)>0,]

  length(unique(dat1$PLT_CN))

  dat1$SP_D_GP <- paste0("S",dat1$SPGP,"_D",dat1$D_GP)
  dat1 <- dat1[,-1]

  # Heading for the abundance matrix
  n <- 14       # 14 dbh classes
  Svec <- rep(0:kfinal, each=n)
  Svec <- paste0("S",Svec)

  Dvec <- paste0("D",seq(1,14))
  Dvec <- rep(Dvec,(kfinal+1))

  S_D_vec <- paste0(Svec,"_",Dvec)    # header for the full matrix

  # find difference between the header and actual S_D classes
  S_D_actual <- levels(as.factor(dat1$SP_D_GP))
  diff <- setdiff(S_D_vec,S_D_actual)

  # add filler rows to dat3 representing the missing S_D classes found above
  filler_row_vec <- rep(0,length(dat1)-2)
  filler_row_vec <- t(as.matrix(replicate(length(diff), filler_row_vec)))
  filler_row_vec <- cbind.data.frame(filler_row_vec,"filler")
  filler_row_vec <- cbind.data.frame(filler_row_vec,diff)

  names(filler_row_vec) <- names(dat1)

  # merge dat3 with the filler dataframe
  dat2 <- rbind.data.frame(dat1,filler_row_vec)


  # derive abundance matrix
  Y <- tapply(dat2$TPH, list(dat2$PLT_CN, dat2$SP_D_GP), sum)
  dim(Y)

  # remove filler row
  Y <- Y[!(row.names(Y) %in% "filler"),]

  Y[is.na(Y)] <- 0

  # re-order columns by name
  Y <- Y[,S_D_vec]
  Y <- Y[-1,]

  # add row names to PLT1 column
  PLT1 <- row.names(Y)

  Y <- cbind.data.frame(PLT1,Y)
  row.names(Y) <- NULL

  Y1 <- merge(Y,tree[,c('PLT_CN', 'YR', 'Latitude', 'Longitude')]|>distinct(),by.x="PLT1", by.y='PLT_CN', all.x=TRUE)
  names(Y1)
  dim(Y1)

  ## Match inventory 2 plots to inventory 1
  PLT_CN <- Y1$PLT1
  length(unique(PLT_CN))


  X1 <- X[X$PLT1 %in% PLT_CN,]
  X1 <- X1[ match(Y1$PLT1, X1$PLT1), ]

  # check if rows are identical
  identical(X1$PLT1,Y1$PLT1)
  chk <- cbind.data.frame(X1$PLT1,Y1$PLT1)

  names(X1)

  X1 <- merge(X1,tree[,c('PLT_CN', 'YR', 'Latitude', 'Longitude')]|>distinct(),
              by.x="PLT2", by.y='PLT_CN', all.x=TRUE)
  names(X1)

  chk2 <- cbind.data.frame(X1$YR,Y1$YR)

  dYR <- chk2[,1] - chk2[,2]
  summary(dYR)

  Y1 <- cbind.data.frame(Y1, dYR)

  X1 <- X1[ order(match(X1$PLT1, Y1$PLT1)), ]


  matrix1 <- Y1
  matrix2 <- X1

  names(Y1)

  M1 <- as.matrix(matrix1[,2:(length(matrix1)-4)])
  dim(M1)

  # Labels of DBH group
  DBH_min <- seq(5,31,2) * 2.54
  DBH_mid <- rep(DBH_min + 2.54,kfinal+1)

  # Calculate B, N, Hs, Hd
  matrix1$B <- rowSums(M1 %*% (DBH_mid^2/40000)*3.14159)
  matrix1$N <- rowSums(M1)
  # Hs and Hd use the div_function defined at the end of the code
  #Hs <- div_func(abundance_matrix=M1, col_id = rep(1:10,each=14))
  #Hd <- div_func(abundance_matrix=M1, col_id = rep(1:14, times = 10))

  data <- add_covs_todf(matrix1)
  data[is.na(data)] <- 0

  # abundance matrix for second inventory
  names(matrix2)

  M2 <- as.matrix(matrix2[,3:(length(matrix2) - 3)])
  dim(M2)

  # Labels of DBH group
  DBH_min2 <- seq(5,31,2) * 2.54
  DBH_mid2 <- rep(DBH_min2 + 2.54,kfinal+1)

  # Calculate B, N, Hs, Hd
  matrix2$B <- rowSums(M2 %*% (DBH_mid2^2/40000)*3.14159)
  matrix2$N <- rowSums(M2)

  write.csv(data,paste('data/gfb3-',out_num,'/val_inv1_abmat.csv', sep = ''))
  write.csv(matrix2,paste('data/gfb3-',out_num,'/val_inv2_abmat.csv', sep = ''))

}


