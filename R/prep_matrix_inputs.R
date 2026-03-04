#' Prepare MATRIX Model Inputs from GFBI3 Forest Inventory Data
#'
#' Previous name "process_data()"
#'
#' Performs all pre-processing steps required to transform a raw multi-survey
#' tree-level inventory into training and validation datasets ready for the
#' MATRIX forest demographic model. Steps include data cleaning, DBH class
#' assignment, hierarchical species grouping, basal area calculation, and a
#' plot-level train/validation split.
#'
#' @param trees_df Data frame. Raw tree-level inventory with one row per
#'   tree-visit. Required columns: \code{PLTCD} (plot identifier),
#'   \code{YR} (survey year), \code{PrevYR} (previous survey year),
#'   \code{DBH} (diameter at breast height, cm), \code{PrevDBH} (previous
#'   DBH, cm), \code{PA} (plot area, ha), \code{Status} (tree status code:
#'   0 = alive, 1 = dead, 2 = recruited), \code{Species} (species identifier),
#'   \code{Latitude}, \code{Longitude}.
#' @param out_num Character or numeric. Region identifier used to route outputs
#'   to \code{data/gfb3-<out_num>/} and \code{outputs/gfb3-<out_num>/}.
#' @param val_prop Numeric in \code{(0, 1)}. Proportion of paired plots
#'   (i.e. plots with a valid previous survey) to reserve for validation.
#'   For example, \code{0.2} reserves 20\% for validation and uses 80\% for
#'   training. Plots with no previous survey record are always assigned to
#'   training.
#' @param plotid_col Character.Name of the column with the plot identifier.
#' @param geo_filter Character.Name of the geopolitical boundary to be used (e.g.,
#' "Honduras", "Africa", etc.) for plotting.
#' @param geo_level Character.Geopolitical unit (country or continent) for
#' plotting.
#'
#' @details
#' The function proceeds through the following stages:
#' \enumerate{
#'   \item \strong{Cleaning}: Drops rows missing \code{PA}, \code{Status},
#'     coordinates, \code{YR}, or \code{Species}. Computes trees per hectare
#'     (\code{TPH = 1/PA}, set to 0 for dead trees) and remeasurement period
#'     (\code{REMPER = YR - PrevYR}). Constructs unique plot-visit identifiers
#'     \code{PLT_CN} and \code{PREV_PLT_CN} from \code{PLTCD} and survey year.
#'   \item \strong{DBH classes}: Assigns each tree to one of 14 diameter
#'     classes (\code{D_GP}) spanning 12.7–78.7 cm in ~5 cm steps following
#'     US FIA conventions. Trees below 12.7 cm receive \code{D_GP = 0};
#'     trees at or above 78.7 cm receive \code{D_GP = 14}.
#'   \item \strong{Species grouping}: If more than 10 species are present,
#'     calls \code{\link{add_spgp_to_df}} to cluster species into optimal
#'     groups via Random Forest proximity and hierarchical clustering
#'     (silhouette-optimised). Otherwise species are assigned individual groups
#'     indexed from 0. Rare/infrequent species are always assigned to group 0.
#'   \item \strong{Basal area and stem density}: Computes plot-level basal
#'     area \code{B} (m² ha⁻¹) and stem density \code{N} (stems ha⁻¹) and
#'     merges them back to the tree data frame.
#'   \item \strong{Train/validation split}: Splits at the plot level. Only
#'     plots with a matched previous survey (\code{PREV_PLT_CN} present in
#'     \code{PLT_CN}) are eligible for the validation set; the remainder go
#'     to training unconditionally.
#'   \item \strong{Bioclimate covariates}: Calls \code{\link{add_covs_todf}}
#'     to append bioclimate variables to the training set only.
#' }
#'
#' @return Integer. The maximum species group index (\code{n_spgp - 1}),
#'   i.e. \code{length(unique(SPGP)) - 1}. Pass this directly to
#'   \code{\link{validation_plots}} as its first argument.
#'
#' @section Side effects:
#' Writes the following files:
#' \describe{
#'   \item{\code{data/gfb3-<out_num>/tree_cleaned.csv}}{Full cleaned tree data
#'     frame with species groups, DBH classes, \code{B}, and \code{N}.}
#'   \item{\code{data/gfb3-<out_num>/final_train.csv}}{Training set with
#'     bioclimate covariates appended.}
#'   \item{\code{data/gfb3-<out_num>/final_val.csv}}{Validation set (no
#'     bioclimate covariates; these are added later by
#'     \code{\link{validation_plots}}).}
#'   \item{\code{outputs/gfb3-<out_num>/hist_samples.png}}{Histogram of
#'     sample counts per species group.}
#'   \item{\code{outputs/gfb3-<out_num>/cluster_summary.txt}}{Species-to-group
#'     assignment table.}
#'   \item{\code{outputs/gfb3-<out_num>/traintestplots.txt}}{Plot counts for
#'     total, training, and validation sets.}
#'   \item{\code{outputs/gfb3-<out_num>/traintestplots.png}}{Map of training
#'     (blue) and validation (red) plot locations.}
#' }
#'
#' @seealso \code{\link{add_spgp_to_df}}, \code{\link{add_covs_todf}},
#'   \code{\link{validation_plots}}
#'
#' @examples
#' \dontrun{
#' n_spgp <- prep_matrix_inputs(
#'   trees_df    = honduras_trees,
#'   out_num    = 1,
#'   val_prop = 0.2
#' )
#' validation_plots(kfinal = n_spgp, out_num = 1)
#' }
prep_matrix_inputs <- function(trees_df,
                               out_num,
                               val_prop,
                               plotid_col = "PlotID",
                               geo_filter = "Honduras",
                               geo_level = "country") {

  dir.create(paste0('data/gfb3-', out_num), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0('outputs/gfb3-', out_num), recursive = TRUE, showWarnings = FALSE)

  #### Clean Tree DF and Add TPH REMPER Columns ####

  dat <-  trees_df |> drop_na('PA', 'Status',
                              'Latitude', 'Longitude', 'YR', 'Species')
  dat$TPH <- 1 / dat$PA
  dat$TPH <- ifelse(dat$Status == 1, 0, dat$TPH)
  dat$REMPER <- dat$YR - dat$PrevYR
  dat <-  dat |> drop_na('REMPER')

  dat$PLT_CN <- paste0(dat[[plotid_col]], '_', dat$YR)
  dat$PREV_PLT_CN <- paste0(dat[[plotid_col]], '_', dat$PrevYR)

  #### Assign DBH Groups ####

  # DBH group
  DBHseq <- seq(5,31,2) * 2.54
  DBHGP <- cut(dat$DBH, breaks=DBHseq, right=FALSE)

  dat <- cbind.data.frame(dat,DBHGP)

  dat$D_GP <-  ifelse(is.na(dat$DBHGP) & dat$DBH >= 78.7, 14, dat$DBHGP)
  dat$D_GP <-  ifelse(is.na(dat$DBHGP) & dat$DBH < 12.7, 0, dat$D_GP)
  dat$D_GP <- as.factor(dat$D_GP)

  #### Species Dynamics Calculations ####

  #Create List of All Present Species
  dfclus <- as.data.frame(list(dplyr::distinct(as.data.frame(dat$Species))))
  colnames(dfclus) <- c('SPCD')

  #Cluster if there are more than 10 species present in the inventory

  if(nrow(dfclus) > 10){

    final_clusters <- add_spgp_to_df(dfclus, out_num, dat)

  } else {

    final_clusters <- dfclus
    final_clusters$SPGP <- row.names(final_clusters)
    final_clusters$SPGP <- as.numeric(final_clusters$SPGP) - 1

  }

  #Joining species groups to original tree data frame
  dat <- merge(dat, final_clusters, by.x = 'Species', by.y = 'SPCD')

  write.csv(dat, paste('data/gfb3-',out_num,'/tree_cleaned.csv', sep = ''))

  kfinal <- length(unique(dat$SPGP)) - 1

  #### Summarize Species Clustering Information ####

  #Histogram of Number of Samples in each species group
  png(paste('outputs/gfb3-',out_num,'/hist_samples.png', sep = ''),
      width=800, height=800)
  hist(dat$SPGP, main = 'Distribution of Samples Per Cluster in Inventory',
       xlab = 'Clusters')
  dev.off()

  #Full summary of which species are in which groups
  fileConn <- file(paste('outputs/gfb3-', out_num,'/cluster_summary.txt',
                         sep = ''), 'w')
  writeLines("The Number of Species in Each Cluster:", fileConn)
  write.table(summary(as.factor(final_clusters$SPGP)), fileConn,
              col.names = FALSE)
  for (x in 0:max(final_clusters$SPGP)) {

    writeLines(paste("\n\nThe Species in Group", x, "Are:"), fileConn)

    writeLines(paste(matrix(subset(final_clusters, SPGP == x)$SPCD)), fileConn)

  }
  close(fileConn)

  #### Add Basal Area and Number of trees ####

  bncalc <- dat |> drop_na('DBH', 'TPH')

  B <- tapply((bncalc$DBH^2/40000)*3.14159*bncalc$TPH, bncalc$PLT_CN, sum)
  N <- tapply(bncalc$TPH, bncalc$PLT_CN, sum)

  bndf <- cbind.data.frame(B, N)
  bndf$PLT_CN <- row.names(bndf)

  dat <- merge(dat, bndf, by = 'PLT_CN')

  #### Train Test Split ####

  dfs <- dat[,c('PLT_CN', 'PREV_PLT_CN')] |> distinct(.keep_all = TRUE)

  valid <- dfs[dfs$PREV_PLT_CN %in% dfs$PLT_CN,]
  invalid <- dfs[!dfs$PREV_PLT_CN %in% dfs$PLT_CN,]

  sample <- sample(c(TRUE, FALSE), nrow(valid), replace=TRUE,
                   prob=c(val_prop,(1 - val_prop)))
  test  <- valid[sample,]
  train   <- valid[!sample,]

  trainplots <- rbind(invalid, train)
  traindf <- dat[dat$PLT_CN %in% trainplots$PLT_CN,]
  testdf <- dat[dat$PLT_CN %in% test$PLT_CN,]


  #Text file with summary of train test split breakdown
  fileConn <- file(paste("outputs/gfb3-", out_num, "/traintestplots.txt",
                         sep=''), 'w')

  writeLines(paste("Total Plots:", length(unique(dat$PLT_CN))), fileConn)

  writeLines(paste("Training Plots:", length(unique(traindf$PLT_CN))),
             fileConn)

  writeLines(paste("Validation Plots:", length(unique(testdf$PLT_CN))),
             fileConn)

  close(fileConn)


  trainplotlist <- dplyr::distinct(as.data.frame(traindf[,c(plotid_col,
                                                            "Latitude",
                                                            "Longitude")]))
  testplotlist <- dplyr::distinct(as.data.frame(testdf[,c(plotid_col,
                                                          "Latitude",
                                                          "Longitude")]))

  train_sf <- st_as_sf(trainplotlist, coords = c("Longitude","Latitude"),
                       crs = 4326, remove = FALSE)
  test_sf <- st_as_sf(testplotlist, coords = c("Longitude","Latitude"),
                      crs = 4326, remove = FALSE)

  if (geo_level == "continent") {
    boundaries <- ne_countries(continent = geo_filter, returnclass = "sf")
  } else {
    boundaries <- ne_countries(country = geo_filter, returnclass = "sf")
  }

  #Map of where the train and test plots are in North America
  png(paste("outputs/gfb3-", out_num, "/traintestplots.png", sep=''),
      width=400, height=600)
  print({

    ggplot() +
      ggtitle(paste0('Region ', out_num,
                     ': Train Plots (Blue) and Test Plots (Red)')) +
      geom_sf(data = boundaries)+
      geom_sf(data = train_sf, aes(color = "Train Plots"),
              col = "blue", size = 1) +
      geom_sf(data = test_sf, aes(color = "Test Plots"),
              col = "red", size = 1)+
      theme_bw()

  })
  dev.off()

  #### Add Bioclimate Covariates to Training Data ####
  traindf <- add_covs_todf(traindf)

  #### Write Training and Testing Data to CSV's ####
  write.csv(traindf,paste('data/gfb3-',out_num,'/final_train.csv', sep = ''),
            row.names = FALSE)
  write.csv(testdf,paste('data/gfb3-',out_num,'/final_val.csv', sep = ''),
            row.names = FALSE)

  #### Return Number of Species Groups to Use in Subsequent Functions ####
  return(kfinal)
}
