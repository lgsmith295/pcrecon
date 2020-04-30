
#' Evaluate climate/tree ring relationships and select chronologies that meet threshold
#'
#' @param crns
#' @param lead
#' @param lag
#' @param prewhiten.crn
#' @param climate
#' @param mos
#' @param method
#' @param prewhiten.clim
#' @param calib
#' @param valid
#' @param cor.window
#' @param type
#' @param alternative
#' @param r
#' @param alpha
#' @param print.out
#' @param save.out
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
eval_clim <- function(crns, lead = 1, lag = NULL, prewhiten.crn = TRUE, climate, mos = 5:8, method = "mean", prewhiten.clim = TRUE, calib, valid, cor.window = "calib", type = "pearson", alternative = "two.sided", r = 0.25, alpha = 0.90, print.out = TRUE, save.out = "R", dir = "PCregOutput/") {

  full <- min(c(valid, calib)): max(c(valid,calib))

  if(prewhiten.clim == TRUE) { clim_ar <- load_clim(clim = climate, mos = mos, type = method, prewhiten.clim = prewhiten.clim, full = full)

  clim <- data.frame(clim_ar[[1]])
  } else {
    clim <- load_clim(clim = climate, mos = mos, type = method, prewhiten.clim = prewhiten.clim, full = full)
  }

  df <- dplyr::full_join(crns, clim)


if(!all(full %in% clim$year)) {
  stop("There are years in the calibration and validation period that are not in your climate data")
}

  PCR_crns <- filter_cor(crns = crns, clim = clim, lead = lead, lag = lag, cor.window = cor.window, type = type, alternative = alternative, r = r, alpha = alpha, prewhiten.crn = prewhiten.crn, prewhiten.clim = prewhiten.clim, calib = calib, full = full, valid = valid)

  cp_df <- dplyr::full_join(PCR_crns$select_crns, clim)
  common_period <- cp_df$year[complete.cases(cp_df)]


  message(paste0("FYI: The full evaluation period (calibration and validation) you've designated is ", min(full) , " to ", max(full),". The common period between your climate and tree ring variables is: ", min(common_period), " to ", max(common_period)))

  if(print.out == TRUE){
    print(PCR_crns$cors_table_small)
    print(PCR_crns$nests)
  }


  if(prewhiten.clim == TRUE){
    eval <- list(clim = clim, calib = calib, valid = valid, full = full, prewhiten.clim = prewhiten.clim, prewhiten.crn = prewhiten.crn,  cors_table = PCR_crns$cors_table, cors_table_small = PCR_crns$cors_table_small, select_crns = PCR_crns$select_crns, nests = PCR_crns$nests, clim_ar = clim_ar[[2]], crn_ar = PCR_crns$crn.ar, dir = dir)

  } else {
    eval <- list(calib = calib, valid = valid, full = full, prewhiten.clim = prewhiten.clim, prewhiten.crn = prewhiten.crn, clim = clim,  cors_table = PCR_crns$cors_table, cors_table_small = PCR_crns$cors_table_small, select_crns = PCR_crns$select_crns, nests = PCR_crns$nests, dir = dir)
  }

  class(eval) <- "PCreg_data"

  if(!is.null(save.out)){
to_save <- list(clim = clim, cors_table = PCR_crns$cors_table, cors_table_small = PCR_crns$cors_table_small, select_crns = PCR_crns$select_crns, nests = PCR_crns$nests)
save_data(data = to_save, save.out, dir)
  }
    return(eval)
}


