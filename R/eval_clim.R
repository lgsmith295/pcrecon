
#' Evaluate climate/tree ring relationships and select chronologies that meet threshold
#'
#' @param crns
#' @param lead
#' @param lag
#' @param climate
#' @param mos
#' @param method
#' @param calib
#' @param valid
#' @param type
#' @param alternative
#' @param r
#' @param prewhiten_crn
#' @param prewhiten_clim
#' @param cor_window
#' @param print
#' @param out_fmt
#' @param out_dir
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
eval_clim <- function(crns, lead = 1, lag = NULL, prewhiten_crn = TRUE, climate, mos = 5:8, method = "mean", calib, valid, cor_window = "calib", type = "pearson", alternative = "two.sided", r = 0.25, alpha = 0.90, print = TRUE, out_fmt = "R", out_dir = "PCregOutput/", pr_years = NULL) {

  full <- min(c(valid, calib)): max(c(valid,calib))

  clim <- climate$clim_small

  prewhiten_clim <- climate$prewhiten_clim

  df <- dplyr::full_join(crns, clim)


if(!all(full %in% clim$year)) {
  stop("There are years in the calibration and validation period that are not in your climate data")
}

  PCR_crns <- filter_cor(crns = crns, clim = clim, lead = lead, lag = lag, cor_window = cor_window, type = type, alternative = alternative, r = r, alpha = alpha, prewhiten_crn = prewhiten_crn, prewhiten_clim = prewhiten_clim, calib = calib, full = full, valid = valid, pr_years = pr_years)

  cp_df <- dplyr::full_join(PCR_crns$select_crns, clim)
  common_period <- cp_df$year[complete.cases(cp_df)]


  message(paste0("The full evaluation period (calibration and validation) you've designated is ", min(full) , " to ", max(full),". The common period between your climate and tree ring variables is: ", min(common_period), " to ", max(common_period)))

  if(print == TRUE){
    print(PCR_crns$cors_table_small)
    print(PCR_crns$nests)
  }


  if(prewhiten_clim == TRUE){
    clim_ar <- climate$clim_ar
    eval <- list(clim = clim, calib = calib, valid = valid, full = full, prewhiten_clim = prewhiten_clim, prewhiten_crn = prewhiten_crn,  cors_table = PCR_crns$cors_table, cors_table_small = PCR_crns$cors_table_small, select_crns = PCR_crns$select_crns, nests = PCR_crns$nests, clim_ar = clim_ar, crn_ar = PCR_crns$crn_ar, out_dir = out_dir)

  } else {
    eval <- list(clim = clim, calib = calib, valid = valid, full = full, prewhiten_clim = prewhiten_clim, prewhiten_crn = prewhiten_crn, cors_table = PCR_crns$cors_table, cors_table_small = PCR_crns$cors_table_small, select_crns = PCR_crns$select_crns, nests = PCR_crns$nests, out_dir = out_dir)
  }

  class(eval) <- "PCreg_data"

  if(!is.null(out_fmt)){
to_save <- list(clim = clim, cors_table = PCR_crns$cors_table, cors_table_small = PCR_crns$cors_table_small, select_crns = PCR_crns$select_crns, nests = PCR_crns$nests)
data <- to_save
save_data(data, out_fmt, out_dir)
  }
    return(eval)
}


