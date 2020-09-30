
#' calculate PCs
#'
#' @param pc_calc period over which to calculate principal components ("calib", "valid", or "full")
#' @param nest_yrs years of the current nest being analyzed
#' @param calib calibration period years
#' @param valid validation period years
#' @param full calibration and validation period
#' @param PCA_chrons dataframe containing chronologies
#' @param period_df

#'
#' @return
#'
#' @examples
#'
#'
#'
calc_PCs <- function(period_df, PCA_chrons, pc_calc, nest_yrs, calib, full, valid) {
  nest <- PCA_chrons %>%
    dplyr::filter(PCA_chrons$year %in% nest_yrs) %>%
    dplyr::select(-year)

  chrons_period <- switch(pc_calc,
                          calib = dplyr::filter(PCA_chrons, year %in% calib),
                          valid = dplyr::filter(PCA_chrons, year %in% valid),
                          full = dplyr::filter(PCA_chrons, year %in% full))


  PCA_chrons_calib <- chrons_period %>%
    dplyr::select(-year)

  PCA <- prcomp(PCA_chrons_calib, scale = TRUE)

  list(PCA = PCA, chrons_period = chrons_period, nest = nest)
}
