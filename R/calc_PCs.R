
#' calculate PCs
#'
#' @param periods_df dataframe of nests
#' @param PCA_chrons dataframe containing chronologies
#' @param period period of time over which to calculate PCs, calibration or nest_yrs
#'
#' @return
#'
#' @examples
calc_PCs <- function(periods_df, PCA_chrons, pc.calc, nest_yrs, calib, full) {
  nest <- PCA_chrons %>%
    dplyr::filter(PCA_chrons$year %in% nest_yrs) %>%
    dplyr::select(-year)

  chrons_period <- switch(pc.calc,
                          calib = dplyr::filter(PCA_chrons, year %in% calib),
                          full = dplyr::filter(PCA_chrons, year %in% full))


  PCA_chrons_calib <- chrons_period %>%
    dplyr::select(-year)

  PCA <- prcomp(PCA_chrons_calib, scale = TRUE)

  list(PCA = PCA, chrons_period = chrons_period, nest = nest)
}
