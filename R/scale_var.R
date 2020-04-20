#' Scale variance to calibration or full observed period
#'
#' @param x
#' @param y
#' @param scale.var
#' @param calib
#'
#' @return
#'
#' @examples
scale_var <- function(x = recon_nest, y = clim_full, scale.var = scale.var, calib) {
  clim_calib <- dplyr::filter(y, y$year %in% calib)

  mn <- mean(x$fit)
  sd_recon <- sd(xfit)

  sd_clim <- switch(scale.var,
         calib = sd(clim_calib$values),
         full = sd(y$values))

scale_fit <- mn + (x$fit - mn) * sd_clim/sd_recon
}
