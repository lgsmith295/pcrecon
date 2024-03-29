#' Scale variance to calibration or full observed period
#'
#' @param x
#' @param y
#' @param calib
#' @param scale_var
#' @param step_mod
#'
#' @return
#'
#' @examples
scale_var <- function(x = recon_nest, y = clim_full, scale_var = scale_var, calib, step_mod) {
  if(isTRUE(length(step_mod$coefficients) == 1)) {
    scale_fit <- x$fit
  } else {
  clim_calib <- dplyr::filter(y, y$year %in% calib)

  mn <- mean(x$fit)
  sd_recon <- sd(x$fit)

  sd_clim <- switch(scale_var,
         calib = sd(clim_calib$values),
         full = sd(y$values))

scale_fit <- mn + (x$fit - mn) * sd_clim/sd_recon

  }

return(scale_fit)
}


