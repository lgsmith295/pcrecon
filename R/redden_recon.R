#' Title
#'
#' @param recon
#' @param ar_model
#'
#' @return
#' @export
#'
#' @examples
redden_recon <- function(recon, ar_model) {
  if(ar_model$order == 0) {
    warning("the ar order for the original climate data was 0, meaning that no autoregressive structure has been added back in to the reconstruction (not reddened). If red noise was found in reconstruction, residual (whitened) reconstruction included in recon dataframe")
    ar <- ar(recon$fit)
    recon$reds <- ar$resid + ar$x.mean
  } else {
    recon <- recon %>%
      dplyr::arrange(year)

    recon$reds <- NA_real_

    for(i in (ar_model$order + 1):length(recon$fit)) {
      tmp <- 0
      for(j in 1:length(ar_model$ar)) {
        tmp <- tmp + recon$fit[i-j] * ar_model$ar[j]
      }
      recon$reds[i] <- recon$fit[i] + tmp
    }
  }
  recon <- recon %>%
    dplyr::arrange(dplyr::desc(year))
  return(recon)
}

