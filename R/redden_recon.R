redden_recon <- function(recon, ar.model) {
  if(ar.model$order == 0) {
    warning("the ar order for the original climate data was 0, meaning that no autoregressive structure has been added back in (not reddened)")
  } else {
    recon <- recon %>%
      dplyr::arrange(year)

    recon$reds <- NA_real_

    for(i in (ar.model$order + 1):length(recon$fit)) {
      tmp <- 0
      for(j in 1:length(ar.model$ar)) {
        tmp <- tmp + recon$fit[i-j] * ar.model$ar[j]
      }
      recon$reds[i] <- recon$fit[i] + tmp
    }
    recon <- recon %>%
      dplyr::arrange(dplyr::desc(year))
  }
  return(recon)
}


