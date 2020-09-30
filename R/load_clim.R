#' Get months of interest for climate analysis, sum or average them
#'
#' @param clim
#' @param method
#' @param prewhiten_clim
#' @param full
#' @param mos months of interest
#'
#' @return dataframe of monthly data
#' @export
#'
#' @examples
load_clim <- function(clim, mos, method = "mean", prewhiten_clim = TRUE, full = full) {
  if (!(ncol(clim) %in% c(3,13))) {
    stop("Climate data must being in 13 column or long formats. See documentation for description")
  }  else {
   if (ncol(clim) == 13) {
    names <- (c("year", 1:12))
    if (!all(names == colnames(clim))) {
      stop("If climate dataframe is in 13 column format, columns must be named year and months 1-12")
   } else {
    clim <- tidyr::pivot_longer(clim, -year, names_to = "month", values_to = "value")
    clim$month <- as.character(clim$month)
   }
  }
   if (ncol(clim) == 3) {
      names <- c("year", "month", "value")
   }

      clim_small <- data.frame(cbind(year = clim$year, clim$value))
   if (!all(names == colnames(clim))) {
        stop("If climate dataframe is in long format, 3 columns must be named: year, month, value.")
      } else {
        as.character(mos)

        clim_prev <- data.frame(year = clim$year, month = as.numeric(clim$month) *-1, value = lag(clim$value, 12))

        clim <- rbind(clim, clim_prev)
        clim <- dplyr::arrange(clim, year)

        clim_small <- clim[which(clim$month %in% mos), ]

      }


  }
  if (method == "individual") {
    clim_small <- tidyr::pivot_wider(clim_small, values = month)
    if (ncol(clim) > 2){
      stop("For individual month option, select only one month at a time")
    }

  }

  if (method == "mean") {
    clim <- tidyr::pivot_wider(clim_small, names_from = month)
    values  <- clim %>%
     dplyr::select(-year)

     values <- rowMeans(values, na.rm = TRUE)

     clim_small <- data.frame(cbind(year = clim$year, values = values))
     #clim_small <- dplyr::filter(clim_small, year %in% full)
  }

  if (method == "sum") {
    clim <- tidyr::pivot_wider(clim_small, names_from = month)
    values  <- clim %>%
     dplyr::select(-year)

    values <- rowSums(sums, na.rm = TRUE)

    clim_small <- data.frame(cbind(year = clim$year, values = values))
    #clim_small <- dplyr::filter(clim_small, year %in% full)
}
    if (isTRUE(prewhiten_clim)){

      x <- data.frame(clim_small[ , 2])
      x.ar <- apply(x, 2, ar_prewhiten, return = "both")

      values <- x.ar$clim_small...2.[[1]]
      ar <- x.ar$clim_small...2.[[2]]


      clim_return <- list(clim_small = clim_small, clim_ar = ar, prewhiten_clim = prewhiten_clim)
    } else {
    clim_return <- list(clim_small = clim_small,  prewhiten_clim = prewhiten_clim)
  }
  return(clim_return)
}


