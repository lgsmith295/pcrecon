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
    if (!all(names == colnames(clim))) {
        stop("If climate dataframe is in long format, 3 columns must be named: year, month, value.")
    }
   }

  }
  if (method == "individual") {
    clim_small <- tidyr::pivot_wider(clim_small, values = month)
    if (ncol(clim) > 2){
      stop("For individual month option, select only one month at a time")
    }

  mos <- convert_mos(mos)
  if(mos[1] < 0){
    prev_year <- lag(clim$value, n = 12)

    clim_prev <- cbind(clim[ ,1:2], prev_year) %>%
      mutate(month_prev = month * -1) %>%
      dplyr::select(-month)

    clim_prev_small <- clim[which(clim$month %in% mos), ]
    clim_curr_small <- clim[which(clim$month %in% mos), ]

    clim_small <- full_join(clim_prev_small, clim_curr_small, by = year)


  } else {

  clim_small <- clim[which(clim$month %in% mos), ]

    #clim_small <- data.frame(cbind(year = clim$year, clim$value))

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

#' convert_mos
#'
#' @param mos
#'
#' @return
#'
#'
#' @examples
convert_mos <- function(mos) {
  if(mos[1] < 0) {
    prev <- sort(c(-12:mos[1]), decreasing = TRUE)
    curr <- c(1:utils::tail(mos, n=1))
    mos <- c(prev,curr)
    return(mos)
  } else {
    return(mos)

  }
}
