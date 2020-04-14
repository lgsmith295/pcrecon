#' Load in Climate Variable
#'
#' @param climate dataframe or matrix in either 13 column (where there is a column for year and for each month) or long (where there are columns for year, month, and value) formats.
#' @param mos months of interest
#' @param type output either individual months, sums of months, or means. "individual", "sum", or "mean"
#'
#'
#' @return dataframe of monthly data
#' @export
#'
#' @examples
load_clim <- function(climate, mos, type) {
  if (!(ncol(climate) %in% c(3,13))) {
    stop("Climate data must being in 13 column or long formats. See documentation for description")
  }  else {
   if (ncol(climate) == 13) {
    names <- (c("year", 1:12))
   }
    if (!all(names == colnames(climate))) {
      stop("If climate dataframe is in 13 column format, columns must be named year and months 1-12")
   } else {
    climate <- tidyr::pivot_longer(climate, -year, names_to = "month", values_to = "value")
    climate$month <- as.character(climate$month)
    }
   if (ncol(climate) == 3) {
      names <- c("year", "month", "value")
   }
   if (!all(names == colnames(climate))) {
        stop("If climate dataframe is in long format, 3 columns must be named: year, month, value.")
      } else {
   as.character(mos)
   clim_small <- climate[which(climate$month == mos), ]
  }
  if (type == "individual") {
    clim <- tidyr::pivot_wider(clim_small, names_from = month)
  }

  if (type == "mean") {
    clim <- tidyr::pivot_wider(clim_small, names_from = month)
    means  <- clim %>%
     dplyr::select(-year)

    means <- dplyr::mutate(means, means = rowMeans(means))

    year <- clim$year
    mean <- means$means

   clim <- data.frame(cbind(year, mean))
  }

  if (type == "sum") {
    clim <- tidyr::pivot_wider(clim_small, names_from = month)
    sums  <- clim %>%
      dplyr::select(-year)

    sums <- dplyr::mutate(sums, sums = rowSums(sums))

    year <- clim$year
    sum <- sums$sums

    clim <- data.frame(cbind(year, sum))

  }
   }

 return(clim)

}

