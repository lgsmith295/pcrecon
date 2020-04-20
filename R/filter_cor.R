#' Filter chronologies based on their correlation with climate data
#'
#' @param crns dataframe, such as returned by the load_crns function, where there is a column containing years and one column for each chronology
#' @param lead single value indicating the maximum number of years to shift chronology data forward. 0 indicates no lead, 2 will evaluate 0, 1, and 2 year leads, etc. Defaults to 1.
#' @param clim dataframe, such as returned by the get_clim function, containing columns for year and either: individual months climate data (may be more than one), a sum of >1 months climate data, or the average of >1 months data
#' @param window years to include in correlation analysis, calibration period in original PCreg software
#' @param method type of correlation analysis, "pearson" or "spearman". Defaults to "pearson"
#' @param alternative indicates the alternative hypothesiss and must be one of "two.sided", "greater", or "less".
#' @param r minimum correlation coefficient needed to retain chronology. Defaults to 0.25.
#' @param alpha minimum alpha value for rejection of null hypothesis. Defaults to 0.90
#'
#' @return
#' @export
#'
#' @examples
filter_cor <- function(crns, lead = 1, clim, cor.window, type = "pearson", alternative = "two.sided", r = 0.25, alpha = 0.90, prewhiten.crn = TRUE, prewhiten.clim = TRUE, calib = calib, valid = valid, full = full){

if(isTRUE(prewhiten.crn)){
    year <- crns$year
    x <- dplyr::select(crns, -year)
    ar <- apply(x, 2, ar_prewhiten, return = "resid")
    crns <- data.frame(cbind(year, ar))
    ar_keep <- apply(x, 2, ar_prewhiten, return = "model")
}

  df <- window_filter(crns = crns, clim = clim, cor.window = cor.window, calib = calib, valid = valid, full = full)

  leads <- c(0:lead)

  rows <- ncol(df$crn_window) * length(leads)

  cors_table <- as.data.frame(matrix(NA, nrow = rows, ncol = 4))
  names <-  c("chronology", "leads", "correlation", "p_value")
  colnames(cors_table) <- names

  crn_names <- colnames(df$crn_window)

 k <- 0
 for (j in 1:length(leads)) {
  for (i in 1:length(crn_names)) {
    k <- k + 1
    crn <- as.vector(as.numeric(df$crn_window[ ,i]))
    clim<- as.vector(as.numeric(df$clim_window[ , ]))
    cor <- cor.test( clim, dplyr::lead(crn, leads[j]), conf.level = alpha, type = type, alternative = alternative)
    cors_table[k, ] <- cbind(crn_names[i], leads[j], cor$estimate, cor$p.value)
}
}
    cors_table_small <- dplyr::filter(cors_table, cors_table$correlation >= r ) %>%
      dplyr::filter(p_value <= 1-alpha)

    select_crns <- crns_table(crns, cors_table_small)

    nests <- nest_tbl(select_crns)

    # filter to just those years that have at least one tree ring measurement
    select_crns <- select_crns %>%
      dplyr::filter(year >= min(nests$startYR)) %>%
      dplyr::filter(year <= max(nests$endYR))

    list <- list(cors_table = cors_table, cors_table_small = cors_table_small, select_crns = select_crns, nests = nests)
    if(isTRUE(prewhiten.clim)){
    list <- list(cors_table = cors_table, cors_table_small = cors_table_small, select_crns = select_crns, nests = nests, crn.ar = ar_keep)
    }
    class(list) <- "PCR_crns"

    return(list)

}



#' Fill table of chronologies and lead chronologies that pass correlation filtering
#'
#' @param crns
#' @param cors_table_small dataframe of filtered chronology names, leads, correlation, and p values.
#'
#' @return dataframe of chronologies, including lead chronologies, that met threshold correlation values for analysis
#'
#'
#' @examples
#'
crns_table <- function(crns = crns, cors_table_small = cors_table_small) {
 for (i in 1:nrow(cors_table_small)) {
  chron <- crns[ ,cors_table_small$chronology[i]]
  if (cors_table_small$leads[i] != 0) {
    lead_val <- as.integer(cors_table_small$leads[i])
    chronology_name <- paste0(cors_table_small$chronology[i], "_lag", lead_val)
    chron_lead <- dplyr::lead(chron, lead_val )
    chron <- data.frame(chron_lead)
    colnames(chron) <- chronology_name
  } else {
  chronology_name <- cors_table_small$chronology[i]
  chron <- data.frame(chron)
  colnames(chron) <- chronology_name
  }
  if (i == 1) {
    crns_select <- cbind(year = crns$year, chron)
  } else {
    crns_select <- cbind(crns_select, chron)
  }

 }
  crns_select <- dplyr::arrange(crns_select, year)
  return (crns_select)

}

#' Select windows for screening by correlation
#'
#' @param crns
#' @param cor.window
#' @param clim
#'
#' @return
#'
#' @examples
window_filter <- function(crns, clim, cor.window, calib, valid, full){

  if(!(cor.window %in% c("calib", "valid", "full"))) {
    stop("cor.window must be indicated as either valid, calib, or full")
  }

  crn_window <- switch(cor.window,
                       calib = dplyr::filter(crns, year %in% calib),
                       valid = dplyr::filter(crns, year %in% valid),
                       full = dplyr::filter(crns, year %in% full))

  clim_window <- switch(cor.window,
                       calib = dplyr::filter(clim, year %in% calib),
                       valid = dplyr::filter(clim, year %in% valid),
                       full = dplyr::filter(clim, year %in% full))

  crn_window <- crn_window %>%
    dplyr::select(-dplyr::starts_with('year',ignore.case = TRUE))

  clim_window <- clim_window %>%
    dplyr::select(-dplyr::starts_with('year',ignore.case = TRUE))

  df <- list(crn_window = crn_window, clim_window = clim_window)
}
