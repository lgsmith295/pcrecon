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
filter_cor <- function(crns, lead = 1, clim, window, type = "pearson", alternative = "two.sided", r = 0.25, alpha = 0.90){
  crn_window <- crns %>%
    dplyr::filter(year %in% window) %>% ## starts_with doesn't work here as below. why?
    dplyr::select(-dplyr::starts_with('year',ignore.case = TRUE))

  clim_window <- clim %>%
    dplyr::filter(year %in% window) %>% ## starts_with doesn't work here as below. why?
    dplyr::select(-dplyr::starts_with('year',ignore.case = TRUE))

  leads <- c(0:lead)

  rows <- ncol(crn_window) * length(leads)

  cors_table <- as.data.frame(matrix(NA, nrow = rows, ncol = 4))
  names <-  c("chronology", "leads", "correlation", "p_value")
  colnames(cors_table) <- names

  crn_names <- colnames(crn_window)

 k <- 0
for (j in 1:length(leads)) {
  for (i in 1:length(crn_names)) {
    k <- k + 1
    crn <- as.vector(as.numeric(crn_window[ ,i]))
    clim<- as.vector(as.numeric(clim_window[ , ])) #add subset to month j for when there are more than one month
    cor <- cor.test( clim, dplyr::lead(crn, leads[j]), conf.level = alpha, type = type, alternative = alternative)
    cors_table[k, ] <- cbind(crn_names[i], leads[j], cor$estimate, cor$p.value)
}
}
    cors_table_small <- dplyr::filter(cors_table, cors_table$correlation >= r ) %>%
      dplyr::filter(p_value <= 1-alpha)

    select_crns <- crns_table(cors_table_small)

    nests_df <- nest_tbl(select_crns)

    select_crns <- select_crns %>%
      dplyr::filter(year >= min(nests_df$startYR)) %>%
      dplyr::filter(year <= max(nests_df$endYR))

    list <- list(cors_table = cors_table, cors_table_small = cors_table_small, select_crns = select_crns)

    return(list)


}



#' Fill table of chronologies and lead chronologies based on correlation output
#'
#' @param cors_table_small dataframe of filtered chronology names, leads, correlation, and p values.
#'
#' @return dataframe of chronologies, including lead chronologies, that met threshold correlation values for analysis
#'
#'
#' @examples
#'
crns_table <- function(cors_table_small) {
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
  return (crns_select)

}

