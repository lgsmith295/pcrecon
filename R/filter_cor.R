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
    dplyr::select(-starts_with('year',ignore.case = TRUE))

  clim_window <- clim %>%
    dplyr::filter(year %in% window) %>% ## starts_with doesn't work here as below. why?
    dplyr::select(-starts_with('year',ignore.case = TRUE))

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
    clim<- as.vector(as.numeric(clim_window[ , ])) #subset to month j for when there are more than one month
    cor <- cor.test( clim, dplyr::lead(crn, leads[j]), conf.level = alpha, type = type, alternative = alternative)
    cors_table[k, ] <- cbind(crn_names[i], leads[j], cor$estimate, cor$p.value)
}
}
    cors_table_small <- filter(cors_table, cors_table$correlation >= r ) %>%
      filter(p_value <= 1-alpha)

    keepers <- c("year", cors_table_small$chronology)

    select_crns_cor <- subset(crns, select=keepers)

    select_crns_cor <- PCA_chrons %>%
      arrange(year)

    nests_df <- nest_tbl(select_crns_cor)

    select_crns_cor <- select_crns_cor %>%
      filter(year >= min(nests_df$startYR)) %>%
      filter(year <= max(nests_df$endYR))
    list <- list(cors_table = cors_table, cors_table_small = cors_table_small, select_crns_cor_sub = select_crns_cor_sub)

    return(list)


}





