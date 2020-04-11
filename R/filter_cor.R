#' Title
#'
#' @param dir path to directory containing chronologies as character string
#' @param crns vector of character strings containing the names of the chronologies wanting to read into R
#' @param lag vector of positive integers indicating which lags should be included. Defaults to zero.
#' @param lead vector of positive integers indicating which leads should be included. Defaults to current year and one
#' @param include 
#'
#' @return dataframe with a year column and columns for each chronology and their leads and lags
#' @export
#' 
#' @details The lag is the number of years to shift the chronology backwards. It defaults to zero because relative to the climate you do not expect the previous tree rings to correlate to current year climate. The lead is the number of years to shift the chronology forward such that this years climate conditions would influence growth in those future years. For example, `lead = 0` would just to the current year. Making `lead = 1` would ONLY put in a chronology shifted one year ahead of the climate (e.g. 2010 climate would affect 2011 tree growth). Setting `lead = c(0, 1, 2)` would create three columns such that the current climate would affect current year tree rings AND tree rings the next year and the year after.
#'
#' @examples
#' \dontrun{
#' 
#' }
load_crns <- function(dir, crns, lag = 0, lead = 1, include = NULL) {

pb <- txtProgressBar(min = 0, max = length(crns), style = 3)
for (i in 1:length(crns)) {
  setTxtProgressBar(pb, i)
  chron <- read.crn(paste0(dir, crns[i]))
  colnames(chron)[1] <- crns[i]
  lag_name <- paste0(file_names_chron[i], "_lag")
  chron <- rownames_to_column(chron, "year") %>%
    dplyr::select(-samp.depth) %>%
    mutate(year = as.integer(as.numeric(year))) %>%
    mutate(!!lag_name := dplyr::lead(chron[ ,1], 1))
  if(i == 1) {
    all_chronologies <- chron
  } else {
    all_chronologies <- full_join(all_chronologies, chron, by = "year")
  }
}
close(pb)
return(all_chronologies)
}
