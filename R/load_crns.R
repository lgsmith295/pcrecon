##### fill table with all chronology and lead 1 chronologies

load_crns <- function(dir, crns, lag, lead, include) {
  
  for (i in 1:length(crns)) {
    chron <- read.crn(paste0(dir, crns[i]))
    colnames(chron)[1] <- crns[i]
    lag_name <- paste0(file_names_chron[i], "_lag")
    chron <- rownames_to_column(chron, "year") %>%
      dplyr::select(-samp.depth) %>%
      mutate(year = as.integer(as.numeric(year))) %>%
      mutate(!!lag_name := lead(chron[ ,1], 1))
    if(i == 1) {
      all_chronologies <- chron
    } else {
      all_chronologies <- full_join(all_chronologies, chron, by = "year")
    }
  }

}
