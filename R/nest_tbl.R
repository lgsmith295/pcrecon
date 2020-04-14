#' Creates dataframe of chronology names, start years, and end years from chronology dataframe
#'
#' @param select_crns_cor dataframe of chronology values where the first column is years, and each subsequent column is a chronologies
#'
#' @return dataframe of chronology names, start years, and end years
#'
#' @examples
nest_tbl <- function(select_crns_cor) {
  rows <- ncol(select_crns_cor)-1
  periods_df <- as.data.frame(matrix(ncol = 3, nrow = rows))
  colnames(periods_df) <- c("ID", "startYR", "endYR")

  for (i in 2:ncol(select_crns_cor)) {
    logic <- !is.na(select_crns_cor[ ,i])
    years <- PCA_chrons$year[logic]
    start <- as.numeric(years[1])
    end <- as.numeric(tail(years, n=1))
    periods_df[i-1, 1] <- colnames(PCA_chrons[i])
    periods_df[i-1, 2] <- start
    periods_df[i-1, 3] <- end
  }

  periods_df <- periods_df %>%
    arrange(desc(startYR))

  return(periods_df)
}
