#' Title
#'
#' @description
#'
#' @usage
#'
#' @param dir path to directory containing chronologies as character string
#' @param crns vector of character strings containing the names of the chronologies wanting to read into R
#' @param include
#' @param logfile character string indicating path to the log file for errors associated with reading in chronologies. The path is the same as `dir` if not otherwise specified.
#'
#' @return dataframe with a year column and columns for each chronology containing values
#' @export
#' @importFrom magrittr %>%
#' @import dplR
#'
#' @details coming soon
#'
#' @examples
#' \dontrun{
#' df <- load_crns(dir = system.file("extdata/crns", package = "pcreg", mustWork = TRUE), crns = list.files(system.file("extdata/crns", package = "pcreg", mustWork = TRUE)))
#' }
load_crns <- function(dir, crns, include = NULL, logfile = "read_crns.log") {
  log_con <- file.path(dir, logfile)
  cat(paste0("Log File for Reading Chronologies: ", Sys.time()), file = log_con, sep = "\n", append = FALSE)
  pb <- txtProgressBar(min = 0, max = length(crns), style = 3)
  j <- 0
  for (i in 1:length(crns)) {
    setTxtProgressBar(pb, i)
    chron <- tryCatch(withCallingHandlers(suppressMessages(read_crn(file.path(dir, crns[i])))), error = function(e) e)
    if(inherits(chron, "error")) {
      # warning(paste0("Not able to read chronology ", crns[i], " - skipping"))
      cat(paste0(crns[i], ": ", chron[["message"]]), file = log_con, sep = "\n", append = TRUE)
      next
    } else {
      j <- j + 1
    colnames(chron)[1] <- crns[i]
    chron <- tibble::rownames_to_column(chron, "year") %>%
      dplyr::select(-samp.depth) %>%
      dplyr::mutate(year = as.integer(as.numeric(year)))
    }
    if(j == 1) {
      all_chronologies <- chron
    } else {
      all_chronologies <- dplyr::full_join(all_chronologies, chron, by = "year")
    }
  }
  # close(log_con)
  close(pb)
  return(all_chronologies)
}
