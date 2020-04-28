#' Parse chronology file headers
#'
#' @param fname character string of full path to file and including file ending
#' @param err character string indicating type of error handling where `stop` will throw and error, `warn` will print warning to the screen, and `log` will pass the test for use in a log file. Defaults to `log`.
#' @param logfile character string indicating path to the log file for errors associated with reading in chronologies. The path is the same as `dir` if not otherwise specified.
#'
#' @return dataframe of fname, site_id, site_name, sp_code (species code), type_c (chronology type), type_m (measurement type), elev, lat_lon, years
#' @export
#'
#' @examples
#'  read_crn_head(system.file("extdata/crns/nm575.crn", package = "pcreg"))
read_crn_head <- function(fname, err = "warn", logfile = "read_crns.log") {

  if(!(err %in% c("stop", "warn", "log"))) {
    stop("err must be indicated as either stop, warn, or log")
  }

  header <- readLines(fname, n = 4)
  if(length(header) < 4) {
    mes <- paste0(basename(fname), " has fewer than 4 lines")
    tmp <- base::switch(err,
                        warn = {
                          warning(mes)
                          # return(NULL)
                        },
                        stop = stop(mes),
                        log = {
                          cat(mes, file = file.path(dirname(fname), logfile), sep = "\n", append = TRUE)
                          # return(NULL)
                        }
    )
  }
  # } else {

  # crn <- try(read_crn(fname))
  #
  # if(class(crn)[1]!='try-error')
    return(tibble::tibble(site_id = substr(header[[1]],1,6),
                      site_name = substr(header[[1]], 10, 61),
                      sp_code = substr(header[[1]], 62, 65),
                      type_c = as.character(substr(header[[2]], 63, 63)),
                      type_m = as.character(substr(header[[3]], 62, 62)),
                      elev = substr(header[[2]], 42,46),
                      lat_lon = substr(header[[2]], 48,57),
                      years = substr(header[[2]], 68, 76),
                      fname = basename(fname)))
  # else return(NULL)
  # }
}
