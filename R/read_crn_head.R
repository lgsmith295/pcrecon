#' Parse chronology file headers
#'
#' @param fname character string of full path to file and including file ending
#'
#' @return dataframe of site_id, site_name, species_code, type_c, type_m, elev, lat_lon, years, first, last, fname
#' @export
#'
#' @examples
#'  read_crn_head(system.file("extdata/crns/nm575.crn", package = "pcreg"))
read_crn_head <- function(fname) {
  header <- readLines(fname, n = 4)
  crn <- try(read_crn(fname))
  if(class(crn)[1]!='try-error')
    return(tibble::tibble(site_id = substr(header[[1]],1,6),
                      site_name = substr(header[[1]], 10, 61),
                      species_code = substr(header[[1]], 62, 65),
                      type_c = substr(header[[2]], 63, 63),
                      type_m = substr(header[[3]], 62, 62),
                      elev = substr(header[[2]], 42,46),
                      lat_lon = substr(header[[2]], 48,57),
                      years = substr(header[[2]], 68, 76),
                      first = row.names(crn)[1],
                      last = tail(row.names(crn),1),
                      fname = basename(fname)))
  else return(NULL)
}
