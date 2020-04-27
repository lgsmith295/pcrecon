#' Load crn chronology files into R
#'
#' @description load_crns reads a list of `.crn` files into R and makes them into a dataframe
#'
#' @aliases load_crns
#' @usage load_crns(dir, crns, type_crn = "S", type_measure = "R",
#'     logfile = "read_crns.log")
#'
#' @param dir path to directory containing chronologies as character string
#' @param crns vector of character strings containing the names of the chronologies wanting to read into R
#' @param type_crn character code indicating type of chronologies to include. Default = "S" (standard). See details for available options (from ITRDB)
#' @param type_measure character code indicating type of measurements to include. Default = "R" (Ring Width). See details for available options (from ITRDB)
#' @param logfile character string indicating path to the log file for errors associated with reading in chronologies. The path is the same as `dir` if not otherwise specified.
#'
#' @return dataframe with a year column and columns for each chronology containing values
#' @export
#' @importFrom magrittr %>%
#' @import dplR
#' @import stringr
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @details coming soon
#'
#'     There is no clear and consistent way that metadata about the measurement type and chronology type are clearly denoted in the ITRDB. The filename is potentially the most consistent, with the file header being the next most consistent for the measurement type, but not the standardization type.
#'
#'     Code Measurement Type (`type_measure`)
#'     D Total Ring Density
#'     E Earlywood Width
#'     I Earlywood Density
#'     L Latewood Width
#'     N Minimum Density
#'     R Ring Width
#'     T Latewood Density
#'     X Maximum Density
#'     P Latewood Percent
#'
#'     Code Chronology Type (`type_crn`)
#'     A ARSTND
#'     P Low Pass Filter
#'     R Residual
#'     S Standard
#'     W Re-Whitened Residual
#'     N Measurements Only
#'
#' @examples
#' df <- load_crns(dir = system.file("extdata/crns",
#' package = "pcreg"),
#' crns = c("nm014", "nm537", "nm545", "nm576"),
#'  type_crn = "S",
#'  type_measure = "R")
load_crns <- function(dir, crns, type_crn = "S", type_measure = "R", logfile = "read_crns.log") {

  if(!(type_crn %in% c("S", "A"))) stop("Chronology type must be specified as 'S' or 'A'.")
  if(!(type_measure %in% c("R", "E", "L", "T", "X"))) stop("Chronology type must be specified as 'E', 'L', 'T', or 'X'.")
  if(type_crn == "A" & type_measure != "R") stop("Chronology type must be Standard ('S') when using earlywood, latewood, or density measurements")

  if(type_measure %in% c("E", "L", "T", "X")) {
    warning("Chronology type is assumed to be Standard ('S') when using earlywood, latewood, or density measurements but this is not explicitly specified in the ITRDB")
    type_crn <- "S"
  }

  crns <- stringr::str_remove(basename(crns), "\\.crn$")

  # type_crn2 <- type_crn
  # type_measure2 <- type_measure
  # if("S" %in% type_crn) type_crn2 <- c(type_crn, " ", "", "_")
  # if("R" %in% type_measure) type_measure2 <- c(type_measure, " ", "", "_")

  # noaa data are so different maybe use separate function for those

  log_con <- file.path(dir, logfile)
  cat(paste0("Log File for Reading Chronologies: ", Sys.time()), file = log_con, sep = "\n", append = FALSE)

  # Vector of location names (e.g. nm537) from spatial filtering = crns
  # Vector of all filenames in the directory = files
  files <- list.files(path = dir)

  # Remove NOAA files
  files <- files[!(stringr::str_detect(files, "-noaa"))]

  # Filter to filenames containing the locations names (e.g. nm537.crn and nm537r.crn)
  files <- files[stringr::str_detect(files, regex(paste(crns, collapse = "|"), ignore_case = TRUE))]
  files <- files[stringr::str_detect(files, "\\.crn$")]

  # stringr::str_detect(files[i], "^[[:alpha:]]{2}\\.crn") # statecode.crn = standard

  # files <- c("nm.crn", "ar.crn", "ar01.crn", "nm01.crn", "nm01r.crn")
  tmp1 <- tibble::tibble(fname = files) %>%
    dplyr::mutate(site = stringr::str_extract(fname, "^[[:alpha:]]{2}[[:digit:]]{0,}"),
                    type_fname = dplyr::if_else(is.na(stringr::str_extract(fname, "^[[:alpha:]]{2}\\.crn")), stringr::str_extract(fname, "[[:alpha:]]+(?=\\.crn)"), NA_character_))

  # files <- files[grepl('[[:digit:]]\\.crn', files)]

  # Parse those headers using Nicholas' read_crn_head
  tmp2 <- lapply(file.path(dir, files), read_crn_head)
  # rbind that into a data.frame
  tmp2 <- do.call(rbind, tmp2)

  df <- tmp1 %>%
    dplyr::left_join(data.frame(tmp2, stringsAsFactors = FALSE), by = "fname") %>%
    dplyr::select(fname, sp_code, site, type_fname, type_m, type_c, lat_lon) %>%
    dplyr::mutate(type_fname = stringr::str_to_upper(type_fname),
                  type_m = dplyr::na_if(type_m, " "),
                  type_c = dplyr::na_if(type_c, " ")) %>%
    dplyr::mutate(match_m = type_m == type_fname)

  # Filter by measurement types and chronology types & to have valid lat-lon strings and valid year ranges (year filter not implemented)
  df <- df %>%
    dplyr::mutate(type_fname = dplyr::if_else(is.na(type_fname), "S", type_fname),
                  type_m = dplyr::if_else(is.na(type_m), "R", type_m))

  # if(type_measure != "R") {
  #   df <- df %>%
  #     dplyr::filter(type_m == type_measure) #, # this filter will fail when header line 3 author names happen to have an "e" or "l" or "x" in that position
                  # type_c %in% type_crn,
                  # grepl(lat_lon, pattern='[[:digit:]]{4,4}-[[:digit:]]{5,5}'))
  # } # this makes the big assumption that a filename ending with "r.crn" always means a residual chronology and never "ring-width" chronology type.

if(type_measure == "R") {
  df <- df %>%
    dplyr::filter(!(type_fname %in% c("R", "E", "L", "T", "X")))
}

if(type_measure %in% c("E", "L", "T", "X")) {
  df <- df %>%
    dplyr::filter(type_fname == type_measure | type_m == type_measure,
                  type_fname != "A")
  }

  if(type_crn %in% c("S")) {
    df <- df %>%
      dplyr::filter(type_fname %in% c("S", "E", "L", "T", "X"))
  }

  if(type_crn %in% c("A")) {
    df <- df %>%
      dplyr::filter(type_fname == type_crn)
  }

  # tryCatch - if df returns nothing

  # Create properly formatted lat/lon coordinates (convert from dddmm to decimal)
  df <- df %>%
    # dplyr::mutate(lat = as.numeric(substr(lat_lon, 1, 2)) + as.numeric(substr(lat_lon, 3, 4)) / 60,
    #        lon=as.numeric(substr(lat_lon, 6, 8)) + as.numeric(substr(lat_lon, 9, 10)) / 60) %>%
    dplyr::select(-lat_lon)

  # Pass the resulting list of filenames to the read_crn function
  crns_files <- df$fname

  # head_problems <- crns[!(crns %in% names(df))]
  # cat(paste0("\n", " Problems parsing headers and therefore not read in: ", head_problems), file = log_con, sep = ",", append = TRUE)

  try(pb <- txtProgressBar(min = 0, max = length(crns_files), style = 3))
  j <- 0
  success_sites <- NULL
  for(i in 1:length(crns_files)) {
    setTxtProgressBar(pb, i)
    chron <- tryCatch(
      expr = {
        withCallingHandlers(suppressMessages(read_crn(file.path(dir, crns_files[i]))))
        },
      error = function(e) {
        e
        }
      )
    if(inherits(chron, "error")) {
      # warning(paste0("Not able to read chronology ", crns[i], " - skipping"))
      cat(paste0(crns_files[i], ": ", chron[["message"]]), file = log_con, sep = "\n", append = TRUE)
      next
    } else {
      j <- j + 1
    colnames(chron)[1] <- stringr::str_remove(crns_files[i], "\\.crn$")
    chron <- tibble::rownames_to_column(chron, "year") %>%
      dplyr::select(-samp.depth) %>%
      dplyr::mutate(year = as.integer(as.numeric(year)))

    # check that years make sense

    success_sites[j] <- df$site[i]
    # Moved from read_crn_head
    # first = row.names(crn)[1]
    # last = tail(row.names(crn), 1)

    }
    if(j == 1) {
      all_chronologies <- chron
    } else {
      all_chronologies <- dplyr::full_join(all_chronologies, chron, by = "year")
    }
  }
  # close(log_con)

  # failed <- crns[!(regex(crns, ignore_case = TRUE) %in% colnames(all_chronologies))]
  failed <- crns[!(stringr::str_detect(crns, regex(paste(success_sites, collapse = "|"), ignore_case = TRUE)))]
  # failed <- crns[!(regex(crns, ignore_case = TRUE) %in% success_sites)]
  if(length(failed) == 0) {
    cat(paste0("\n All sites successfully read in."), file = log_con, sep = "\n", append = TRUE)
  } else {
    cat(paste(c("\n Sites attempted but not successfully read in:", failed),  collapse = " "), file = log_con, append = TRUE)
  }

  close(pb)
  return(all_chronologies)
}
