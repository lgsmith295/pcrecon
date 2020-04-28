#' Parse metadata json files from ITRDB
#'
#' @param dir file directory where json files are saved
#' @param center logical whether to report the center of the field site as a single lat/lon or a full location bounding box. Defaults to TRUE.
#' @param logfile directory to the log file. If NULL uses the current working directory.
#'
#' @return dataframe of ID, lat, lon, elevation
#' @import jsonlite
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
parse_json <- function(dir, center = TRUE, logfile = NULL) {

  if(is.null(logfile)) {
    log_con <- file.path(getwd(), "parse_json.log")
  } else {
    log_con <- file.path(logfile, "parse_json.log")
  }
  cat(paste0("Log File for Parsing Metadata: ", Sys.time()), file = log_con, sep = "\n", append = FALSE)

  if(!dir.exists(dir)) {
    cat("directory does not exist", file = log_con, sep = "\n", append = TRUE)
    stop("directory does not exist")
  }

  # make list of files to import
  files <- list.files(dir)
  if(length(files) == 0) {
    cat("directory is empty", file = log_con, sep = "\n", append = TRUE)
    stop("directory is empty")
  }

  # remove manifest
  files <- files[which(files != "manifest.json")]
  files <- files[stringr::str_detect(files, "\\.json$")] # only json files
  # files <- files[!(stringr::str_detect(files, "-noaa"))] # deal with noaa separately?

  n_files <- length(files)
  cat(paste0("\n Attempting to parse ", n_files, " files.\n"), file = log_con, sep = "\n", append = TRUE)

  # set up empty item to make into a dataframe
  df_meta <- NULL

  # loop through files
  j <- 0
  for(i in 1:n_files) {
    df_flat <- try(jsonlite::fromJSON(file.path(dir, files[i]), flatten = TRUE))
    if(class(df_flat) == "try-error") {
      cat(paste0(files[i], " fails to read in with jsonlite::fromJSON"), file = log_con, sep = "\n", append = TRUE)
      next
    }
    j <- j + 1

    # extract site info
    ID <- try(df_flat$site$paleoData[[1]][1:10])
    if(class(ID) == "try-error") {
      cat(paste0(files[i], ": cannot parse file - skipping"), file = log_con, sep = "\n", append = TRUE)
      next
    }
    if(nrow(ID) > 1) {
      cat(paste0(files[i], ": has more than 1 ID - skipping"), file = log_con, sep = "\n", append = TRUE)
      next
    }
    names(ID)[names(ID) == "dataTableName"] <- "ID"

    # species info
    species <- try(df_flat$site$paleoData[[1]]$species[[1]][1:2])
    if(class(species) == "try-error" | class(species) != "data.frame") {
      cat(paste0(files[i], ": cannot parse species info"), file = log_con, sep = "\n", append = TRUE)
      species <- data.frame(speciesCode = NA_character_, scientificName = NA_character_, stringsAsFactors = FALSE)
    }

    # geographic info
    geo <- try(data.frame(cbind(
      southernmostLatitude = as.numeric(df_flat$site$geo.properties.southernmostLatitude),
      northernmostLatitude = as.numeric(df_flat$site$geo.properties.northernmostLatitude),
      westernmostLongitude = as.numeric(df_flat$site$geo.properties.westernmostLongitude),
      easternmostLongitude = as.numeric(df_flat$site$geo.properties.easternmostLongitude),
      minElevationMeters = as.numeric(df_flat$site$geo.properties.minElevationMeters),
      maxElevationMeters = as.numeric(df_flat$site$geo.properties.maxElevationMeters)),
      stringsAsFactors = FALSE))
    if(class(geo) == "try-error" | nrow(geo) > 1) {
      cat(paste0(files[i], ": cannot parse geography info"), file = log_con, sep = "\n", append = TRUE)
      geo <- data.frame(cbind(
        southernmostLatitude = NA_real_,
        northernmostLatitude = NA_real_,
        westernmostLongitude = NA_real_,
        easternmostLongitude = NA_real_,
        minElevationMeters = NA_real_,
        maxElevationMeters = NA_real_),
        stringsAsFactors = FALSE)
    }

   df_temp <- try(dplyr::bind_cols(ID, species, geo) %>% # can add more here as desired
      dplyr::mutate(chron_length = mostRecentYear - earliestYear))
   if(class(df_temp) == "try-error") {
     cat(paste0(files[i], ": cannot parse file - skipping"), file = log_con, sep = "\n", append = TRUE)
     next
   }

    # combine all files
    df_meta <- dplyr::bind_rows(df_meta, df_temp)
  }

  # export dataframe
  if(center == TRUE) {
    df_meta$lat <- rowMeans(data.frame(df_meta$southernmostLatitude,
                                       df_meta$northernmostLatitude))
    df_meta$lon <- rowMeans(data.frame(df_meta$easternmostLongitude,
                                       df_meta$westernmostLongitude))
    df_meta$elev <- rowMeans(data.frame(df_meta$minElevationMeters,
                                        df_meta$maxElevationMeters))

    df_meta <- df_meta %>%
      dplyr::select(-maxElevationMeters,
             -minElevationMeters,
             -easternmostLongitude,
             -westernmostLongitude,
             -northernmostLatitude,
             -southernmostLatitude)
  }
  return(df_meta)
}


