#' Parse metadata json files from ITRDB
#'
#' @param dir file directory where json files are saved
#' @param center logical whether to report the center of the field site as a single lat/lon or a full location bounding box. Defaults to TRUE.
#'
#' @return dataframe of ID, lat, lon, elevation
#' @import jsonlite
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
parse_json <- function(dir, center = TRUE) {
  # make list of files to import
  files <- list.files(dir)

  # remove manifest
  files <- files[which(files != "manifest.json")]
  files <- files[stringr::str_detect(files, "\\.json$")] # only json files
  # files <- files[!(stringr::str_detect(files, "-noaa"))] # deal with noaa separately?

  n_files <- length(files)

  # set up empty item to make into a dataframe
  df_meta <- NULL

  # loop through files
  for(i in 1:n_files) {
    df_flat <- jsonlite::fromJSON(file.path(dir, files[i]), flatten = TRUE)

    # extract site info
    ID <- df_flat$site$paleoData[[1]][1:10]
    names(ID)[names(ID) == "dataTableName"] <- "ID"

    # species info
    species <- df_flat$site$paleoData[[1]]$species[[1]][1:2]

    # geographic info
    geo <- data.frame(cbind(
      southernmostLatitude = as.numeric(df_flat$site$geo.properties.southernmostLatitude),
      northernmostLatitude = as.numeric(df_flat$site$geo.properties.northernmostLatitude),
      westernmostLongitude = as.numeric(df_flat$site$geo.properties.westernmostLongitude),
      easternmostLongitude = as.numeric(df_flat$site$geo.properties.easternmostLongitude),
      minElevationMeters = as.numeric(df_flat$site$geo.properties.minElevationMeters),
      maxElevationMeters = as.numeric(df_flat$site$geo.properties.maxElevationMeters)),
      stringsAsFactors = FALSE)

    df_temp <- dplyr::bind_cols(ID, species, geo) %>% # can add more here as desired
      dplyr::mutate(chron_length = mostRecentYear - earliestYear)

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


