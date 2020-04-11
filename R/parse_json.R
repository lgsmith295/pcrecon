

#' Parse metadata json files from ITRDB 
#'
#' @param dir file directory where json files are saved
#'
#' @return dataframe of ID, lat, lon, elevation
#' @export
#'
#' @examples
parse_json <- function(dir) {
  # make list of files to import
  files <- list.files(dir)
  
  # remove manifest
  files <- files[which(files != "manifest.json")]
  n_files <- length(files)
  
  # set up empty item to make into a dataframe
  df_meta <- NULL
  
  # loop through files
  for(i in 1:n_files) {
    df_flat <- jsonlite::fromJSON(paste0(dir, "/", files[i]), flatten = TRUE)
    
    # extract site info
  site <- df_flat$site$paleoData[[1]][1:10] 
   
  # species info
  species <- df_flat$site$paleoData[[1]]$species[[1]][1:2]
  
  # geographic info
  geo <- data.frame(cbind(southernmostLatitude = as.numeric(df_flat$site$geo.properties.southernmostLatitude),
                    northernmostLatitude = as.numeric(df_flat$site$geo.properties.northernmostLatitude),
                    westernmostLongitude = as.numeric(df_flat$site$geo.properties.westernmostLongitude),
                    easternmostLongitude = as.numeric(df_flat$site$geo.properties.easternmostLongitude),
                    minElevationMeters = as.numeric(df_flat$site$geo.properties.minElevationMeters),
                    maxElevationMeters = as.numeric(df_flat$site$geo.properties.maxElevationMeters)),
  stringsAsFactors = FALSE)
  
  # combine info
  df_temp <- bind_cols(site, species, geo) %>% # can add more here as desired
    mutate(chron_length = mostRecentYear - earliestYear)
  
  # combine all files
  df_meta <- bind_rows(df_meta, df_temp)
  }
  
  # check how it worked
  str(df_meta)
  summary(df_meta)
  nrow(df_meta) == n_files
  
  # export dataframe
  return(df_meta)
}
