
#' Spatial Filtering of Tree Ring Data 
#'
#' @param ID vector of character strings containing chronology IDs
#' @param lat numeric vector of latitudes for each chronology
#' @param lon numeric vector of longitudes for each chronology
#' @param center center point for calculating radius (radius type)
#' @param radius distance from center in km, default 150 (radius type)
#' @param climate dataframe with climate variables for spatial correlation (footprint type), defaults to NULL 
#' @param type "radius" or "footprint", defaults to radius
#'
#' @return
#' @export
#'
#' @examples
filter_spat <- function(ID, lat, lon, center, radius, climate = NULL, type = "radius") {
  # is it best to have them read in three vectors for ID, lat, long - requiring that they be the same length? 
  df <- data.frame(cbind(ID, lat, lon))
  if (type == "radius") {
    
    
    
    
  } else {
    
    
    
    
    
  
  }
}