
#' Spatial Filtering of Tree Ring Data Based on Radius From a Point
#'
#' @param data  dataframe containing columns labelled ID, lat
#' @param cent_lat latitude of center point
#' @param cent_lon longitude of center point
#' @param radius distance from center in km, default 150 (radius type)
#'
#' @return
#' @export
#'
#' @examples
#'
filter_rad <- function(data, cent_lat, cent_lon, radius, climate = NULL, type = "radius") {
  # parse dataframe and store as spatial object
  lat <- data$lat
  lon <- data$lon
  ID <- data$ID

  stores <- sf::st_sfc(st_multipoint(cbind(lon, lat)), crs = 4326)
  me <- st_sfc(st_point(c(cent_lon, cent_lat)), crs = 4326)

  #convert to albers equal area projection
  stores_aea <- st_transform(stores, "+proj=aea +lat_1=29.5 +lat_2=42.5")
  me_aea     <- st_transform(me, "+proj=aea +lat_1=29.5 +lat_2=42.5")

  #store as individual points and attach IDs
  tr_points <- st_cast(stores_aea, "POINT") %>%
    st_sf(cbind(ID, stores_aea_column))

  # circle from radius and select points within
  circle <- st_buffer(me_aea, radius)
  logical <- st_contains(circle, tr_points, sparse = FALSE)
  crns <- dplyr::filter(names, logical)
  return (crns)
}


filter_foot <- function(data, footprint, r, alpha)





