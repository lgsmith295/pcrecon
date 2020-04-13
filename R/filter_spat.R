
#' Spatial Filtering of Points Based on Radius From a Point
#'
#' @param data  dataframe containing columns labeled ID, lat, and lon
#' @param cent_lat latitude of center point
#' @param cent_lon longitude of center point
#' @param radius distance from center in km, default 150
#'
#' @return character vector of chronology names within the selected radius
#' @export
#'
#' @examples
#' # metadata returned from parse_json function, must be a dataframe or matrix containing columns lat, lon, and ID
#' select_crns_rad <- filter_rad(data = metadata, cent_lat = 35.65, cent_lon = -105.32, radius = 150)
#'
filter_rad <- function(data, cent_lat, cent_lon, radius = 150, climate = NULL) {

  radius_m <- radius * 1000

  # parse dataframe and store as spatial object
  data <- data.frame(data, stringsAsFactors = FALSE)
  lat <- as.numeric(data$lat)
  lon <- as.numeric(data$lon)
  ID <- as.character(data$ID)

  stores <- sf::st_sfc(sf::st_multipoint(cbind(lon, lat)), crs = 4326)
  me <- sf::st_sfc(sf::st_point(c(cent_lon, cent_lat)), crs = 4326)

  # convert to albers equal area projection
  stores_aea <- sf::st_transform(stores, "+proj=aea +lat_1=29.5 +lat_2=42.5")
  me_aea     <- sf::st_transform(me, "+proj=aea +lat_1=29.5 +lat_2=42.5")

  # store as individual points and attach IDs
  tr_points <- sf::st_cast(stores_aea, "POINT")
  tr_points <- sf::st_sf(cbind(ID, data.frame(tr_points)))

  # circle from radius and select points within
  circle <- sf::st_buffer(me_aea, radius_m)
  logical <- sf::st_contains(circle, tr_points, sparse = FALSE)
  select_crns <- ID[which(logical == TRUE)]

  if(length(select_crns) == 0) warning("No chronologies found within this radius")

  return (select_crns)
}


#' Spatial Filtering of Points Based on Climate Footprint
#'
#' @param data dataframe containing columns labeled ID, lat, and lon
#' @param footprint Raster or SpatialPolygonDataFrame object
#' @param r minimum correlation coefficient for spatial selection
#' @param alpha alpha value , default null because you set this when making the footprint in KNMI
#'
#' @return character vector of chronology names that fall within the area where correlation >= r
#' @export
#'
#' @examples
#' # Climate footprint .nc file is a netCDF file downloaded from KNMI climate explorer with correlations between PRISM precipitation (1895-present) and streamflow at Gallinas Creek near Montezuma, NM.
#'
#' #' footprint <- raster::raster("inst/extdata/gallinas_cf.nc")
#' select_crns_fp <- filter_foot(data = metadata, footprint = footprint, r = 0.5)
#'
filter_foot <- function(data, footprint, r = 0.4, alpha = NULL) {
  # parse dataframe and store as spatial object
  lat <- data$lat
  lon <- data$lon
  ID <- data$ID

  # check class of footprint object, convert to polygon from raster if needed
  if (class(footprint) == "RasterLayer") {
    footprint <- rasterToPolygons(footprint)
  }
    crns <- sf::st_sfc(sf::st_multipoint(cbind(lon, lat)), crs = 4326)
    crns <- sf::st_transform(crns, sf::st_crs(footprint))

    tr_points <- sf::st_cast(crns, "POINT")
    tr_points <- sf::st_sf(cbind(ID, data.frame(tr_points)))


    fp = sf::st_as_sf(footprint)
    points = sf::st_as_sf(tr_points)
    intersect <- suppressMessages(suppressWarnings(data.frame(sf::st_intersection(fp, points))))

    small_df <- dplyr::filter(intersect, correlation >= r)
    select_crns <- as.character(small_df$ID, stringsAsFactors = FALSE)

    return(select_crns)
}

