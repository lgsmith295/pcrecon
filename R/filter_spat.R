
#' Spatial Filtering of Points Based on Radius From a Point
#'
#' @param cent_lat latitude of center point
#' @param cent_lon longitude of center point
#' @param radius distance from center in km, default 150
#' @param x
#' @param climate
#' @param plot
#' @param buff
#'
#' @return character vector of chronology names within the selected radius
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' # metadata returned from parse_json function, must be a dataframe or matrix containing columns lat, lon, and ID
#' select_crns_rad <- filter_rad(data = metadata, cent_lat = 35.65, cent_lon = -105.32, radius = 150)
#'
filter_rad <- function(x, cent_lat, cent_lon, radius = 150, climate = NULL, plot = TRUE, buff = 5) {

  radius_m <- radius * 1000

  # parse dataframe and store as spatial object
  data <- data.frame(x, stringsAsFactors = FALSE)
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

  if(isTRUE(plot)){
    load(system.file("data/world_map.rds", package = "pcrecon", mustWork = TRUE))
    load(system.file("data/us_map.rds", package = "pcrecon", mustWork = TRUE))
    bbox <- sf::st_bbox(stores)
   plot <- (ggplot() +
      geom_sf(data = world, color = "#2b2b2b", fill = "white", size=0.125) +
      geom_sf(data = usa, color = "#2b2b2b", fill = "white", size=0.125) +
      coord_sf(crs = sf::st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
      geom_sf(data = tr_points, alpha = 0.3) +
      geom_sf(data = circle, alpha = 0.5) +
      geom_point(aes(x = cent_lon, y = cent_lat), shape = 22, fill = "red") +
      labs(x = " ", y = " ") +
      coord_sf(xlim = c(bbox$xmin-buff, bbox$xmax+buff), ylim = c(bbox$ymin-buff, bbox$ymax+buff), expand = FALSE))
   ret <- list(select_crns = select_crns, plot = plot)
   class(ret) <- "spatial_filter"
   return(ret)

  } else {

  return (select_crns)
  }
}

#' Spatial Filtering of Points Based on Climate Footprint
#'
#' @param footprint Raster or SpatialPolygonDataFrame object
#' @param r minimum correlation coefficient for spatial selection
#' @param cent_lat
#' @param cent_lon
#' @param plot
#' @param x
#' @param radius
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

filter_foot <- function(x, footprint, r = 0.3, cent_lat, cent_lon, radius = 500, plot = TRUE) {
  # parse dataframe and store as spatial object
  lat <- x$lat
  lon <- x$lon
  ID <- x$ID

  # check class of footprint object, convert to polygon from raster if needed
  if (class(footprint) == "RasterLayer") {
    contour <- sf::st_as_sf(raster::rasterToPolygons(raster::clump(footprint$correlation >= r), dissolve = TRUE))
    footprint <- sf::st_as_sf(raster::rasterToPolygons(footprint))
  } else {
    contour <- clump(footprint$correlation >= r, dissolve = TRUE)
  }

  circle <- sf::st_transform(radius(cent_lat, cent_lon, radius), crs = 4326)

  select_poly <- suppressMessages(suppressWarnings(contour[circle, ]))

  crns <- sf::st_sfc(sf::st_multipoint(cbind(lon, lat)), crs = 4326)
  crns <- sf::st_transform(crns, sf::st_crs(footprint))

  tr_points <- sf::st_cast(crns, "POINT")
  tr_points <- sf::st_sf(cbind(ID, data.frame(tr_points)))

  points <- sf::st_as_sf(tr_points)
  intersect <- suppressMessages(suppressWarnings(data.frame(sf::st_intersection(select_poly, points))))

  select_crns <- as.character(intersect$ID, stringsAsFactors = FALSE)

  if(length(select_crns) == 0) warning("No chronologies found within this area")
  if(isTRUE(plot)){
    plot <- (ggplot() + geom_sf(data = footprint, aes(fill = correlation), color = NA) +
      scale_fill_gradientn(colours = viridisLite::viridis(3)) +
      geom_sf(data = contour, alpha = 0.05, color = "red") +
      geom_sf(data = select_poly, alpha = 0.05, color = "black") +
      geom_sf(data = tr_points, alpha = 0.3, size = 1) +
      geom_point(aes(x = cent_lon, y = cent_lat), shape = 21, fill = "blue", size = 1))
    ret <- list(select_crns = select_crns, plot = plot)
    class(ret) <- "spatial_filter"
    return(ret)
  } else {
  return(select_crns)
  }

}



#' Radius Polygon Around Point
#'
#' @param cent_lat
#' @param cent_lon
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
#'
radius <- function(cent_lat, cent_lon, radius){
  radius_m <- radius * 1000
  me <- sf::st_sfc(sf::st_point(c(cent_lon, cent_lat)), crs = 4326)
  me_aea <- sf::st_transform(me, "+proj=aea +lat_1=29.5 +lat_2=42.5")
  circle <- sf::st_buffer(me_aea, radius_m)

  return(circle)

}
