<<<<<<< HEAD
# ## Make Map
# cent_lat = 36.21
# cent_lon = -105.9
# radius = 250
# radius_m <- radius * 1000
#
# center <- sf::st_sfc(sf::st_point(c(cent_lon = cent_lon, cent_lat = cent_lat)), crs = 4326)
#
# # convert to albers equal area projection
# center_aea <- sf::st_transform(center, "+proj=aea +lat_1=29.5 +lat_2=42.5")
#
# # circle from radius and select points within
# circle <- sf::st_buffer(center_aea, radius_m)
#
# states <- ggplot2::map_data("state")
#
# df_states <- tibble::tibble(region = state.name, state = state.abb) %>%
#   dplyr::mutate(region = stringr::str_to_lower(df_states$region))
#
# metadata1 <- metadata %>%
#   dplyr::rename(latitude = lat, longitude = lon) %>%
#   dplyr::mutate(state = stringr::str_extract(metadata$ID, "[[^:alpha:]]{2}"),
#                 selected = dplyr::if_else(ID %in% select_crns_rad, TRUE, FALSE)) %>%
#   dplyr::left_join(df_states)
#
# ggplot(states, aes(long, lat)) + geom_polygon(aes(group = group), fill = "white", colour = "gray") + coord_fixed(1.3) + guides(fill = FALSE) + geom_point(data = metadata1, aes(longitude, latitude, colour = selected)) # + theme_nothing()
#
# metadata2 <- metadata %>%
#   dplyr::rename(latitude = lat, longitude = lon) %>%
#   dplyr::mutate(state = stringr::str_extract(metadata$ID, "[[^:alpha:]]{2}"),
#                 selected = dplyr::if_else(ID %in% select_crns_rad, TRUE, FALSE)) %>%
#   dplyr::left_join(df_states) %>%
#   dplyr::left_join(states)
#
# ggplot(metadata2, aes(long, lat)) + geom_polygon(aes(group = group), fill = "white", colour = "gray") + coord_fixed(1.3) + guides(fill = FALSE) + geom_point(aes(longitude, latitude, colour = selected)) # + theme_nothing()
=======
  ## Make Map
  cent_lat = 36.21
cent_lon = -105.9
radius = 250
radius_m <- radius * 1000

center <- sf::st_sfc(sf::st_point(c(cent_lon = cent_lon, cent_lat = cent_lat)), crs = 4326)

# convert to albers equal area projection
center_aea <- sf::st_transform(center, "+proj=aea +lat_1=29.5 +lat_2=42.5")

# circle from radius and select points within
circle <- sf::st_buffer(center_aea, radius_m)

states <- ggplot2::map_data("state")

df_states <- tibble::tibble(region = state.name, state = state.abb) %>%
  dplyr::mutate(region = stringr::str_to_lower(.$region))

metadata1 <- metadata %>%
  dplyr::rename(latitude = lat, longitude = lon) %>%
  dplyr::mutate(state = stringr::str_extract(metadata$ID, "[[^:alpha:]]{2}"),
                selected = dplyr::if_else(ID %in% select_crns_rad, TRUE, FALSE)) %>%
  dplyr::left_join(df_states)

ggplot(states, aes(long, lat)) + geom_polygon(aes(group = group), fill = "white", colour = "gray") + coord_fixed(1.3) + guides(fill = FALSE) + geom_point(data = metadata1, aes(longitude, latitude, colour = selected)) # + theme_nothing()

metadata2 <- metadata %>%
  dplyr::rename(latitude = lat, longitude = lon) %>%
  dplyr::mutate(state = stringr::str_extract(metadata$ID, "[[^:alpha:]]{2}"),
                selected = dplyr::if_else(ID %in% select_crns_rad, TRUE, FALSE)) %>%
  dplyr::left_join(df_states) %>%
  dplyr::left_join(states)

ggplot(metadata2, aes(long, lat)) + geom_polygon(aes(group = group), fill = "white", colour = "gray") + coord_fixed(1.3) + guides(fill = FALSE) + geom_point(aes(longitude, latitude, colour = selected)) # + theme_nothing()
>>>>>>> 4b51b1aa4a544438f1bb6cfb58f466afd24b0971

# geom_polygon(aes(group = group, fill = assault)) +
# coord_map("albers",  at0 = 45.5, lat1 = 29.5)
