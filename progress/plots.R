
library(rgdal)
library(raster)

select_crns_fp <- filter_foot(x = metadata, footprint = footprint, r = 0.5, cent_lat = 35.65, cent_lon = -105.32, radius = 300, plot = TRUE)

e <- as(extent(-108, -98, 33, 38), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r <- crop(footprint, e)

plot(footprint)
plot(r)

devtools::check(args = c('--as-cran'))

print(select_crns_fp$plot)

select_crns_rad$plot


ggsave("radius_plot.tiff", plot = select_crns_rad$plot, device = "tiff", dpi = 600)

select_crns_fp[[2]]

recon$plot


ggsave("recon_plot.tiff", plot = pw_recon$plot, device = "tiff", dpi = 600, width = 10.00)

plot2 <- ggplot_build(select_crns_fp$plot)

plot2


plot1 <- select_crns_fp$plot

load(system.file("data/us_map.rds", package = "pcrecon", mustWork = TRUE))

plot_2 <- plot1 + geom_sf(data = usa, fill = NA, color = "black", size = .125) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none") +
  scale_fill_viridis(begin = 0.1)


plot_3 <- plot1 + geom_sf(data = usa, fill = NA, color = "black", size=0.5) +
  coord_sf(xlim = c(-108, -98), ylim = c(33,38)) +
  scale_fill_viridis_c(begin = 0.1) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())



print(plot_2)

library(sf)
e <- as(extent(-108, -98, 33, 38), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r <- crop(plot_2, e)
foo <- st_crop(usa, e)



plot_crop <- select_crns_fp$plot
plot_crop

plot_3 <- plot1 + geom_sf(data = usa, color = "black", fill = NA, size=0.125)

ggsave("plot_footprint.tiff", plot_3, dpi = 600)
ggsave("plot2.tiff", plot_2, dpi = 600)
ggsave("plot2_crop.tiff", plot_3, dpi = 600)

plot_3

