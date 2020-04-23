library(sf)

fp <- readGDAL("inst/extdata/cfp.nc")

library(raster)
 cor <- stack("inst/extdata/gallinas_cf.nc")
 dim(cor)

 plot(cor)
