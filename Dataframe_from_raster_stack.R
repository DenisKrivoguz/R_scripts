setwd("D:/GIS/Kerch_peninsula")

library(raster)

dem <- raster("dem_utm_cut.tif")
mbi <- raster("mbi_utm_cut.tif")
ndvi <- raster("ndvi_utm_cut.tif")
dtr <- raster("dtr_utm_cut.tif")
dtw <- raster("dtw_utm_cut.tif")
lsi3 <- raster("lsi3_utm_cut.tif")
lsi8 <- raster("lsi8_utm_cut.tif")
slope <- raster("slope_utm_cut.tif")
soils <- raster("soils_utm_cut.tif")
spi <- raster("spi_utm_cut.tif")


ext <- extent(227783, 315714, 4987754, 5040340)


extent(dem) <- ext
extent(mbi) <- ext
extent(ndvi) <- ext
extent(dtr) <- ext
extent(dtw) <- ext
extent(lsi3) <- ext
extent(lsi8) <- ext
extent(slope) <- ext
extent(soils) <- ext
extent(spi) <- ext


mbi_res <- resample(mbi, dem)
ndvi_res <- resample(ndvi, dem)
dtr_res <- resample(dtr, dem)
dtw_res <- resample(dtw, dem)
lsi3_res <- resample(lsi3, dem)
lsi8_res <- resample(lsi8, dem)
slope_res <- resample(slope, dem)
soils_res <- resample(soils, dem)
spi_res <- resample(spi, dem)


raster_stack <- stack(c(dem, ndvi_res, mbi_res, dtr_res, dtw_res, lsi3_res, lsi8_res, slope_res, soils_res, spi_res))

stackdf <- rasterToPoints(raster_stack)

new_stackdf <- na.omit(stackdf)
