#Script to prepare covariates for simulation

# load required libraries
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
source("code/functions/UKCEH_functions.R")

# ATLAS data ####
CRS_used <- "EPSG:27700"


## Release pen ####

### load in the release pen shapefile ####
pen <- st_read("data/ReleasePen/ReleasePen2.shp")

### find the centre of the release pen ####
cen_pen <- st_centroid(st_geometry(pen)) %>%
  vect()


## Habitat raster ####

### load in, transform, crop the habitat raster and change values to UKCEH AC list ####
hab <- rast("data/UKCEH-2018-25m_AllUK/gb2018lcm25m.tif") %>%
  subset(., 1) %>%
  project(., CRS_used) %>%
  crop(., buffer(cen_pen, width = 10000)) %>%
  LC_to_AC(.)

### save the cropped habitat raster ####
writeRaster(hab, "outputs/script_4/ATLAS outputs/cropped habitat raster.tif", overwrite = T)

### extract the extent of the cropped habitat raster to use for other data ####
ext <- as.polygons(hab, extent=T) %>%
  st_as_sf(.)

## Distance to release pen ####

### create a raster of distance to release pen and save ####
pen_dist <- distance(hab, pen)
writeRaster(pen_dist, "outputs/script_4/ATLAS outputs/cropped pen distance raster.tif", overwrite = T)


## Release woodland ####

### create a buffer around the release pen as vect and raster ####
pen_buffer <- buffer(vect(pen), width = 500)
pb_rast <- rasterize(pen_buffer, hab, res = 1000) %>%
  ifel(. == 1, 33, .) #note: release area is marked as value 33

### merge pen buffer into the habitat raster and save ####
hab_pb <- merge(pb_rast, hab)
writeRaster(hab_pb, "outputs/script_4/ATLAS outputs/cropped release pen habitat raster.tif", overwrite = T)


## Hedges ####

### load in and crop hedgerow data ####
hedges <- st_read("data/Woody_LinearFeatures/NorthWyke_LinearFeatures.shp") %>%
  st_transform(., crs = CRS_used) %>%
  st_crop(x = ., y = ext)

### save cropped hedgerow shapefile ####
st_write(hedges, "outputs/script_4/ATLAS outputs/cropped hedgerow shapefile.shp", append = F)

### create a raster of distance to nearest hedgerow ####
hedges_dist <- terra::distance(hab, hedges)

### save the distance to hedgerows raster ####
writeRaster(hedges_dist, "outputs/script_4/ATLAS outputs/cropped hedgerow distance raster.tif", overwrite = T)


## Hedges and edges ####

### extract just the woodland from hab ####
wood_rast <- ifel(hab %in% 1:2, 1, NA)

### extract and save distance to woodland raster ####
wood_dist <- distance(wood_rast)
writeRaster(wood_dist, "outputs/script_4/ATLAS outputs/cropped wood distance raster.tif", overwrite = T)

### convert the hedges shapefile into a raster ####
hedges_rast <- rasterize(hedges, hab, res = 1000)

### merge the hedges and woodland to make hedges and edges and save ####
hedges_edges_rast <- merge(wood_rast, hedges_rast)
writeRaster(hedges_edges_rast, "outputs/script_4/ATLAS outputs/cropped hedges_edges raster.tif", overwrite = T)

### create a raster of the distance to hedges and edges and save####
he_dist <- distance(hedges_edges_rast)
writeRaster(he_dist, "outputs/script_4/ATLAS outputs/cropped hedges_edges distance raster.tif", overwrite = T)


## Feeders ####

### load in the feeder points and convert to shapefile ####
feeders <- read.table("data/FeederCoords2017_27700.csv", sep = ",", header = T) %>%
  st_as_sf(., coords = c("coords.x1", "coords.x2"), crs = CRS_used)

### create a raster of distance to feeders and save ####
feed_dist <- distance(hab, feeders)
writeRaster(feed_dist, "outputs/script_4/ATLAS outputs/cropped feeder distance raster.tif", overwrite = T)

