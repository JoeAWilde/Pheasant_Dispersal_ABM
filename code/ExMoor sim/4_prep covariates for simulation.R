#Script to prepare covariates for simulation

# load required libraries
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
source("code/functions/UKCEH_functions.R")
source("code/functions/hedge_trimming.R")

# APHA data ####
CRS_used <- "EPSG:27700"

## Release pen ####

### load in the release pen and convert to a closed shape ####
pen_pts <- read.table("data/fake Exmoor site/exmoor_release_pen_coordinates.csv",sep = ",", header = TRUE)
# Ensure the loop is closed
if(any(pen_pts[nrow(pen_pts), ] != pen_pts[1, ])) {
  pen_pts <- rbind(pen_pts, pen_pts[1, ])
}
# Convert to sf, specifying the CRS if known
pen_sf <- st_as_sf(pen_pts, coords = c("X", "Y"), crs = CRS_used)
# Convert to LINESTRING
pen_line <- st_sfc(st_linestring(as.matrix(pen_pts)), crs = CRS_used)
pen <- vect(st_cast(pen_line, "POLYGON", crs = CRS_used)) %>%
  st_as_sf()

plot(pen)
### find the center of the release pen ####
cen_pen <- st_centroid(st_geometry(pen)) %>%
  vect()


## Habitat raster ####

### load in, transform, crop the habitat raster and change values to UKCEH AC list ####
hab <- rast("data/UKCEH-2018-25m_AllUK/gb2018lcm25m.tif") %>%
  subset(., 1) %>%
  crop(., buffer(cen_pen, width = 10000)) %>%
  LC_to_AC(.) %>%
  `crs<-`(CRS_used)

### save the exmoor cropped habitat raster ####
writeRaster(hab, "outputs/script_4/Exmoor site/exmoor cropped habitat raster.tif", overwrite = T)

### extract the extent of the exmoor cropped habitat raster to use for other data ####
ext <- as.polygons(hab, extent=T) %>%
  st_as_sf(.)


## Distance to release pen ####

### create a raster of distance to release pen and save ####
pen_dist <- distance(hab, pen)
writeRaster(pen_dist, "outputs/script_4/Exmoor site/exmoor cropped pen distance raster.tif", overwrite = T)


## Release woodland ####

### create a buffer around the release pen as vect and raster ####
pen_buffer <- buffer(vect(pen), width = 500)
pb_rast <- rasterize(pen_buffer, hab, res = 1000) %>%
  ifel(. == 1, 33, .) #note: release area is marked as value 33

### merge pen buffer into the habitat raster and save ####
hab_pb <- merge(pb_rast, hab)
writeRaster(hab_pb, "outputs/script_4/Exmoor site/exmoor cropped release pen habitat raster.tif", overwrite = T)


## Hedges ####

### load in and crop hedgerow data ####
hedges <- st_read("data/Linear+Woody+Features_2393217/wlff-2016_5294765/GB_WLF_V1_0.gdb") %>%
  st_crop(x = ., y = ext)

crs(hedges) <- CRS_used

### save exmoor cropped hedgerow shapefile ####
st_write(st_as_sf(hedges), "outputs/script_4/Exmoor site/exmoor cropped hedgerow shapefile.shp", append = F)

### create a raster of distance to nearest hedgerow ####
hedges_dist <- terra::distance(hab, hedges)

### save the distance to hedgerows raster ####
writeRaster(hedges_dist, "outputs/script_4/Exmoor site/exmoor cropped hedgerow distance raster.tif", overwrite = T)


## Hedges and edges ####

### extract just the woodland from hab ####
wood_rast <- ifel(hab %in% 1:2, 1, NA)

### save just woodland as a raster ####
writeRaster(wood_rast, "outputs/script_4/Exmoor site/exmoor cropped wood raster.tif", overwrite = T)

### extract and save distance to woodland raster ####
wood_dist <- distance(wood_rast)
writeRaster(wood_dist, "outputs/script_4/Exmoor site/exmoor cropped wood distance raster.tif", overwrite = T)

### convert the hedges shapefile into a raster ####
hedges_rast <- rasterize(hedges, hab, res = 1000)

### merge the hedges and woodland to make hedges and edges and save ####
hedges_edges_rast <- merge(wood_rast, hedges_rast)
writeRaster(hedges_edges_rast, "outputs/script_4/Exmoor site/exmoor cropped hedges_edges raster.tif", overwrite = T)

### create a raster of the distance to hedges and edges and save####
he_dist <- distance(hedges_edges_rast)
writeRaster(he_dist, "outputs/script_4/Exmoor site/exmoor cropped hedges_edges distance raster.tif", overwrite = T)


## Trimmed hedges and edges ####

### use function to trim hedges within a certain radius of the centre of the pen ####
trim_hedges_rast <- trim_hedges(hab, hedges_rast, cen_pen, 2000)

### merge the hedges and woodland to make trimmed hedges and edges and save ####
trim_hedges_edges_rast <- merge(wood_rast, trim_hedges_rast)
writeRaster(trim_hedges_edges_rast, "outputs/script_4/Exmoor site/exmoor cropped trimmed hedges_edges raster.tif", overwrite = T)

### create a raster of the distance to trimmed hedges and edges and save####
trim_he_dist <- distance(trim_hedges_edges_rast)
writeRaster(trim_he_dist, "outputs/script_4/Exmoor site/exmoor cropped trimmed hedges_edges distance raster.tif", overwrite = T)


## Feeders ####

### load in the feeder points and convert to shapefile ####
feeders <- read.table("data/fake Exmoor site/fake_feeder_locations.csv",
                      sep = ",", header = T) %>%
  rename(X = X..Easting., 
         Y = Y..Northing.) %>%
  select(X, Y) %>%
  st_as_sf(., coords = c("X", "Y"), crs = CRS_used)

### create a raster of distance to feeders and save ####
feed_dist <- distance(hab, feeders)
writeRaster(feed_dist, "outputs/script_4/Exmoor site/exmoor cropped feeder distance raster.tif", overwrite = T)
