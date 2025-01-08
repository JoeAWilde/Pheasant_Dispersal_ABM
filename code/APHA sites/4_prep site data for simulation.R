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

for(ss in c("A", "B", "D")) {
  ## Release pen ####
  
  ### load in the release pen and convert to a closed shape ####
  pen_pts <- read.table(paste0("data/Data for Exeter - anonymised LandscapeV2/Site ", ss,
                               "/Site ", ss, "_Release Pen Coordinate data.csv"),
                        sep = ",", header = TRUE) %>%
    dplyr::select(X, Y)
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
  
  ### find the center of the release pen ####
  cen_pen <- st_centroid(st_geometry(pen)) %>%
    vect()
  
  
  ## Habitat raster ####
  
  ### load in, transform, crop the habitat raster and change values to UKCEH AC list ####
  hab <- rast(paste0("data/Data for Exeter - anonymised LandscapeV2/Site ", ss,
                     "/Site ", ss, " Land Use Raster.tif")) %>%
    subset(., 1) %>%
    crop(., buffer(cen_pen, width = 10000)) %>%
    LC_to_AC(.) %>%
    `crs<-`(CRS_used)
  
  ### save the cropped habitat raster ####
  writeRaster(hab, paste0("outputs/script_4/APHA outputs/site ", ss,
                          "/site ", ss, " cropped habitat raster.tif"), overwrite = T)
  
  ### extract the extent of the cropped habitat raster to use for other data ####
  ext <- as.polygons(hab, extent=T) %>%
    st_as_sf(.)
  
  
  ## Distance to release pen ####
  
  ### create a raster of distance to release pen and save ####
  pen_dist <- distance(hab, pen)
  writeRaster(pen_dist, paste0("outputs/script_4/APHA outputs/site ", ss,
                               "/site ", ss, " cropped pen distance raster.tif"), overwrite = T)
  
  
  ## Release woodland ####
  
  ### create a buffer around the release pen as vect and raster ####
  pen_buffer <- buffer(vect(pen), width = 500)
  pb_rast <- rasterize(pen_buffer, hab, res = 1000) %>%
    ifel(. == 1, 33, .) #note: release area is marked as value 33
  
  ### merge pen buffer into the habitat raster and save ####
  hab_pb <- merge(pb_rast, hab)
  writeRaster(hab_pb, paste0("outputs/script_4/APHA outputs/site ", ss,
                             "/site ", ss, " cropped release pen habitat raster.tif"), overwrite = T)
  
  
  ## Hedges ####
  
  ### load in and crop hedgerow data ####
  hedges <- readRDS(paste0("data/Data for Exeter - anonymised LandscapeV2/Site ", ss,
                           "/Site ", ss, " Hedgerow Data.rds")) %>%
    st_multilinestring(.) %>%
    vect(.) 
  
  crs(hedges) <- CRS_used
  
  ### save cropped hedgerow shapefile ####
  st_write(st_as_sf(hedges), paste0("outputs/script_4/APHA outputs/site ", ss,
                                    "/site ", ss, " cropped hedgerow shapefile.shp"), append = F)
  
  ### create a raster of distance to nearest hedgerow ####
  hedges_dist <- terra::distance(hab, hedges)
  
  ### save the distance to hedgerows raster ####
  writeRaster(hedges_dist, paste0("outputs/script_4/APHA outputs/site ", ss,
                                  "/site ", ss, " cropped hedgerow distance raster.tif"), overwrite = T)
  
  
  ## Hedges and edges ####
  
  ### extract just the woodland from hab ####
  wood_rast <- ifel(hab %in% 1:2, 1, NA)
  
  ### save just woodland as a raster ####
  writeRaster(wood_rast, paste0("outputs/script_4/APHA outputs/site ", ss, 
                                "/site ", ss, " cropped wood raster.tif"), overwrite = T)
  
  ### extract and save distance to woodland raster ####
  wood_dist <- distance(wood_rast)
  writeRaster(wood_dist, paste0("outputs/script_4/APHA outputs/site ", ss,
                                "/site ", ss, " cropped wood distance raster.tif"), overwrite = T)
  
  ### convert the hedges shapefile into a raster ####
  hedges_rast <- rasterize(hedges, hab, res = 1000)
  
  ### merge the hedges and woodland to make hedges and edges and save ####
  hedges_edges_rast <- merge(wood_rast, hedges_rast)
  writeRaster(hedges_edges_rast, paste0("outputs/script_4/APHA outputs/site ", ss,
                                        "/site ", ss, " cropped hedges_edges raster.tif"), overwrite = T)
  
  ### create a raster of the distance to hedges and edges and save####
  he_dist <- distance(hedges_edges_rast)
  writeRaster(he_dist, paste0("outputs/script_4/APHA outputs/site ", ss,
                              "/site ", ss, " cropped hedges_edges distance raster.tif"), overwrite = T)
  
  
  ## Trimmed hedges and edges ####
  
  ### use function to trim hedges within a certain radius of the centre of the pen ####
  trim_hedges_rast <- trim_hedges(hab, hedges_rast, cen_pen, 2000)
  
  ### merge the hedges and woodland to make trimmed hedges and edges and save ####
  trim_hedges_edges_rast <- merge(wood_rast, trim_hedges_rast)
  writeRaster(trim_hedges_edges_rast, paste0("outputs/script_4/APHA outputs/site ", ss,
                                             "/site ", ss, " cropped trimmed hedges_edges raster.tif"), overwrite = T)
  
  ### create a raster of the distance to trimmed hedges and edges and save####
  trim_he_dist <- distance(trim_hedges_edges_rast)
  writeRaster(trim_he_dist, paste0("outputs/script_4/APHA outputs/site ", ss,
                                   "/site ", ss, " cropped trimmed hedges_edges distance raster.tif"), overwrite = T)
  
  
  ## Edges of fields ####
  field_edges <- ifel(hab %in% 3:4, 1, NA) %>%
    as.polygons(., dissolve = TRUE) %>%
    .[!is.na(values(.)), ] %>%
    st_as_sf(.) %>%
    st_boundary(.)
  
  field_edges_dist <- distance(hab, field_edges)
  writeRaster(field_edges_dist, paste0("outputs/script_4/APHA outputs/site ", ss,
                                       "/site ", ss, " cropped field_edges distance raster.tif"), overwrite = T)
  
  ## Feeders ####
  
  ### load in the feeder points and convert to shapefile ####
  feeders <- read.table(paste0("data/Data for Exeter - anonymised LandscapeV2/Site ",
                               ss, "/FAKE_Site ", ss, " Hopper_Feeder Location Data.csv"),
                        sep = ",", header = T) %>%
    st_as_sf(., coords = c("X", "Y"), crs = CRS_used)
  
  ### create a raster of distance to feeders and save ####
  feed_dist <- distance(hab, feeders)
  writeRaster(feed_dist, paste0("outputs/script_4/APHA outputs/site ", ss,
                                "/site ", ss, " cropped FAKE feeder distance raster.tif"), overwrite = T)
}
