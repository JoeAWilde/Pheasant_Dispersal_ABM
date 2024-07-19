library(moveVis)
library(basemaps)
library(sf)
library(tidyverse)
library(terra)
library(sp)

remove.packages("basemaps")
.rs.restartR()
remotes::install_github("zross/basemap-fork")
library(basemaps)

cen_pen <- st_read("data/ReleasePen/ReleasePen2.shp") %>%
  st_centroid(st_geometry()) %>%
  st_coordinates()

df <- readRDS("outputs/script_6/APHA output/simulation_data.rds") %>%
  filter(id == 1 & site == "B") %>%
  st_as_sf(., coords = c("x", "y"), crs = "EPSG:27700")

ll_df <- st_transform(df, 4326) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  set_names(c("long", "lat")) %>%
  mutate(DateTime = df$DateTime, 
         id = df$id)

data_move <- df2move(ll_df, 
                     proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                     x = "long", y = "lat", time = "DateTime", track_id = "id")

m <- align_move(data_move, res = 1, unit = "hours")

crs_string <- proj4string(m)

# hab_list <- rast("data/UKCEH_Devon_2018/LCM.tif")[[1]] %>%
#   project(., crs_string)

data("basemap_data")
r_list <- basemap_data[[1]]
r_times <- basemap_data[[2]]

frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "discrete",
                         fade_raster = TRUE) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_timestamps(type = "label") %>%
  add_progress()

animate_frames(frames, out_file = "moveVis_test.gif", overwrite = T)
