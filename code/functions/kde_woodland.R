kde_woodland <- function(df, wood_rast, kde_size) {
  require(tidyverse)
  require(terra)
  require(tidyterra)
  require(sf)
  require(amt)
  
  trk <- df %>%
    make_track(tbl = ., .x = x, .y = y, .t = DateTime, crs = "EPSG:27700")
  
  trast1 <- make_trast(trk, res = 25)
  
  kde_all <- hr_kde(trk, trast = trast1, levels = kde_size, h = c(25, 25))
  kde_polygon <- hr_isopleths(kde_all)
  kde_sf <- st_as_sf(kde_polygon)
  kde_rast <- rasterize(kde_sf, hab, res = 1000) 
  
  woodland <- kde_rast * wood_rast
  
  dist <- terra::distance(woodland)
  
  kde_wood_rasts <- c(woodland, dist)
  
  return(kde_wood_rasts)
}