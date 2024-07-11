trim_hedges <- function(hab, hedges_rast, cen_pen, dist) {
  
  trimming_buffer <- st_buffer(st_as_sf(cen_pen), dist = dist)
  
  trim_rast <- rasterize(trimming_buffer, hab, res = 1000) %>%
    ifel(. == 1, 44, .)
  
  trimmed_hedge_rast <- merge(trim_rast,hedges_rast) %>%
    ifel(. == 44, NA, .)
  
  return(trimmed_hedge_rast)
  
}