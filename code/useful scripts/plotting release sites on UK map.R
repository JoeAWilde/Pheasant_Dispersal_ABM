library(tidyverse)
library(sf)
library(readxl)


UK <- st_read("../../Luke Data and Code/BTODev_Tetrad_Analysis/SpatialData/CoastOutline/UK_Coatline.shp")

# root <- "outputs/PA sites/script_3/"
# files <- paste0(root,list.files(root)) %>%
#   .[grepl("0_pen_shapefile.shp", .)]
# 
# pens <- lapply(files, function(file) {
#   shapefile <- st_read(file)
#   st_centroid(shapefile)
# }) %>%
#   do.call(rbind, .)

PAs <- read_xlsx("new_sites_2.xlsx") %>%
  filter(Location != "Wasdale screes - Cumbria") %>%
  st_as_sf(., coords = c("Easting", "Northing"), crs = "EPSG:27700")

p1 <- ggplot() + 
  geom_sf(data = UK, linewidth = 2, colour = "lightgrey") + 
  geom_sf(data = PAs, size = 6) + 
  theme_classic()
p1

ggsave(p1, filename = "outputs/PA sites/PA_sites.png", units = "px", 
       height = 4690, width = 3320)
