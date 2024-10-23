library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(gganimate)
library(ggnewscale)
library(ggspatial)
source("code/functions/UKCEH_functions.R")

# load in data ####

df <- readRDS("outputs/PA sites/script_6/Ex2000simulation_data.rds") %>%
  filter(month(DateTime) == 9 & id %in% 1:10)

hab <- rast("outputs/PA sites/script_4/Ex2000 cropped habitat raster.tif") %>%
  ifel(. == 0, NA, .)

pen <- st_read("outputs/PA sites/script_3/Ex2000_pen_shapefile.shp")


feeder_pts <- st_read("outputs/PA sites/script_3/Ex2000_feeders_shapefile.shp")

PAs <- st_read("data/Protected Areas/All_UK_PAs.shp") %>%
  st_transform(crs = "EPSG:27700") %>%
  st_crop(., st_buffer(pen, crs = "EPSG:27700", dist = 10000))

# build up the plot ####

hab_cols <- UKCEH_colours(hab, short_list = T)

p1 <- ggplot() +
  geom_spatraster(data = as.factor(hab), alpha = 0.35) +
  
  scale_fill_manual(name = "Habitat", values = hab_cols$colour,
                    labels = hab_cols$habitat) +
  new_scale_fill() +
  
  ## Add release pen and feeder locations
  geom_sf(data = pen, aes(colour = "Pen", fill = "Pen"), show.legend = F) +
  # geom_spatvector(data = feeder_pts, aes(colour = "Feeders"), size = 20/8, alpha = 0.4) +
  
  scale_colour_manual(name = expression("POIs"), values =c("Pen" = "white",
                                                           "Feeders" = "red")) +
  scale_fill_manual(name = expression("POIs"), values =c("Pen" = "white",
                                                         "Feeders" = "red")) +
  new_scale_color() + 
  ## Add home ranges
  
  geom_path(data = df, aes(x = x, y = y, colour = DayNight, group = id), linewidth = 1, alpha = 0.7) + 
  geom_point(data = df, aes(x = x, y = y, colour = DayNight, group = id), size = 2) + 
  scale_colour_manual(name = "Time of day", values = c("turquoise", "navy")) + 
  
  geom_sf(data = PAs, fill = "transparent", linetype = "dashed") + 
  
  # annotation_north_arrow(which_north = "grid", height = unit(10/8, "cm"),
  #                        width = unit(10/8, "cm"), aes(location = "tr"),
  #                        pad_x = unit(6/8, "cm"), pad_y = unit(6/8, "cm"),) +
  # annotation_scale(height = unit(2/8, "cm"), pad_x = unit(20/8, "cm"),
  #                  pad_y = unit(6/8, "cm"), text_cex = 10/8) +


  ## set theme
  theme_light(base_size = 10) +
  theme(legend.key.size = unit(0.01, "cm"), 
        legend.key.width = unit(0.1, "cm")) +
  scale_y_continuous(name = "Distance from release pen (m)", breaks = NULL, limits = c(min(df$y)-100, max(df$y)+100)) +
  scale_x_continuous(name = "Distance from release pen (m)", breaks = NULL, limits = c(min(df$x)-500, max(df$x)+500)) +
  coord_sf(datum = pull_crs(hab))

# ggsave(p1, filename = "outputs/animated tracks/static_test.png",
#        units = "px", height = 790, width = 980)

anim <- p1 +
  transition_reveal(df$DateTime) +
  ggtitle("Date: {frame_along}")

animate(anim, nframes = 24*30, fps = 8, height = 790, width = 980)

anim_save(filename = "Ex2000_sept.gif", path = "outputs/animated tracks")
