library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(gganimate)
library(ggnewscale)
library(ggspatial)
source("code/functions/UKCEH_functions.R")

# load in data ####

df <- readRDS("outputs/script_6/APHA output/simulation_data.rds") %>%
  filter(id == 3 & site == "B")

hab <- rast("outputs/script_4/APHA outputs/site B/site B cropped habitat raster.tif") %>%
  ifel(. == 0, NA, .)

pen_pts <- read.table("data/Data for Exeter - anonymised LandscapeV2/Site B/Site B_Release Pen Coordinate data.csv", 
                      sep = ",", header = TRUE) %>%
  dplyr::select(X, Y)
# Ensure the loop is closed
if(any(pen_pts[nrow(pen_pts), ] != pen_pts[1, ])) {
  pen_pts <- rbind(pen_pts, pen_pts[1, ])
}
# Convert to sf, specifying the CRS if known
pen_sf <- st_as_sf(pen_pts, coords = c("X", "Y"), crs = "EPSG:27700")
# Convert to LINESTRING
pen_line <- st_sfc(st_linestring(as.matrix(pen_pts)), crs = "EPSG:27700")
pen <- vect(st_cast(pen_line, "POLYGON", crs = "EPSG:27700")) %>%
  st_as_sf()


feeder_pts <- read.csv("data/Data for Exeter - anonymised LandscapeV2/Site B/Site B Hopper_Feeder Location Data.csv") %>%
  vect(., geom = c("X", "Y"), crs = "EPSG:27700") 

# build up the plot ####

hab_cols <- UKCEH_colours(hab, short_list = T)

p1 <- ggplot() +
  geom_spatraster(data = as.factor(hab), alpha = 0.35) +
  
  scale_fill_manual(name = "Habitat", values = hab_cols$colour,
                    labels = hab_cols$habitat) +
  new_scale_fill() +
  
  ## Add release pen and feeder locations
  geom_sf(data = pen, aes(colour = "Pen", fill = "Pen"), show.legend = F) +
  geom_spatvector(data = feeder_pts, aes(colour = "Feeders"), size = 20, alpha = 0.4) +
  
  scale_colour_manual(name = expression("POIs"), values =c("Pen" = "white",
                                                           "Feeders" = "red")) +
  scale_fill_manual(name = expression("POIs"), values =c("Pen" = "white",
                                                         "Feeders" = "red")) +
  new_scale_color() + 
  ## Add home ranges
  
  geom_path(data = df, aes(x = x, y = y, colour = DayNight, group = id), linewidth = 10, alpha = 0.5) + 
  geom_point(data = df, aes(x = x, y = y, colour = DayNight, group = id), size = 20) + 
  scale_colour_manual(name = "Time of day", values = c("turquoise", "navy")) + 
  
  annotation_north_arrow(which_north = "grid", height = unit(10, "cm"),
                         width = unit(10, "cm"), aes(location = "tr"),
                         pad_x = unit(6, "cm"), pad_y = unit(6, "cm"),) +
  annotation_scale(height = unit(2, "cm"), pad_x = unit(20, "cm"),
                   pad_y = unit(6, "cm"), text_cex = 10) +
  
  
  ## set theme
  theme_light(base_size = 130) +
  theme(legend.key.width = unit(6, "cm")) +
  scale_y_continuous(name = "Distance from release pen (m)", breaks = NULL, limits = c(min(df$y)-100, max(df$y)+100)) +
  scale_x_continuous(name = "Distance from release pen (m)", breaks = NULL, limits = c(min(df$x)-500, max(df$x)+500)) +
  coord_sf(datum = pull_crs(hab))

# ggsave(p1, filename = "outputs/animated tracks/static_test.png", 
#        units = "px", height = 4320, width = 7980)

anim <- p1 +
  transition_reveal(df$DateTime) +
  ggtitle("Date: {frame_along}")

animate(anim, nframes = nrow(df), fps = 4, height = 6320, width =7980)

anim_save(filename = "site_B_id_3.gif", path = "outputs/animated tracks")
