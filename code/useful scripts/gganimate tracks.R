library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(gganimate)
library(ggnewscale)
library(ggspatial)
source("code/functions/UKCEH_functions.R")

# load in data ####
ss <- "Ex"
root <- "outputs/PA sites/script_6/"
files <- paste0(root, list.files(root)) %>%
  .[grepl(ss, .)]

df <- lapply(files, readRDS) %>%
  do.call(rbind, .) %>%
  filter(month(DateTime) == 7 & id == 1) %>%
  mutate(site = factor(site, levels = paste0(ss, c(0, 250, 500, 1000, 2000))), 
         new_site = factor(as.integer(substr(site, 3, 99))))

hab <- rast(paste0("outputs/PA sites/script_4/", ss, "0 cropped habitat raster.tif"))

pen_root <- "outputs/PA sites/script_3/"
pen_files <- paste0(pen_root, list.files(pen_root)) %>%
  .[grepl(ss, .) & grepl("pen", .) & grepl(".shp", .)]
pen <- lapply(pen_files, st_read) %>%
  do.call(rbind, .)

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
  
  scale_colour_manual(name = expression("POIs"), values =c("Pen" = "white",
                                                           "Feeders" = "red")) +
  scale_fill_manual(name = expression("POIs"), values =c("Pen" = "white",
                                                         "Feeders" = "red")) +
  new_scale_color() + 
  ## Add home ranges
  geom_path(data = df, aes(x = x, y = y, colour = new_site), linewidth = 2, alpha = 0.7) + 
  geom_point(data = df, aes(x = x, y = y, colour = new_site), size = 4) + 
  scale_colour_manual(name = "Dist. released from PA", values = RColorBrewer::brewer.pal(5, "Dark2")) +
  
  geom_sf(data = PAs, fill = "transparent", linetype = "dashed", linewidth = 1) +
  
  # annotation_north_arrow(which_north = "grid", height = unit(10/8, "cm"),
  #                        width = unit(10/8, "cm"), aes(location = "tr"),
  #                        pad_x = unit(6/8, "cm"), pad_y = unit(6/8, "cm"),) +
  annotation_scale(height = unit(0.25, "cm"), pad_x = unit(2, "cm"),
                   pad_y = unit(1, "cm"), text_cex = 1) +


  ## set theme
  theme_light(base_size = 10) +
  theme(legend.key.size = unit(0.01, "cm"), 
        legend.key.width = unit(0.1, "cm")) +
  scale_y_continuous(name = "Northing", breaks = NULL, limits = c(min(df$y)-100, max(df$y)+100)) +
  scale_x_continuous(name = "Easting", breaks = NULL, limits = c(min(df$x)-500, max(df$x)+500)) +
  coord_sf(datum = pull_crs(hab))

# ggsave(p1, filename = "outputs/animated tracks/static_test.png",
#        units = "px", height = 790, width = 980)

anim <- p1 +
  transition_reveal(df$DateTime) +
  # exit_fade() +
  ggtitle("Date: {frame_along}")

animate(anim, nframes = 329, fps = 8, height = 790, width = 980,
        renderer = av_renderer(paste0("outputs/animated tracks/", ss, "_id1_allpens.mkv")))


# anim_save(filename = "APHA_site_D.gif", path = "outputs/animated tracks")

