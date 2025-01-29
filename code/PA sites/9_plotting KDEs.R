## Script to load in and summarise the simulated movement data
## Joe Wilde
## 01/02/2024

## Load in required packages ####
library(progress)
library(sf)
library(amt)
library(terra)
library(tidyterra)
library(tidyverse)
library(ggnewscale)
library(ggblend)
library(ggspatial)
source("code/functions/UKCEH_functions.R")

CRS_used <- "EPSG:27700"
ss <- "As"
root <- "outputs/PA sites/script_6/"
files <- paste0(root, list.files(root)) %>%
  .[grepl(ss, .)]
## Load in simulated movement data ####
sim_trk <-lapply(files, readRDS) %>%
  do.call(rbind , .) %>%
  na.omit() %>%
  filter(id %in% sample(1:max(id), 10)) %>%
  mutate(id = paste(site, id, sep = "_")) %>%
  make_track(tbl = ., .x = x, .y = y, .t = DateTime,
             ID = id, SinceRel = DaysSinceRel, site = site, crs = "EPSG:27700") %>%
  time_of_day(.) %>%
  mutate(week = week(t_),
         month = month(t_),
         day = yday(t_))

site0 <- sim_trk %>%
  filter(site == paste0(ss, 0))

site250 <- sim_trk %>%
  filter(site == paste0(ss, 250))

site500 <- sim_trk %>%
  filter(site == paste0(ss, 500))

site1000 <- sim_trk %>%
  filter(site == paste0(ss, 1000))

site2000 <- sim_trk %>%
  filter(site == paste0(ss, 2000))

## Calculate KDEs ####
time_unit <- "month"

#make a template raster for KDEs
trast <- make_trast(sim_trk, res = 25)

#make multiple KDEs per time unit
hr_sim0 <- site0 %>%
  nest(data = -time_unit) %>%
  mutate(kde = map(data, hr_kde, trast = trast, levels = 0.95, h = c(25, 25)))

hr_sim250 <- site250 %>%
  nest(data = -time_unit) %>%
  mutate(kde = map(data, hr_kde, trast = trast, levels = 0.95, h = c(25, 25)))

hr_sim500 <- site500 %>%
  nest(data = -time_unit) %>%
  mutate(kde = map(data, hr_kde, trast = trast, levels = 0.95, h = c(25, 25)))

hr_sim1000 <- site1000 %>%
  nest(data = -time_unit) %>%
  mutate(kde = map(data, hr_kde, trast = trast, levels = 0.95, h = c(25, 25)))

hr_sim2000 <- site2000 %>%
  nest(data = -time_unit) %>%
  mutate(kde = map(data, hr_kde, trast = trast, levels = 0.95, h = c(25, 25)))

#load in habitat data and UKCEH colours
hab_rast <- terra::rast(paste0("outputs/PA sites/script_4/", ss, "0 cropped habitat raster.tif")) %>%
  ifel(. == 0, NA, .)

hab_cols <- UKCEH_colours(hab_rast, T)

PAs <- st_read("data/Protected Areas/All_UK_PAs.shp") %>%
  st_transform(crs = "EPSG:27700") %>%
  st_crop(., st_buffer(st_as_sf(sim_trk[1,], coords = c("x_", "y_"),
                                crs = "EPSG:27700"), dist = 10000))

pen0 <- st_read(paste0("outputs/PA sites/script_3/", ss, "0_pen_shapefile.shp")) %>%
  st_centroid()
pen250 <- st_read(paste0("outputs/PA sites/script_3/", ss, "250_pen_shapefile.shp")) %>%
  st_centroid()
pen500 <- st_read(paste0("outputs/PA sites/script_3/", ss, "500_pen_shapefile.shp")) %>%
  st_centroid()
pen1000 <- st_read(paste0("outputs/PA sites/script_3/", ss, "1000_pen_shapefile.shp")) %>%
  st_centroid()
pen2000 <- st_read(paste0("outputs/PA sites/script_3/", ss, "2000_pen_shapefile.shp")) %>%
  st_centroid()

## Find the levels of the grouping factor that the real and simulated data have in common
## Need this as there are weeks worth of gaps in the real data
# TrkLevs <- as.data.frame(hr_pheas[,colnames(hr_pheas) == time_unit])
SimLevs <- as.data.frame(hr_sim0[,colnames(hr_sim0) == time_unit])
Facets <- unique(SimLevs$month[!SimLevs$month %in% 3:6])


#create the boundaries of the plot
min_x <- min(sim_trk$x_, na.rm = T)
max_x <- max(sim_trk$x_, na.rm = T)

min_y <- min(sim_trk$y_, na.rm = T)
max_y <- max(sim_trk$y_, na.rm = T)

if(min_x < xmin(hab_rast)) min_x <- xmin(hab_rast)
if(max_x > xmax(hab_rast)) max_x <- xmax(hab_rast)

if(min_y < ymin(hab_rast)) min_y <- ymin(hab_rast)
if(max_y > ymax(hab_rast)) max_y <- ymax(hab_rast)


oranges_palette <- RColorBrewer::brewer.pal(5, "Oranges")
greys_palette <- RColorBrewer::brewer.pal(5, "Greys")

gc()

## Loop through each level and make a plot
# for(j in 1:length(Facets)){
j <- 1
  if(j == 1) {
    pb <- progress_bar$new(total = length(Facets), format = "[:bar] :percent eta::eta", clear = F)
    pb$tick(0)
  }
  
  sim_kde0 <- hr_sim0[hr_sim0[,colnames(hr_sim0) == time_unit] == paste0(Facets[j]),]$kde[[1]]
  sim_kde250 <- hr_sim250[hr_sim250[,colnames(hr_sim250) == time_unit] == paste0(Facets[j]),]$kde[[1]]
  sim_kde500 <- hr_sim500[hr_sim500[,colnames(hr_sim500) == time_unit] == paste0(Facets[j]),]$kde[[1]]
  sim_kde1000 <- hr_sim1000[hr_sim1000[,colnames(hr_sim1000) == time_unit] == paste0(Facets[j]),]$kde[[1]]
  sim_kde2000 <- hr_sim2000[hr_sim2000[,colnames(hr_sim2000) == time_unit] == paste0(Facets[j]),]$kde[[1]]
  # real_kde <- hr_pheas[hr_pheas[,colnames(hr_pheas) == time_unit] == paste0(Facets[j,]),]$kde[[1]]
  
  sim_isos0 <- hr_isopleths(sim_kde0)
  sim_isos250 <- hr_isopleths(sim_kde250)
  sim_isos500 <- hr_isopleths(sim_kde500)
  sim_isos1000 <- hr_isopleths(sim_kde1000)
  sim_isos2000 <- hr_isopleths(sim_kde2000)
  # real_isos <- hr_isopleths(real_kde)
  
  # overlap_df$overlap[j] <- hr_overlap(sim_kde, real_kde, type = "hr")$overlap[1]
  
  ## make plot
  
  pt <- ggplot() +
    geom_spatraster(data = as.factor(hab_rast), alpha = 0.35) +
    
    scale_fill_manual(name = "Habitat", values = hab_cols$colour,
                      labels = hab_cols$habitat) +
    new_scale_fill() +
    
    ## Add home ranges
    
    list(
      geom_sf(data = sim_isos0, aes(colour = "0m", fill = "0m"), alpha = 0.5, linewidth = 7),
      geom_sf(data = sim_isos250, aes(colour = "250m", fill = "250m"), alpha = 0.5, linewidth = 7),
      geom_sf(data = sim_isos500, aes(colour = "500m", fill = "500m"), alpha = 0.5, linewidth = 7),
      geom_sf(data = sim_isos1000, aes(colour = "1000m", fill = "1000m"), alpha = 0.5, linewidth = 7),
      geom_sf(data = sim_isos2000, aes(colour = "2000m", fill = "2000m"), alpha = 0.5, linewidth = 7)
    ) |> blend("multiply") +
    scale_colour_manual(name = "Release pen distance from PA", values = c("0m" = oranges_palette[1],
                                                              "250m" = oranges_palette[2], 
                                                              "500m" = oranges_palette[3], 
                                                              "1000m" = oranges_palette[4], 
                                                              "2000m" = oranges_palette[5]), 
                        breaks = c("0m", "250m", "500m", "1000m", "2000m")) +
    scale_fill_manual(name = "Release pen distance from PA", values = c("0m" = oranges_palette[1],
                                                            "250m" = oranges_palette[2], 
                                                            "500m" = oranges_palette[3], 
                                                            "1000m" = oranges_palette[4], 
                                                            "2000m" = oranges_palette[5]), 
                      breaks = c("0m", "250m", "500m", "1000m", "2000m")) +
    geom_sf(data = PAs, fill = "transparent", aes(linetype = "Protected Area"), linewidth = 7) +
    scale_linetype_manual(name = "", values = c("Protected Area" = "dashed")) +
    new_scale_color() +
    new_scale_fill() +
    ## Add release pen locations
    geom_sf(data = pen0, aes(colour = "0m", fill = "0m"),  size = 25) +
    geom_sf(data = pen250, aes(colour = "250m", fill = "250m"),  size = 25) +
    geom_sf(data = pen500, aes(colour = "500m", fill = "500m"),  size = 25) +
    geom_sf(data = pen1000, aes(colour = "1000m", fill = "1000m"),  size = 25) +
    geom_sf(data = pen2000, aes(colour = "2000m", fill = "2000m"),  size = 25) +
    
    scale_colour_manual(name = "Release pen", values = c("0m" = greys_palette[5],
                                                         "250m" = greys_palette[4], 
                                                         "500m" = greys_palette[3], 
                                                         "1000m" = greys_palette[2], 
                                                         "2000m" = greys_palette[1]), 
                        breaks = c("0m", "250m", "500m", "1000m", "2000m")) +
    scale_fill_manual(name = "Release pen", values = c("0m" = greys_palette[5],
                                                       "250m" = greys_palette[4], 
                                                       "500m" = greys_palette[3], 
                                                       "1000m" = greys_palette[2], 
                                                       "2000m" = greys_palette[1]), 
                      breaks = c("0m", "250m", "500m", "1000m", "2000m")) +
    annotation_north_arrow(which_north = "grid", height = unit(10, "cm"),
                           width = unit(10, "cm"), aes(location = "tr"),
                           pad_x = unit(6, "cm"), pad_y = unit(6, "cm"),) +
    annotation_scale(height = unit(2, "cm"), pad_x = unit(20, "cm"),
                     pad_y = unit(6, "cm"), text_cex = 10) +
    
    ## change labels
    ggtitle(month.name[Facets[j]]) +
    
    
    ## set theme
    theme_light(base_size = 130) +
    theme(legend.key.width = unit(6, "cm")) +
    scale_y_continuous(name = "", 
                       breaks = NULL, limits = c(min_y, max_y)) +
    scale_x_continuous(name = "", 
                       breaks = NULL, limits = c(min_x, max_x)) +
    coord_sf(datum = pull_crs(hab_rast))
  # pt
  
  
  month <- paste0(Facets[j])
  if(Facets[j] == 1) month <- 13
  if(Facets[j] == 2) month <- 14
  
  output_path <- paste0("outputs/PA sites/script_8/pres/site_", ss, "_", month, "_homeranges_step_9.png")
  
  png(output_path, type = "cairo", width = 7980, height = 4920)
  print(pt)
  dev.off()
  
  pb$tick()
};while (dev.cur() != 1) dev.off()

