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


## Load in actual movement data ####
pheas_trk <-  read.table("data/Pheas_filtered.csv", header = T, sep = ",") %>%
  mutate(DateTime = ymd_hms(DateTime), 
         Deploydatetime = ymd_hms(Deploydatetime)) %>%
  filter(year(Deploydatetime) == 2018) %>%
  group_by(ID) %>%
  filter(minute(DateTime) == minute(DateTime[1])) %>%
  mutate(SinceRel = as.numeric(difftime(DateTime, DateTime[1], units = "days"))) %>%
  ungroup() %>%
  make_track(tbl = ., .x = X, .y = Y, .t = DateTime,
             ID = ID, SinceRel = SinceRel, crs = "EPSG:27700") %>%
  time_of_day(.) %>%
  mutate(week = week(t_),
         month = month(t_),
         day = yday(t_))

## Load in simulated movement data ####
sim_trk <- readRDS("outputs/script_6/simulation_data.rds") %>%
  filter(x != 0) %>%
  make_track(tbl = ., .x = x, .y = y, .t = DateTime,
             ID = id, SinceRel = DaysSinceRel, crs = "EPSG:27700") %>%
  time_of_day(.) %>%
  mutate(week = week(t_),
         month = month(t_),
         day = yday(t_))

## Calculate KDEs ####
time_unit <- "month"

#make a template raster for KDEs
trast <- make_trast(pheas_trk, res = 25)

#make multiple KDEs per time unit
hr_pheas <- pheas_trk %>%
  nest(data = -time_unit) %>%
  mutate(kde = map(data, hr_kde, trast = trast, levels = 0.95, h = c(25, 25)))

hr_sim <- sim_trk %>%
  nest(data = -time_unit) %>%
  mutate(kde = map(data, hr_kde, trast = trast, levels = 0.95, h = c(25, 25)))

# saveRDS(hr_pheas, paste0("outputs/derived/script_9/site_", 
#                          i, "_real_kde.rds"))
# 
# saveRDS(hr_sim, paste0("outputs/derived/script_9/site_", 
#                        i, "_sim_kde_NULL.rds"))


## Read in some covariate information for plotting ####

# pen <- read.table(paste0("data/Data for Exeter - anonymised LandscapeV2/Site ", i,
#                          "/Site ", i, "_Release Pen Coordinate data.csv"),
#                   sep = ",", header = TRUE) %>%
#   dplyr::select(X, Y)
# 
# # Ensure the loop is closed
# if(any(pen[nrow(pen), ] != pen[1, ])) {
#   pen <- rbind(pen, pen[1, ])
# }
# 
# # Convert to sf, specifying the CRS if known
# pen_sf <- st_as_sf(pen, coords = c("X", "Y"), crs = "EPSG:27700")
# 
# # Convert to LINESTRING
# pen_line <- st_sfc(st_linestring(as.matrix(pen)), crs = "EPSG:27700")

pen_pts <- st_read("data/ReleasePen/ReleasePen2.shp")

## Read in feeder coordinates
feeder_pts <- read.csv("data/FeederCoords2017_27700.csv") %>%
  vect(., geom = c("coords.x1", "coords.x2"), crs = "EPSG:27700") 

#load in habitat data and UKCEH colours
hab_rast <- terra::rast("outputs/script_4/ATLAS outputs/cropped habitat raster.tif") %>%
  ifel(. == 0, NA, .)

hab_cols <- UKCEH_colours(hab_rast, T)

## Find the levels of the grouping factor that the real and simulated data have in common
## Need this as there are weeks worth of gaps in the real data
TrkLevs <- as.data.frame(hr_pheas[,colnames(hr_pheas) == time_unit])
SimLevs <- as.data.frame(hr_sim[,colnames(hr_sim) == time_unit])
Facets <- hr_pheas[,colnames(hr_pheas) == time_unit][is.na(match(TrkLevs[,1], SimLevs[,1]))==F,]

#create the boundaries of the plot
min_x <- min(c(pheas_trk$x_, sim_trk$x_), na.rm = T)
max_x <- max(c(pheas_trk$x_, sim_trk$x_), na.rm = T)

min_y <- min(c(pheas_trk$y_, sim_trk$y_), na.rm = T)
max_y <- max(c(pheas_trk$y_, sim_trk$y_), na.rm = T)


gc()

## Loop through each level and make a plot
for(j in 1:nrow(Facets)){
  if(j == 1) {
    pb <- progress_bar$new(total = nrow(Facets), format = "[:bar] :percent eta::eta", clear = F)
    pb$tick(0)
    
    overlap_df <- data.frame(month = month.name[Facets$month],
                             overlap = rep_len(0, nrow(Facets)))
  }
  
  sim_kde <- hr_sim[hr_sim[,colnames(hr_sim) == time_unit] == paste0(Facets[j,]),]$kde[[1]]
  real_kde <- hr_pheas[hr_pheas[,colnames(hr_pheas) == time_unit] == paste0(Facets[j,]),]$kde[[1]]
  
  sim_isos <- hr_isopleths(sim_kde)
  real_isos <- hr_isopleths(real_kde)
  
  overlap_df$overlap[j] <- hr_overlap(sim_kde, real_kde, type = "hr")$overlap[1]
  
  ## make plot
  
  pt <- ggplot() +
    geom_spatraster(data = as.factor(hab_rast), alpha = 0.35) +
    
    scale_fill_manual(name = "Habitat", values = hab_cols$colour,
                      labels = hab_cols$habitat) +
    new_scale_fill() +
    
    ## Add home ranges
    
    list(
      geom_sf(data = real_isos, aes(colour = "Observed", fill = "Observed"), alpha = 0.5, linewidth = 7),
      geom_sf(data = sim_isos, aes(colour = "Simulated", fill = "Simulated"), alpha = 0.5, linewidth = 7)
    ) |> blend("multiply") +
    scale_colour_manual(name = "Data type", values = c("Simulated" = "goldenrod4",
                                                       "Observed" = "blue4")) +
    scale_fill_manual(name = "Data type", values = c("Simulated" = "goldenrod",
                                                     "Observed" = "blue")) +
    new_scale_fill() +
    ## Add release pen and feeder locations
    geom_sf(data = pen_pts, aes(colour = "Pen", fill = "Pen"), show.legend = F) +
    geom_spatvector(data = feeder_pts, aes(colour = "Feeders"), size = 20, alpha = 0.4) +

    scale_colour_manual(name = expression("POIs"), values =c("Pen" = "white",
                                                             "Feeders" = "red")) +
    scale_fill_manual(name = expression("POIs"), values =c("Pen" = "white",
                                                           "Feeders" = "red")) +
    
    annotation_north_arrow(which_north = "grid", height = unit(10, "cm"),
                           width = unit(10, "cm"), aes(location = "tr"),
                           pad_x = unit(6, "cm"), pad_y = unit(6, "cm"),) +
    annotation_scale(height = unit(2, "cm"), pad_x = unit(20, "cm"),
                     pad_y = unit(6, "cm"), text_cex = 10) +
    
    ## change labels
    ggtitle(month.name[Facets$month[j]]) +
    
    
    ## set theme
    theme_light(base_size = 130) +
    theme(legend.key.width = unit(6, "cm")) +
    scale_y_continuous(name = "Distance from release pen (m)", breaks = NULL, limits = c(min_y, max_y)) +
    scale_x_continuous(name = "Distance from release pen (m)", breaks = NULL, limits = c(min_x, max_x)) +
    coord_sf(datum = pull_crs(hab_rast))
  # pt
  
  
  month <- paste0(Facets[j,])
  if(Facets[j,] == 1) month <- 13
  if(Facets[j,] == 2) month <- 14
  
  output_path <- paste0("outputs/script_7/", month, " day homeranges big.png")
  
  png(output_path, type = "cairo", width = 7980, height = 6320)
  print(pt)
  dev.off()
  
  pb$tick()
}
while (dev.cur() != 1) dev.off()

saveRDS(overlap_df, paste0("outputs/derived/script_9/site_", i, 
                           "_KDE_overlap_values.rds"))
