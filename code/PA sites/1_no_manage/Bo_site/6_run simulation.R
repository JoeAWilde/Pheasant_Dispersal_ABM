#Script to run the dispersal simulation 
setwd("/mnt/shared/scratch/jwilde/Pheasant_Dispersal_ABM/")
# load required libraries
library(tidyverse)
library(terra)
library(doSNOW)
library(sf)
library(suncalc)
library(progress)
library(readxl)
library(brms)

source("code/functions/simulation function.R")

CRS_used <- "EPSG:27700"

site_to_run <- "Bo"


site_coords <- read_xlsx("all_PA_sites.xlsx") %>%
  mutate(id = paste0(substr(Location, 1, 2), Approx_dist_from_PA))
sites <- site_coords$id %>%
    .[grepl(site_to_run, .)]

## load in the step length parameters ####
sl_model <- readRDS("outputs/script_2/sl_regress_rp.rds")

sl_pars <- as.data.frame(summary(sl_model)$fixed)
sl_spec_pars <- as.data.frame(summary(sl_model)$spec_pars)

sl_pars <- list(
  Intercept = sl_pars$Estimate[1],
  Hab_values = as.integer(substr(rownames(sl_pars)[2:10], 4, 6)),
  Hab_betas = sl_pars$Estimate[2:10],
  Time_beta = sl_pars$Estimate[11],
  Gam_shape = sl_spec_pars$Estimate[1]
)

## load in turning angle parameters ####
ta_model <- readRDS("outputs/script_2/ta_regress_rp.rds")

ta_pars <- as.data.frame(summary(ta_model)$fixed)

ta_pars <- list(
  vm_mu = ta_pars$Estimate[1], 
  vm_kappa = 1
)

# iSSF parameters ####

## load in iSSF coefficients ####
ssf_model <- readRDS("outputs/script_3/iSSF_field_edges.rds")
ssf_betas <- ssf_model$model$coefficients

## extract names of coefficients ####
cov_names <- names(ssf_betas)
exp(ssf_betas)

## !!!>>> change feeder beta to zero <<<!!! ####
ssf_betas["feed"] <- 0
exp(ssf_betas)

# Simulation parameters ####
st_date <- ymd_hms("2018-07-18 07:05:00")
n_IDS <- 1000
fix_rate <- 60
n_steps <- as.numeric(difftime(st_date + months(7), st_date, units = "mins")) / fix_rate
n_csteps <- 200
stop_if_left <- TRUE

## !!!>>> change dogin dates and times to outside sim season <<<!!! ####
dogin_dates <- as.Date(seq(ymd_hms("2000-06-01 00:00:01"), ymd_hms("2000-06-02 00:00:01"), by = "days"))
dogin_times <- hms::as_hms(c("23:59:59", "00:00:01"))
dogin_prob <- 0.0

# Other parameters for simulation ####

## create a dataframe of sunlight times at the centre of the pen ####
atlas_pen_cent <- st_read("data/ATLAS data/Landscape data/ReleasePen/ReleasePen2.shp") %>%
  st_centroid(.) %>%
  st_transform(., "EPSG:4326") %>%
  st_coordinates(.)

suntimes <- getSunlightTimes(as.Date(seq(from = st_date,
                                         to = st_date + minutes(fix_rate * (n_steps-1)),
                                         length.out = n_steps)),
                             lat = atlas_pen_cent[2], 
                             lon = atlas_pen_cent[1], 
                             keep = c("sunrise", "sunset"),
);rm(atlas_pen_cent)

## calculate daily survival rates ####
### daily probability of death before winter ####
Autmort <- list(
  Autdaysno = as.numeric((ymd("2018-11-01")-ymd("2018-07-01"))), # days in autumn
  Autdays = seq(ymd("2018-07-01"), ymd("2018-11-01"), by = "day"),
  AutSurv = 0.47 # overall proportion of birds that survive this period
  # daily probability an individual dies
)
Autmort$Autdaily = ( 1 - Autmort$AutSurv^(1/Autmort$Autdaysno))

### daily probability of death during winter ####
Wintmort <- list(
  wintdaysno = as.numeric((ymd("2019-04-01")-ymd("2018-11-01"))), # days in winter
  Wintdays = seq(ymd("2018-11-01"), ymd("2019-04-01"), by = "day"),
  WintSurv = 0.36
)# overall proportion of birds that survive this period
Wintmort$Wintdaily <-(1 - Wintmort$WintSurv^(1/Wintmort$wintdaysno)) # daily probability an individual dies

### daily probability of death during spring ####
Springmort <- list(
  Springdaysno = as.numeric((ymd("2019-07-01")-ymd("2019-04-01"))), # days in spring
  Springdays = seq(ymd("2019-04-01"), ymd("2019-07-01"), by = "day"),
  SpringSurv = 0.36 # overall proportion of birds that survive this period
)
Springmort$Springdaily <-( 1 - Springmort$SpringSurv^(1/Springmort$Springdaysno)) # probability and individual dies on a day


for(ss in sites[2:length(sites)]){
  # Parallel processing set up ####
  ## create cluster of cores ####
  cl <- makeCluster(25, type = "SOCK")
  registerDoSNOW(cl)
  
  ##create progress bar for simulation loop ####
  pb <- progress_bar$new(
    format = "ID = :ID [:bar] :percent | :elapsed | eta::eta", 
    total = n_IDS, 
    clear = F
  )
  
  ID_list <- 1:n_IDS
  
  progress <- function(n) {
    pb$tick(tokens = list(ID = ID_list[n]))
  }
  
  opts <- list(progress = progress)
  # Simulation loop ####
  foreach(id = 1:n_IDS, .options.snow = opts) %dopar% {
    ## reload required packages for each worker ####
    require(tidyverse)
    require(terra)
    require(sf)
    
    pen_pts <- st_read(paste0("outputs/script_4/PA sites/", ss, "_pen_shapefile.shp"))
    
    ## create the area where dogging in occurs ####
    dogin_buffer <- st_difference(st_buffer(st_geometry(pen_pts), dist = 200), pen_pts)
    dogin_outside_edge <- st_boundary(st_buffer(st_geometry(pen_pts), dist = 200))
    
    ## load in covariate rasters (can't be passed to workers) ####
    short_list <- T
    hab <- rast(paste0("outputs/script_5/PA sites/", ss, " cropped habitat raster.tif"))
    pen <- rast(paste0("outputs/script_5/PA sites/", ss, " cropped pen distance raster.tif"))
    feed <- rast(paste0("outputs/script_5/PA sites/", ss, " cropped FAKE feeder distance raster.tif"))
    wood <- rast(paste0("outputs/script_5/PA sites/", ss, " cropped wood distance raster.tif"))
    hedges <- rast(paste0("outputs/script_5/PA sites/", ss, " cropped hedgerow distance raster.tif"))
    field_edges <- rast(paste0("outputs/script_5/PA sites/", ss, " cropped field_edges distance raster.tif"))
    
    ## bind all covariate rasters together ####
    covs <- c(feed, hab, wood, pen, hedges, field_edges)
    
    wood_rast <- rast(paste0("outputs/script_5/PA sites/", ss, " cropped wood raster.tif"))
    
    ## load in the hedges and egdes rasters ####
    hedges_edges <- rast(paste0("outputs/script_5/PA sites/", ss, " cropped trimmed hedges_edges raster.tif"))
    hedges_edges_dist <- rast(paste0("outputs/script_5/PA sites/", ss, " cropped trimmed hedges_edges distance raster.tif"))
    
    try({
      ## start the simulation ####
      sim_df <- id_sim(id, sl_pars, ta_pars, ssf_betas, cov_names, pen_pts, dogin_dates, dogin_times, 
                       dogin_prob, dogin_buffer, dogin_outside_edge, covs, wood_rast, Autmort, Wintmort, Springmort, 
                       st_date, n_IDs, n_steps, n_csteps, fix_rate, stop_if_left, suntimes, short_list, 
                       hedges_edges, hedges_edges_dist) %>%
        mutate(site = ss)
      
      ## save the simulation ####
      saveRDS(sim_df, paste0("outputs/script_6/PA sites/1_no_manage/Bo_site/", id, "_sim_output_site_", ss, ".rds"))
      rm(sim_df)
    })
  }; stopCluster(cl)
}
