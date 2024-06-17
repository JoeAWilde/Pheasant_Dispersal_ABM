#Script to run an integrated step-selection function to determine which habitats and other covariates dictate where the pheasants move to

#load required libraries
library(tidyverse)
library(amt)
library(terra)
source("code/functions/RandomSteps.R")

# # ATLAS data ####
# 
# ## load in the tracking data and subset to hourly ####
# df <- read.table("data/Pheas_filtered.csv", sep = ",", header = T) %>%
#   mutate(DateTime = ymd_hms(DateTime), 
#          Deploydatetime = ymd_hms(Deploydatetime)) %>%
#   filter(year(Deploydatetime) == "2018") %>%
#   group_by(ID) %>%
#   filter(minute(DateTime) == minute(DateTime[1])) %>%
#   mutate(
#     sl = sqrt((X - lag(X))^2 + ((Y - lag(Y))^2)), 
#     ta = atan2(Y - lag(Y), X - lag(X)), 
#     difftime = as.numeric(difftime(DateTime, lag(DateTime), units = "days")),
#     sl = if_else(difftime > 0.05, NA, sl), 
#     sl = if_else(sl > 1000, NA, sl), 
#     SinceRel = as.numeric(difftime(DateTime, DateTime[1], units = "days")), 
#     site = "NORTHWYKE"
#   ) %>%
#   ungroup()
# 
# ## turn into amt track ####
# trk <- make_track(tbl = df, .x = X, .y = Y, .t = DateTime, 
#                      site = site, ID = ID, SinceRel = SinceRel, crs = 27700)
# 
# ## I have no idea what this bit does. Seems crucial though. ####
# at_stp <- trk %>%
#   nest(data = -"ID") %>% # nest by individual
#   mutate(samp = amt::map(data, ~ .x %>% track_resample(rate = minutes(60), tolerance = minutes(10)))) %>%
#   dplyr::select(ID, samp) %>%
#   unnest(cols = samp)
# 
# ## remove short bursts ####
# at_stp <- at_stp %>% 
#   mutate(burst_ = paste0(ID, "_", burst_)) %>% 
#   filter_min_n_burst(min_n = 2) %>%
#   
#   ## create steps object, calculate step length and turning angle but only within burst ####
#   steps_by_burst(keep_cols = 'start')
# 
# ## estimate parameters used to draw control steps ####
# ### gamma distribution (fit to step lengths) ####
# SL_dist <- fit_distr(at_stp$sl_, "gamma"); SL_dist
# 
# ### von Mises distribution (fit to turn angles) ####
# TA_dist <- fit_distr(at_stp$ta_, "vonmises"); TA_dist
# 
# ## generate control steps ####
# at_stp <- at_stp %>%
#   random_steps2(n_control = 250, ta_distr = TA_dist, sl_distr = SL_dist)
# 
# ## load in covariate rasters ####
# hab <- rast("outputs/script_1/ATLAS outputs/cropped habitat raster.tif")
# feed <- rast("outputs/script_1/ATLAS outputs/cropped feeder distance raster.tif")
# pen <- rast("outputs/script_1/ATLAS outputs/cropped pen distance raster.tif")
# wood <- rast("outputs/script_1/ATLAS outputs/cropped wood distance raster.tif")
# 
# ## bind rasters into onto vector ####
# covs <- c(hab, feed, pen, wood)
# 
# ## extract covariate values for all steps ####
# at_stp <- extract_covariates(at_stp, covs)
# names(at_stp)[16:19] <- c("hab", "feed", "pen", "wood")
# 
# 
# # APHA data ####
# sites <- c("A", "B", "D")
# 
# ## load tracking data ####
# df <- readRDS("data/Data for Exeter - anonymised GPSV2/current_tracks_tod.rds") %>%
#   filter(tod == "DAY")
# 
# ## turn into amt track ####
# trk <- make_track(tbl = df, .x = X_coord, .y = Y_coord, .t = DateTime, site = site,
#                   ID = ID, SinceRel = SinceRel, crs = 27700)
# ## ??? ####
# ap_stp <- trk %>%
#   nest(data = -"ID") %>% # nest by individual
#   mutate(samp = amt::map(data, ~ .x %>% track_resample(rate = minutes(60), tolerance = minutes(10)))) %>%
#   dplyr::select(ID, samp) %>%
#   unnest(cols = samp) # unest data column
# 
# ## remove short bursts ####
# ap_stp <- ap_stp %>% 
#   mutate(burst_ = paste0(ID, "_", burst_)) %>% 
#   filter_min_n_burst(min_n = 2) %>%
# 
# ## now create steps object, calculate step length and turning angle but only within burst ####
#   steps_by_burst(keep_cols = 'start')
# 
# ## look at the parameters that we are going to use to draw random steps ####
# ### gamma distribution (fit to step lengths) ####
# SL_dist <- fit_distr(ap_stp$sl_, "gamma"); SL_dist
# 
# ### von Mises distribution (fit to turn angles) ####
# TA_dist <- fit_distr(ap_stp$ta_, "vonmises"); TA_dist
# 
# ## generate random steps ####
# ap_stp <- ap_stp %>% 
#   random_steps2(n_control = 250, ta_distr = TA_dist, sl_distr = SL_dist)
# 
# for(site in sites) {
#   hab <- rast(paste0("outputs/script_1/APHA outputs/site ", 
#                      site, "/site ", site," cropped habitat raster.tif"))
#   
#   feed <- rast(paste0("outputs/script_1/APHA outputs/site ", 
#                       site, "/site ", site," cropped feeder distance raster.tif"))
#   
#   pen <- rast(paste0("outputs/script_1/APHA outputs/site ", 
#                      site, "/site ", site," cropped pen distance raster.tif"))
#   
#   wood <- rast(paste0("outputs/script_1/APHA outputs/site ", 
#                       site, "/site ", site," cropped wood distance raster.tif"))
#   
#   sub_stp <- ap_stp[ap_stp$site == site,]
#   
#   sub_stp <- extract_covariates(sub_stp, feed) %>%
#     rename(feed = ncol(.))
#   sub_stp <- extract_covariates(sub_stp, pen) %>%
#     rename(pen = ncol(.))
#   sub_stp <- extract_covariates(sub_stp, hab )%>%
#     rename(hab = ncol(.))
#   sub_stp <- extract_covariates(sub_stp, wood )%>%
#     rename(wood = ncol(.))
#   
#   if(site == sites[1]) {
#     cov_stp <- sub_stp
#   } else {
#     cov_stp <- rbind(cov_stp, sub_stp)
#   }
# }
# 
# # Combined data ####
# 
# ## bind all data into one ####
# all_stp <- rbind(cov_stp, at_stp)
# 
# ## relevel habitat covariate as a factor ####
# all_stp$hab <- factor(all_stp$hab)
# 
# ## add log(sl) and cos(ta) as covariates ####
# all_stp <- all_stp %>%
#   mutate(log_sl_ = log(sl_),
#          cos_ta_ = cos(ta_))
# 
# ## save as RDS ####
# saveRDS(all_stp, "outputs/script_3/all_sites_with_control_steps.rds")

all_stp <- readRDS("outputs/script_3/all_sites_with_control_steps.rds")

# Step selection function ####

## run the model ####
m1 <- fit_issf(all_stp, 
               # Response
               case_ ~ 
                 # Habitat
                 feed + hab + wood + 
                 # Pen attraction (Plot code below works if you take out these two terms below)
                 pen + pen:SinceRel + 
                 # Movement
                 sl_ + log_sl_ + cos_ta_ + 
                 # Stratum
                 strata(step_id_),
               # Need this later for model predictions
               model = TRUE)

## inspect summary ####
summary(m1)

## save the ssf model ####
saveRDS(m1, "outputs/script_3/iSSF.rds")

## extract coefficients and save ####
saveRDS(summary(m1)$coefficients[, 1], "outputs/script_3/iSSF_coefficients.rds")
