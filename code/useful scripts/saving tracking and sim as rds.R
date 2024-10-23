library(tidyverse)

all_APHA_sites <- readRDS("shiny_app/summarised tracking data/APHA data/all_APHA_sites.rds") %>%
  mutate(sites = "all 11") %>%
  rename(no_birds = Number_of_Birds, 
         count = Total_Number_of_Fixes, 
         number_fixes = Total_Fixes_Overall, 
         prop = Proportional_Fixes) %>%
  select(-no_birds, -data_type, -number_fixes)

APHA_3_sites <- readRDS("shiny_app/summarised tracking data/APHA data/site Asummarised tracking data.rds") %>%
  rbind(., readRDS("shiny_app/summarised tracking data/APHA data/site Bsummarised tracking data.rds")) %>%
  rbind(., readRDS("shiny_app/summarised tracking data/APHA data/site Dsummarised tracking data.rds")) %>%
  select(dist_from_pen_band, 
         month, 
         mean_count, 
         mean_prop) %>%
  rename(distance_bins = dist_from_pen_band, 
         count = mean_count, 
         prop = mean_prop) %>%
  mutate(sites = "original 3", 
         no_sites = 3) %>%
  rbind(., data.frame(
    distance_bins = rep_len(seq(0, 2000, 250), 9 * 12),
    count = 0, 
    prop = 0, 
    month = rep(month.name, each = 9), 
    no_sites = 11, 
    sites = "original 3")) %>%
  distinct(distance_bins, month, .keep_all = T) %>%
  arrange(month, distance_bins)

ATLAS_sites <- readRDS("shiny_app/summarised tracking data/ATLAS data/ATLAS summarised data.rds") %>%
  select(-number_fixes, -number_birds) %>%
  rename(sites = site, 
         distance_bins = distance_bin) %>%
  mutate(no_sites = 1, 
         prop = 0)

all_sites <- rbind(all_APHA_sites, APHA_3_sites, ATLAS_sites) %>%
  group_by(month, distance_bins) %>%
  mutate(count = sum(count)) %>%
  ungroup() %>%
  distinct(distance_bins, month, .keep_all = T) %>%
  arrange(month, distance_bins) %>%
  mutate(no_sites = 12, 
         sites = "all sites")

all_tracking <- rbind(all_APHA_sites, APHA_3_sites, ATLAS_sites, all_sites)  

saveRDS(all_tracking, "shiny_app/summarised tracking data/all_summarised_tracking_data.rds")

sim <- readRDS("shiny_app/summarised simulation data/kde_woodland_sim.rds") %>%
  rename(distance_bins = dist_from_pen_band, 
         count = mean_count, 
         prop = mean_prop) %>%
  select(-dist_from_pen_str) %>%
  mutate(data_type = "simulation", 
         no_sites = 1) %>%
  rename(sites = data_type)

sim_and_track <- all_tracking %>%
  mutate(
    max_count = NA, 
    min_count = NA, 
    sd_count = NA, 
    min_prop = NA, 
    max_prop = NA, 
    sd_prop = NA, 
    sd_low_count = NA,
    sd_high_count = NA, 
    sd_low_prop = NA, 
    sd_high_prop = NA
  ) %>%
  rbind(., sim)

saveRDS(sim_and_track, "shiny_app/simulation_and_all_tracking_summarised_data.rds")
