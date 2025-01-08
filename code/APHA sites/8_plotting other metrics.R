library(tidyverse)
library(progress)
library(amt)
library(sf)
library(terra)
library(tidyterra)
library(ggforce)
library(ggnewscale)
library(ggblend)

CRS_used <- "EPSG:27700"

for(ss in c("A", "B", "D")){

  sim_df <- readRDS("outputs/script_6/APHA output/simulation_data_rp.rds") %>%
    filter(site == ss) %>%
    mutate(month = month.name[month(DateTime)],
           dist = sqrt(x^2 + y^2), 
           sl_ = sqrt(((x - lag(x))^2 + (y - lag(y))^2)))
  
  real_df <- readRDS("data/Data for Exeter - anonymised GPSV2/combined_current_tracks.rds") %>%
    filter(site == ss) %>%
    mutate(
      DateTime = ymd_hms(DateTime), 
      month = month.name[month(DateTime)],
      dist = sqrt(X_coord^2 + Y_coord^2), 
      sl_ = sqrt(((X_coord - lag(X_coord))^2 + (Y_coord - lag(Y_coord))^2))
    )
  
  pp <- ggplot() + 
    geom_density(data = sim_df, aes(x = sl_, fill = "Simulated"),colour = "black", alpha = 0.5) + 
    geom_density(data = real_df, aes(x = sl_, fill = "Observed"), colour = "black", alpha = 0.5) + 
    scale_x_continuous(name = "Step lengths (m)", limits = c(0,1000)) + 
    scale_y_continuous(name = "Density", breaks = NULL) + 
    scale_fill_manual(name = "Data type", values = c("Observed" = "navy", "Simulated" = "orange")) + 
    theme_classic(base_size = 30)
  pp  
  
  ggsave(pp, filename = paste0("outputs/script_8/APHA data/site ", ss, "/step length comp.png"), units = "px", height = 4320, width = 7980)
  
  
  count_df <- data.frame(
    month = rep(month.name, each = 18),
    dist_band = rep_len(rep(c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000),
                            each = 2), 216),
    data_type = rep_len(c("sim", "real"), 216),
    count = rep_len(0, 216),
    prop = rep_len(0, 216)
  )
  
  pb <- progress_bar$new(total = nrow(count_df), format = "[:bar] :percent eta::eta", 
                         clear = F)
  for(i in 1:nrow(count_df)) {
    if(count_df$data_type[i] == "sim") {
      if(count_df$dist_band[i] != 2000) {
        count_df$count[i] <- length(which(sim_df$dist > count_df$dist_band[i] & 
                                            sim_df$dist <= count_df$dist_band[i+2] & 
                                            sim_df$month == count_df$month[i]))
        count_df$prop[i] <- count_df$count[i] / length(which(sim_df$month == count_df$month[i]))
      } else {
        count_df$count[i] <- length(which(sim_df$dist > count_df$dist_band[i] &
                                            sim_df$month == count_df$month[i]))
        count_df$prop[i] <- count_df$count[i] / length(which(sim_df$month == count_df$month[i]))
      }
      
    } else {
      if(count_df$dist_band[i] != 2000) {
        count_df$count[i] <- length(which(real_df$dist > count_df$dist_band[i] & 
                                            real_df$dist <= count_df$dist_band[i+2] & 
                                            real_df$month == count_df$month[i]))
        count_df$prop[i] <- count_df$count[i] / length(which(real_df$month == count_df$month[i]))
      } else {
        count_df$count[i] <- length(which(real_df$dist > count_df$dist_band[i] &
                                            real_df$month == count_df$month[i]))
        count_df$prop[i] <- count_df$count[i] / length(which(real_df$month == count_df$month[i]))
      }
    }
    pb$tick()
  }
  
  count_small <- count_df %>%
    filter(month %in% unique(real_df$month)) %>%
    mutate(month = factor(month, levels = month.name[c(7:12, 1:6)]), 
           prop = if_else(is.na(prop), 0, prop), 
           dist_band_str = factor(if_else(
             dist_band == 2000, 
             paste0(dist_band, "m+"), 
             paste0(dist_band, "-", lead(lead(dist_band)), "m")), 
             levels = c("0-250m", "250-500m", "500-750m", "750-1000m",
                        "1000-1250m", "1250-1500m", "1500-1750m", "1750-2000m", "2000m+")))
  
  p1 <- ggplot(data = count_small, aes(x = dist_band_str,
                                       y = prop, fill = data_type)) + 
    geom_col(position = position_dodge()) + 
    scale_fill_manual(name = "Data type", values = c("navy", "orange3"), 
                      labels = c("Observed", "Simulated")) + 
    scale_x_discrete(name = "Distance band") + 
    scale_y_continuous(name = "Proportion of fixes") + 
    theme_bw(base_size = 17) + 
    facet_wrap(vars(month), axes = "all_x")
  p1
  ggsave(p1, filename = paste0("outputs/script_8/APHA data/site ", ss, "/distance_props_rp.png"), units = "px", height = 4320, width = 7980)
  
  p2 <- ggplot(data = count_small, aes(x = dist_band_str,
                                       y = count, fill = data_type)) + 
    geom_col(position = position_dodge()) + 
    scale_fill_manual(name = "Data type", values = c("navy", "orange3"), 
                      labels = c("Observed", "Simulated")) + 
    scale_x_discrete(name = "Distance band") + 
    scale_y_continuous(name = "Total number of fixes") + 
    theme_bw(base_size = 17) + 
    facet_wrap(vars(month), axes = "all_x")
  p2
  ggsave(p2, filename = paste0("outputs/script_8/APHA data/site ", ss, "/distance_counts_rp.png"), units = "px", height = 4320, width = 7980)
  
}

