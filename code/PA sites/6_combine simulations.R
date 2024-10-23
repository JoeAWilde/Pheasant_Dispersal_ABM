library(tidyverse)
library(progress)
library(doSNOW)
library(sf)

root <- "outputs/PA sites/script_5/"

site_prefix <- "Go"

sites <- paste0(site_prefix, c(0, 250, 500, 1000, 2000))

for(ss in sites) {
  if(ss == sites[1]) pb <- progress_bar$new(total = length(sites),
                                            format = "[:bar] :percent eta::eta",
                                            clear = F); pb$tick(0)
  
  sim_files <- paste0(root, list.files(root)) %>%
    .[grepl(ss, .)]

  all_df <- lapply(sim_files, readRDS) %>%
    do.call(rbind, .)
  saveRDS(all_df, paste0("outputs/PA sites/script_6/", ss, "simulation_data.rds"))
  pb$tick()
}

for(ss in sites) {
  all_sites_df <- readRDS(paste0("outputs/PA sites/script_6/", ss, "simulation_data.rds"))
  
  PAs <- st_read("data/Protected Areas/All_UK_PAs.shp") %>%
    st_transform(crs = "EPSG:27700") %>%
    st_crop(., st_buffer(st_as_sf(all_sites_df[1,], coords = c("x", "y"), crs = "EPSG:27700"), dist = 10000))
  
  sim_df <- all_sites_df %>% 
    mutate(month = month.name[month(DateTime)]) %>%
    group_by(id) %>%
    mutate(
      id_new = paste0(site, id),
      dist_from_pen = sqrt(x^2 + y^2), 
      sl_ = sqrt(((x - lag(x))^2 + (y - lag(y))^2))) %>%
    ungroup()
  
  sum_df <- data.frame(
    sim_group = rep_len(1:100, 100*9*12),
    dist_from_pen_band = rep_len(seq(0, 2000, 275), 100*9*12),
    mean_birds = 0,
    mean_fixes = 0,
    mean_monthly_fixes = 0,
    mean_prop = 0,
    month = rep_len(rep(month.name, each = 9), 100*12*9),
    data_type = "sim",
    no_sites = length(unique(sim_df$site)),
    sites = paste0("Gormire ", ss, "m away from PA boundary"),
    sd_birds = NA,
    max_fixes = NA,
    min_fixes = NA,
    sd_fixes = NA,
    min_prop = NA,
    max_prop = NA,
    sd_prop = NA,
    sd_low_fixes = NA,
    sd_high_fixes = NA,
    sd_low_prop = NA,
    sd_high_prop = NA,
    total_fixes = 0,
    mean_prop_total_fixes = 0,
    min_prop_total_fixes = NA,
    max_prop_total_fixes = NA,
    sd_low_prop_total_fixes = NA,
    sd_high_prop_total_fixes = NA,
    total_birds_released = 10,
    prop_birds_alive = 0, 
    monthly_birdhours_PA = 0
  )
  
  
  # Parallel processing set up ####
  ## create cluster of cores ####
  cl <- makeCluster(parallel::detectCores(logical = F)-1, type = "SOCK")
  registerDoSNOW(cl)
  
  ##create progress bar for simulation loop ####
  pb <- progress_bar$new(
    format = "[:bar] :percent | :elapsed | eta::eta", 
    total = nrow(sum_df), 
    clear = F
  )
  
  progress <- function(n) {
    pb$tick()
  }
  
  opts <- list(progress = progress)
  # Simulation loop ####
  foreach(i = 1:nrow(sum_df), .options.snow = opts) %dopar% {
    require(tidyverse)
    require(sf)
    # if(i == 1) pb <- progress_bar$new(total = nrow(sum_df), format = "[:bar] :percent eta::eta", 
    #                                   clear = F); pb$tick(0)
    ith_ids <- seq(1, max(sim_df$id, na.rm = T), 10)[sum_df$sim_group[i]] : 
      (seq(1, max(sim_df$id, na.rm = T), 10)[sum_df$sim_group[i]] + 9)
    
    sub_sim_df <- subset(sim_df, id %in% ith_ids)
    
    sum_df$total_fixes[i] <- nrow(sub_sim_df)
    sum_df$mean_birds[i] <- length(unique(sub_sim_df$id_new[sub_sim_df$month == sum_df$month[i]]))
    sum_df$prop_birds_alive[i] <- sum_df$mean_birds[i] / sum_df$total_birds_released[i]
    sum_df$mean_monthly_fixes[i] <- nrow(sub_sim_df[sub_sim_df$month == sum_df$month[i],])
    
    if(sum_df$dist_from_pen_band[i] == 2000) {
      sub_df <- sub_sim_df %>%
        dplyr::filter(
          month == sum_df$month[i] &
            dist_from_pen >= sum_df$dist_from_pen_band[i]
        )
    } else {
      sub_df <- sub_sim_df %>%
        dplyr::filter(
          month == sum_df$month[i] &
            dist_from_pen >= sum_df$dist_from_pen_band[i] &
            dist_from_pen < sum_df$dist_from_pen_band[i+1]
        )
    }
    
    sum_df$mean_fixes[i] <- nrow(sub_df)
    sum_df$mean_prop[i] <- sum_df$mean_fixes[i] / sum_df$mean_monthly_fixes[i]
    sum_df$mean_prop_total_fixes[i] <- sum_df$mean_fixes[i] / sum_df$total_fixes[i]
    
    in_PA <- lengths(st_within(
      st_as_sf(sub_sim_df[sub_sim_df$month == sum_df$month[i],],
               coords = c("x", "y"), crs = "EPSG:27700"), PAs)) > 0
    
    sum_df$monthly_birdhours_PA[i] <- length(in_PA[in_PA == T])
    
    filename <- paste0("outputs/PA sites/script_6/summarised data loop output/", ss, "_", i, "_summ_data.rds")
    
    saveRDS(sum_df[i,], filename)
  }
  
  root <- "outputs/PA sites/script_6/summarised data loop output/"
  files_to_read <- paste0(root, list.files(root)) %>%
    .[grepl(ss, .)]
  
  sum_df <- lapply(files_to_read, readRDS) %>%
    do.call(rbind , .)
  
  # for(fn in 1:length(files_to_read)) {
  #   
  #   sum_df[fn, ] <- readRDS(paste0("outputs/PA sites/script_6/summarised data loop output/", 
  #                                  files_to_read[fn]))
  # }
  
  
  summ_fixes <- sum_df %>%
    group_by(dist_from_pen_band, month) %>%
    mutate(mean_fixes_1 = mean(mean_fixes, na.rm = T), 
           max_fixes = max(mean_fixes, na.rm = T), 
           min_fixes = min(mean_fixes, na.rm = T), 
           sd_fixes = sd(mean_fixes, na.rm = T),
           mean_prop_1 = mean(mean_prop, na.rm = T), 
           min_prop = min(mean_prop, na.rm = T), 
           max_prop = max(mean_prop, na.rm = T), 
           sd_prop = sd(mean_prop, na.rm = T)) %>%
    select(-mean_fixes, -mean_prop) %>%
    rename(mean_fixes = mean_fixes_1, 
           mean_prop = mean_prop_1) %>%
    ungroup() %>%
    group_by(month) %>%
    mutate(mean_monthly_fixes = mean(mean_monthly_fixes), 
           mean_birds_1 = mean(mean_birds, na.rm = T), 
           sd_birds = sd(mean_birds, na.rm = T), 
           mean_birdhours_PA = mean(monthly_birdhours_PA, na.rm = T), 
           sd_birdhours_PA = sd(monthly_birdhours_PA, na.rm = T), 
           min_birdhours_PA = min(monthly_birdhours_PA, na.rm = T),
           max_birdhours_PA = max(monthly_birdhours_PA, na.rm = T)) %>%
    select(-mean_birds) %>%
    rename(mean_birds = mean_birds_1) %>%
    ungroup() %>%
    distinct(month, dist_from_pen_band, .keep_all = T) %>%
    arrange(month, dist_from_pen_band) %>%
    select(-sim_group)
  
  saveRDS(summ_fixes, 
          paste0("shiny_app/summarised simulation data/", ss, " summarised data.rds"))
}
