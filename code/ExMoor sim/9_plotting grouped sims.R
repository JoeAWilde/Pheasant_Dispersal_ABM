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
# for(ss in c("B")){
  sim_df <- readRDS("outputs/script_6/APHA output/simulation_data_doginchange.rds") %>%
    filter(site == ss) %>%
    mutate(month = month.name[month(DateTime)],
           dist_from_pen = sqrt(x^2 + y^2), 
           sl_ = sqrt(((x - lag(x))^2 + (y - lag(y))^2)))
  
  real_df <- readRDS("data/Data for Exeter - anonymised GPSV2/combined_current_tracks.rds") %>%
    filter(site == ss) %>%
    mutate(
      DateTime = ymd_hms(DateTime), 
      month = month.name[month(DateTime)],
      dist_from_pen = sqrt(X_coord^2 + Y_coord^2), 
      sl_ = sqrt(((X_coord - lag(X_coord))^2 + (Y_coord - lag(Y_coord))^2))
    )
  
  for(m in month.name) {
    mth_sim <- subset(sim_df, month == m)
    
    for(i in seq(1, max(sim_df$id), 10)) {
      if(i == 1) {
        sim_alive_vec <- c(length(unique(mth_sim$id[mth_sim$id %in% i : (i+9)])))
      } else {
        sim_alive_vec <- append(sim_alive_vec, 
                                length(unique(mth_sim$id[mth_sim$id %in% i : (i+9)])))
      }
    }
    
    if(m == month.name[1]) {
      alive_df <- data.frame(month = m, 
                             no_alive_real = length(unique(real_df$ID[real_df$month == m])), 
                             no_alive_sim_mean = mean(sim_alive_vec), 
                             no_alive_sim_sd = sd(sim_alive_vec))
    } else {
      alive_df <- rbind(alive_df, 
                        data.frame(month = m, 
                                   no_alive_real = length(unique(real_df$ID[real_df$month == m])), 
                                   no_alive_sim_mean = mean(sim_alive_vec), 
                                   no_alive_sim_sd = sd(sim_alive_vec)))
    }
  }
  
  count_df <- data.frame(
    month = rep(month.name, each = 51*9),
    dist_from_pen_band = rep_len(rep(c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000),
                                     each = 51), 51*9),
    sim_group = rep_len(c(1:50, NA), 51*9),
    data_type = rep_len(c(rep_len("sim", 50), "real"), 51*9),
    count = rep_len(0, 51*9),
    prop = rep_len(0, 51*9)
  )
  
  pb <- progress_bar$new(total = nrow(count_df), format = "[:bar] :percent eta::eta", 
                         clear = F)
  for(i in 1:nrow(count_df)) {
    if(count_df$data_type[i] == "sim") {
      
      ith_ids <- seq(1, max(sim_df$id, na.rm = T), 10)[count_df$sim_group[i]] : 
        (seq(1, max(sim_df$id, na.rm = T), 10)[count_df$sim_group[i]] + 9)
      
      sub_sim_df <- subset(sim_df, id %in% ith_ids)
      
      if(count_df$dist_from_pen_band[i] != 2000) {
        count_df$count[i] <- length(which(sub_sim_df$dist_from_pen > count_df$dist_from_pen_band[i] & 
                                            sub_sim_df$dist_from_pen <= count_df$dist_from_pen_band[i+51] & 
                                            sub_sim_df$month == count_df$month[i]))
        count_df$prop[i] <- count_df$count[i] / length(which(sub_sim_df$month == count_df$month[i]))
      } else {
        count_df$count[i] <- length(which(sub_sim_df$dist_from_pen > count_df$dist_from_pen_band[i] &
                                            sub_sim_df$month == count_df$month[i]))
        count_df$prop[i] <- count_df$count[i] / length(which(sub_sim_df$month == count_df$month[i]))
      }
      
    } else {
      if(count_df$dist_from_pen_band[i] != 2000) {
        count_df$count[i] <- length(which(real_df$dist_from_pen > count_df$dist_from_pen_band[i] & 
                                            real_df$dist_from_pen <= count_df$dist_from_pen_band[i+51] & 
                                            real_df$month == count_df$month[i]))
        count_df$prop[i] <- count_df$count[i] / length(which(real_df$month == count_df$month[i]))
      } else {
        count_df$count[i] <- length(which(real_df$dist_from_pen > count_df$dist_from_pen_band[i] &
                                            real_df$month == count_df$month[i]))
        count_df$prop[i] <- count_df$count[i] / length(which(real_df$month == count_df$month[i]))
      }
    }
    pb$tick()
  }
  
  count_small <- count_df %>%
    filter(month %in% unique(real_df$month)) %>%
    mutate(month = factor(month, levels = month.name[c(7:12, 1:6)]), 
           prop = if_else(is.na(prop), 0, prop))
  
  for(i in 1:nrow(count_small)) {
    count_small$dist_from_pen_str[i] <- ifelse(
      count_small$dist_from_pen_band[i] == 2000, 
      paste0(count_small$dist_from_pen_band[i], "m+"), 
      paste0(count_small$dist_from_pen_band[i], " - ", count_small$dist_from_pen_band[i+11], 
             "m")
    )
  }
  
  count_small$dist_from_pen_str <- factor(count_small$dist_from_pen_str, 
                                          levels = c("0 - 250m", "250 - 500m", "500 - 750m",
                                                     "750 - 1000m", "1000 - 1250m",
                                                     "1250 - 1500m","1500 - 1750m",
                                                     "1750 - 2000m", "2000m+"))
  
  sim_count <- count_small[count_small$data_type == "sim",]
  
  summ_count <- sim_count %>%
    group_by(dist_from_pen_band, month) %>%
    summarise(mean_count = mean(count), 
              max_count = max(count), 
              min_count = min(count), 
              sd_count = sd(count),
              mean_prop = mean(prop), 
              min_prop = min(prop), 
              max_prop = max(prop), 
              sd_prop = sd(prop))%>%
    ungroup()
  
  final_df <- rbind(summ_count %>%
                      mutate(data_type = "sim"), count_small[count_small$data_type == "real",] %>%
                      select(month, dist_from_pen_band, count, prop) %>%
                      rename(mean_count = count, 
                             mean_prop = prop) %>%
                      mutate(min_count = NA, #
                             max_count = NA, 
                             sd_count = NA, 
                             min_prop = NA,
                             max_prop = NA,
                             sd_prop = NA, 
                             data_type = "real")) %>%
    arrange(dist_from_pen_band, month)
  
  for(i in 1:nrow(final_df)) {
    final_df$dist_from_pen_str[i] <- ifelse(
      final_df$dist_from_pen_band[i] == 2000, 
      paste0(final_df$dist_from_pen_band[i], "m+"), 
      paste0(final_df$dist_from_pen_band[i], " - ", final_df$dist_from_pen_band[i+16], 
             "m")
    )
  }
  
  final_df$dist_from_pen_str <- factor(final_df$dist_from_pen_str, 
                                       levels = c("0 - 250m", "250 - 500m", "500 - 750m",
                                                  "750 - 1000m", "1000 - 1250m",
                                                  "1250 - 1500m","1500 - 1750m",
                                                  "1750 - 2000m", "2000m+"))
  
  final_df <- final_df %>%
    mutate(sd_low_count = if_else(mean_count - sd_count < 0, 0, mean_count - sd_count), 
           sd_high_count = mean_count + sd_count, 
           sd_low_prop = if_else(mean_prop - sd_prop < 0, 0, mean_prop - sd_prop), 
           sd_high_prop = mean_prop + sd_prop)
  
  
  
  p1 <- ggplot(data = final_df) +
    geom_col(aes(x = dist_from_pen_str, y = mean_count, fill = data_type,
                 group = data_type), position = position_dodge()) + 
    geom_errorbar(aes(x = dist_from_pen_str, ymin=sd_low_count, ymax=sd_high_count,
                      group = data_type),
                  position="dodge") +
    geom_point(aes(x = dist_from_pen_str, y = min_count, group = data_type), shape = 1, position = position_dodge(width=0.9)) + 
    geom_point(aes(x = dist_from_pen_str, y = max_count, group = data_type), shape = 1, position = position_dodge(width=0.9)) + 
    scale_y_continuous(name = "Number of fixes") + 
    scale_x_discrete(name = "Distance from Pen (m)") + 
    scale_fill_manual(name = "Data type", values = c("navy", "orange3")) + 
    theme_classic() + 
    facet_wrap(vars(month))
  p1
  
  ggsave(p1, filename = paste0("outputs/script_9/APHA data/site ", ss, "/distance_counts_doginchange.png"), units = "px", height = 4320, width = 7980)
  
  p2 <- ggplot(data = final_df) +
    geom_col(aes(x = dist_from_pen_str, y = mean_prop, fill = data_type,
                 group = data_type), position = position_dodge()) + 
    geom_errorbar(aes(x = dist_from_pen_str, ymin=sd_low_prop, ymax=sd_high_prop,
                      group = data_type),
                  position="dodge") +
    geom_point(aes(x = dist_from_pen_str, y = min_prop, group = data_type), shape = 1, position = position_dodge(width=0.9)) + 
    geom_point(aes(x = dist_from_pen_str, y = max_prop, group = data_type), shape = 1, position = position_dodge(width=0.9)) + 
    scale_y_continuous(name = "Proportion of fixes") + 
    scale_x_discrete(name = "Distance from Pen (m)") + 
    scale_fill_manual(name = "Data type", values = c("navy", "orange3")) + 
    theme_classic() + 
    facet_wrap(vars(month))
  p2
  ggsave(p2, filename = paste0("outputs/script_9/APHA data/site ", ss, "/distance_prop_doginchange.png"), units = "px", height = 4320, width = 7980)
}