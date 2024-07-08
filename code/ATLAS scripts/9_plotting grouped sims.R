library(tidyverse)
library(progress)
library(amt)
library(sf)
library(terra)
library(tidyterra)
library(ggforce)
library(ggnewscale)
library(ggblend)

#load in the shape of the release pen
pen <- st_read("data/ReleasePen/ReleasePen2.shp") %>%
  st_transform(., crs = "EPSG:27700")

#save the centre of the pen as a single point
cen_pen <- st_centroid(pen) %>%
  st_coordinates()


sim_df <- readRDS("outputs/script_6/ATLAS output/simulation_data.rds") %>%
  filter(BirdDead!=1) %>%
  mutate(cent_X = x - cen_pen[1,1], 
         cent_Y = y - cen_pen[1,2],
         month = month.name[month(DateTime)],
         dist_from_pen = sqrt(cent_X^2 + cent_Y^2))

real_df <- read.csv("data/Pheas_filtered.csv") %>%
  mutate(DateTime = ymd_hms(DateTime)) %>%
  filter(year(Deploydatetime) == 2018) %>%
  group_by(ID) %>%
  filter(minute(DateTime) == minute(DateTime[1])) %>%
  ungroup() %>%
  mutate(
    cent_X = X - cen_pen[1,1], 
    cent_Y = Y - cen_pen[1,2],
    DateTime = ymd_hms(DateTime), 
    month = month.name[month(DateTime)],
    dist_from_pen = sqrt(cent_X^2 + cent_Y^2))


count_df <- data.frame(
  month = rep(month.name, each = 11*9),
  dist_from_pen_band = rep_len(rep(c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000),
                                   each = 11), 11*9),
  sim_group = rep_len(c(1:10, NA), 11*9),
  data_type = rep_len(c(rep_len("sim", 10), "real"), 11*9),
  count = rep_len(0, 11*9),
  prop = rep_len(0, 11*9)
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
                                          sub_sim_df$dist_from_pen <= count_df$dist_from_pen_band[i+11] & 
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
                                          real_df$dist_from_pen <= count_df$dist_from_pen_band[i+11] & 
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
            mean_prop = mean(prop), 
            min_prop = min(prop), 
            max_prop = max(prop))%>%
  ungroup()

final_df <- rbind(summ_count %>%
                    mutate(data_type = "sim"), count_small[count_small$data_type == "real",] %>%
                    select(month, dist_from_pen_band, count, prop) %>%
                    rename(mean_count = count, 
                           mean_prop = prop) %>%
                    mutate(min_count = NA, #
                           max_count = NA, 
                           min_prop = NA,
                           max_prop = NA,
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



p1 <- ggplot(data = final_df) +
  geom_col(aes(x = dist_from_pen_str, y = mean_count, fill = data_type,
               group = data_type), position = position_dodge()) + 
  geom_errorbar(aes(x = dist_from_pen_str, ymin=min_count, ymax=max_count,
                    group = data_type),
                position="dodge") +
  scale_y_continuous(name = "Number of fixes") + 
  scale_x_discrete(name = "Distance from Pen (m)") + 
  scale_fill_manual(name = "Data type", values = c("navy", "orange3")) + 
  theme_classic() + 
  facet_wrap(vars(month))
p1
ggsave(p1, filename = "outputs/script_9/ATLAS data/distance_counts.png", units = "px", height = 4320, width = 7980)

p2 <- ggplot(data = final_df) +
  geom_col(aes(x = dist_from_pen_str, y = mean_prop, fill = data_type,
               group = data_type), position = position_dodge()) + 
  geom_errorbar(aes(x = dist_from_pen_str, ymin=min_prop, ymax=max_prop,
                    group = data_type),
                position="dodge") +
  scale_y_continuous(name = "Proportion of fixes") + 
  scale_x_discrete(name = "Distance from Pen (m)") + 
  scale_fill_manual(name = "Data type", values = c("navy", "orange3")) + 
  theme_classic() + 
  facet_wrap(vars(month))
p2
ggsave(p2, filename = "outputs/script_9/ATLAS data/distance_prop.png", units = "px", height = 4320, width = 7980)
