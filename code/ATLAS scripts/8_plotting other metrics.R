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

ggplot(data = sim_df[sim_df$month == "July",], aes(x = x, y = y, colour = id)) + 
  geom_path(show.legend = F) + 
  theme_classic()

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

pp <- ggplot() + 
  geom_density(data = sim_df[sim_df$sl_!=0, ], aes(x = sl_, fill = "Simulated"),colour = "black", alpha = 0.5) + 
  geom_density(data = real_df, aes(x = dist, fill = "Observed"), colour = "black", alpha = 0.5) + 
  scale_x_continuous(name = "Step lengths (m)", limits = c(0, 400)) + 
  scale_y_continuous(name = "Density", breaks = NULL) + 
  scale_fill_manual(name = "Data type", values = c("Observed" = "navy", "Simulated" = "orange")) + 
  theme_classic(base_size = 30)
pp  

ggsave(pp, filename = "outputs/script_8/ATLAS data/step length comp.png", units = "px", height = 4320, width = 7980)


count_df <- data.frame(
  month = rep(month.name, each = 18),
  dist_from_pen_band = rep_len(rep(c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000),
                          each = 2), 216),
  data_type = rep_len(c("sim", "real"), 216),
  count = rep_len(0, 216),
  prop = rep_len(0, 216)
)

pb <- progress_bar$new(total = nrow(count_df), format = "[:bar] :percent eta::eta", 
                       clear = F)
for(i in 1:nrow(count_df)) {
  if(count_df$data_type[i] == "sim") {
    if(count_df$dist_from_pen_band[i] != 2000) {
      count_df$count[i] <- length(which(sim_df$dist_from_pen > count_df$dist_from_pen_band[i] & 
                                          sim_df$dist_from_pen <= count_df$dist_from_pen_band[i+2] & 
                                          sim_df$month == count_df$month[i]))
      count_df$prop[i] <- count_df$count[i] / length(which(sim_df$month == count_df$month[i]))
    } else {
      count_df$count[i] <- length(which(sim_df$dist_from_pen > count_df$dist_from_pen_band[i] &
                                          sim_df$month == count_df$month[i]))
      count_df$prop[i] <- count_df$count[i] / length(which(sim_df$month == count_df$month[i]))
    }
    
  } else {
    if(count_df$dist_from_pen_band[i] != 2000) {
      count_df$count[i] <- length(which(real_df$dist_from_pen > count_df$dist_from_pen_band[i] & 
                                          real_df$dist_from_pen <= count_df$dist_from_pen_band[i+2] & 
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
         prop = if_else(is.na(prop), 0, prop), 
         dist_from_pen_band_str = factor(if_else(
           dist_from_pen_band == 2000, 
           paste0(dist_from_pen_band, "m+"), 
           paste0(dist_from_pen_band, "-", lead(lead(dist_from_pen_band)), "m")), 
           levels = c("0-250m", "250-500m", "500-750m", "750-1000m",
                      "1000-1250m", "1250-1500m", "1500-1750m", "1750-2000m", "2000m+")))

p1 <- ggplot(data = count_small, aes(x = dist_from_pen_band_str,
                                     y = prop, fill = data_type)) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual(name = "Data type", values = c("navy", "orange3"), 
                    labels = c("Observed", "Simulated")) + 
  scale_x_discrete(name = "dist_from_penance band") + 
  scale_y_continuous(name = "Proportion of fixes") + 
  theme_bw(base_size = 17) + 
  facet_wrap(vars(month), axes = "all_x")
p1
ggsave(p1, filename = "outputs/script_8/ATLAS data/dist_from_penance_props.png", units = "px", height = 4320, width = 7980)

p2 <- ggplot(data = count_small, aes(x = dist_from_pen_band_str,
                                     y = count, fill = data_type)) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual(name = "Data type", values = c("navy", "orange3"), 
                    labels = c("Observed", "Simulated")) + 
  scale_x_discrete(name = "dist_from_penance band") + 
  scale_y_continuous(name = "Total number of fixes") + 
  theme_bw(base_size = 17) + 
  facet_wrap(vars(month), axes = "all_x")
p2
ggsave(p2, filename = "outputs/script_8/ATLAS data/dist_from_penance_counts.png", units = "px", height = 4320, width = 7980)



