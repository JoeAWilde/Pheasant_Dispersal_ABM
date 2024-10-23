library(tidyverse)
library(tidylog)
library(progress)
library(sf)
library(readxl)


#Claire Turner data ####
df <- read.table("data/Claire Turner PhD/AllFixes.csv", sep = ",", header = T) %>%
  mutate(
    DateTime = ymd_hms(DateTime),
    dist_from_pen = sqrt(((x - pen_x)^2) + ((y - pen_y)^2)),
    month = month.name[month(DateTime)]
  ) %>%
  group_by(Tx) %>%
  mutate(
    release_date = DateTime[1],
    release_year = year(DateTime)[1]
  ) %>%
  ungroup() %>%
  mutate(
    release_year = case_when(
      release_year == 2001 ~ 1,
      release_year == 2002 ~ 2,
      release_year == 2003 ~ 3
    )
  )

for(m in 1:12) {
  if(m == 1) {
    monthly_released <- c(length(unique(df$Tx[month(df$release_date) == m])))
  } else {
    monthly_released <- append(monthly_released,
                               length(unique(df$Tx[month(df$release_date) == m])))
  }
}
cum_released <- monthly_released
for(m in c(8:12)) {

    cum_released[m] <- cum_released[m] + cum_released[m-1]
}

cum_released <- ifelse(cum_released == 0, max(cum_released),
                       ifelse(cum_released == 146, 147, cum_released))

release_df <- data.frame(month = month.name,
                         cum_released = cum_released)

sum_df <- data.frame(
  dist_from_pen_band = rep_len(seq(0, 2000, 250), 9*12),
  mean_birds = 0,
  mean_fixes = 0,
  mean_monthly_fixes = 0,
  mean_prop = 0,
  month = rep(month.name, each = 9),
  data_type = "real",
  no_sites = 4,
  sites = "GWCT radio data",
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
  total_fixes = nrow(df),
  mean_prop_total_fixes = 0,
  min_prop_total_fixes = NA,
  max_prop_total_fixes = NA,
  sd_low_total_fixes = NA,
  sd_high_prop_total_fixes = NA,
  total_birds_released = 0,
  prop_birds_alive = 0
)


for(i in 1:nrow(sum_df)) {

  if(i == 1) {
    pb <- progress_bar$new(total = nrow(sum_df),
                           format = "[:bar] :percent eta::eta",
                           clear = F)
    pb$tick(0)
  }

  sum_df$mean_birds[i] <- length(unique(df$Tx[df$month == sum_df$month[i]]))
  sum_df$total_birds_released[i] <- release_df$cum_released[release_df$month == sum_df$month[i]]
  sum_df$prop_birds_alive[i] <- sum_df$mean_birds[i] / sum_df$total_birds_released[i]
  sum_df$mean_monthly_fixes[i] <- nrow(df[df$month == sum_df$month[i],])

  if(sum_df$dist_from_pen_band[i] == 2000) {
    sub_df <- df %>%
      dplyr::filter(
        month == sum_df$month[i] &
          dist_from_pen >= sum_df$dist_from_pen_band[i]
      )
  } else {
    sub_df <- df %>%
      dplyr::filter(
        month == sum_df$month[i] &
          dist_from_pen >= sum_df$dist_from_pen_band[i] &
          dist_from_pen < sum_df$dist_from_pen_band[i+1]
      )
  }

  sum_df$mean_fixes[i] <- nrow(sub_df)
  sum_df$mean_prop[i] <- sum_df$mean_fixes[i] / sum_df$mean_monthly_fixes[i]
  sum_df$mean_prop_total_fixes[i] <- sum_df$mean_fixes[i] / sum_df$total_fixes[i]
  pb$tick()
}

saveRDS(sum_df, "shiny_app/summarised tracking data/GWCT_data.rds")


#ATLAS data ####

atlas_pen_cen <- st_read("data/ReleasePen/ReleasePen2.shp") %>%
  st_centroid() %>%
  st_coordinates()

df <- readRDS("data/ATLAS_df.rds") %>%
  mutate(
    Deploydatetime = ymd_hms(Deploydatetime),
    month = month.name[month(DateTime)], 
    dist_from_pen = sqrt(((X - atlas_pen_cen[1, 1])^2) + ((Y - atlas_pen_cen[1, 2])^2))
  ) %>%
  group_by(ID) %>%
  mutate(
    dist = sqrt(((X - lag(X))^2) + ((Y - lag(Y))^2))
  ) %>%
  ungroup()

sum_df <- data.frame(
  dist_from_pen_band = rep_len(seq(0, 2000, 250), 9*12),
  mean_birds = 0,
  mean_fixes = 0,
  mean_monthly_fixes = 0,
  mean_prop = 0,
  month = rep(month.name, each = 9),
  data_type = "real",
  no_sites = 1,
  sites = "ATLAS data",
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
  total_fixes = nrow(df),
  mean_prop_total_fixes = 0,
  min_prop_total_fixes = NA,
  max_prop_total_fixes = NA,
  sd_low_total_fixes = NA,
  sd_high_prop_total_fixes = NA,
  total_birds_released = 125,
  prop_birds_alive = 0
)

for(i in 1:nrow(sum_df)) {
  if(i == 1) {
    pb <- progress_bar$new(total = nrow(sum_df),
                           format = "[:bar] :percent eta::eta",
                           clear = F)
    pb$tick(0)
  }
  
  sum_df$mean_birds[i] <- length(unique(df$ID[df$month == sum_df$month[i]]))
  sum_df$prop_birds_alive[i] <- sum_df$mean_birds[i] / sum_df$total_birds_released[i]
  sum_df$mean_monthly_fixes[i] <- nrow(df[df$month == sum_df$month[i],])
  
  if(sum_df$dist_from_pen_band[i] == 2000) {
    sub_df <- df %>%
      dplyr::filter(
        month == sum_df$month[i] &
          dist_from_pen >= sum_df$dist_from_pen_band[i]
      )
  } else {
    sub_df <- df %>%
      dplyr::filter(
        month == sum_df$month[i] &
          dist_from_pen >= sum_df$dist_from_pen_band[i] &
          dist_from_pen < sum_df$dist_from_pen_band[i+1]
      )
  }
  
  sum_df$mean_fixes[i] <- nrow(sub_df)
  sum_df$mean_prop[i] <- sum_df$mean_fixes[i] / sum_df$mean_monthly_fixes[i]
  sum_df$mean_prop_total_fixes[i] <- sum_df$mean_fixes[i] / sum_df$total_fixes[i]
  pb$tick()
}

saveRDS(sum_df, "shiny_app/summarised tracking data/ATLAS_data.rds")
#APHA data ####
##3 sites ####
df <- readRDS("data/Data for Exeter - anonymised GPSV2/combined_current_tracks.rds") %>%
  mutate(
    ID = paste0(site, ID),
    month = month.name[month(DateTime)], 
    dist_from_pen = sqrt((X_coord^2) + (Y_coord^2)), 
    ReleaseDate = ymd(ReleaseDate)
  ) %>% 
  filter(
    !(site == "A" & ID == 7 & TimePoint %in% 2341:2343)
  ) %>%
  filter(
    !(site == "A" & ID == 9 & TimePoint %in% 2320:2327)
  )


sum_df <- data.frame(
  dist_from_pen_band = rep_len(seq(0, 2000, 250), 9*12),
  mean_birds = 0,
  mean_fixes = 0,
  mean_monthly_fixes = 0,
  mean_prop = 0,
  month = rep(month.name, each = 9),
  data_type = "real",
  no_sites = 3,
  sites = "APHA - sites A, B & D",
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
  total_fixes = nrow(df),
  mean_prop_total_fixes = 0,
  min_prop_total_fixes = NA,
  max_prop_total_fixes = NA,
  sd_low_total_fixes = NA,
  sd_high_prop_total_fixes = NA,
  total_birds_released = 30,
  prop_birds_alive = 0
)

for(i in 1:nrow(sum_df)) {
  if(i == 1) {
    pb <- progress_bar$new(total = nrow(sum_df),
                           format = "[:bar] :percent eta::eta",
                           clear = F)
    pb$tick(0)
  }
  
  sum_df$mean_birds[i] <- length(unique(df$ID[df$month == sum_df$month[i]]))
  sum_df$prop_birds_alive[i] <- sum_df$mean_birds[i] / sum_df$total_birds_released[i]
  sum_df$mean_monthly_fixes[i] <- nrow(df[df$month == sum_df$month[i],])
  
  if(sum_df$dist_from_pen_band[i] == 2000) {
    sub_df <- df %>%
      dplyr::filter(
        month == sum_df$month[i] &
          dist_from_pen >= sum_df$dist_from_pen_band[i]
      )
  } else {
    sub_df <- df %>%
      dplyr::filter(
        month == sum_df$month[i] &
          dist_from_pen >= sum_df$dist_from_pen_band[i] &
          dist_from_pen < sum_df$dist_from_pen_band[i+1]
      )
  }
  
  sum_df$mean_fixes[i] <- nrow(sub_df)
  sum_df$mean_prop[i] <- sum_df$mean_fixes[i] / sum_df$mean_monthly_fixes[i]
  sum_df$mean_prop_total_fixes[i] <- sum_df$mean_fixes[i] / sum_df$total_fixes[i]
  pb$tick()
}

saveRDS(sum_df, "shiny_app/summarised tracking data/3_sites.rds")

##all sites ####
df <- read_xlsx("data/all_ranging_data/Monthly Ranging Profile Summary - Complete V1.xlsx") %>%
  rename(dist_from_pen_band = distance_bins) %>%
  mutate(
    month = factor(
      month.name[as.integer(substr(Year_Month, 6, 8))], 
      levels = month.name[c(7:12, 1:6)]), 
    dist_from_pen_band = if_else(substr(dist_from_pen_band, 1, 4) %in% as.character(seq(2000, 9000, 250)), 
                            "2000", dist_from_pen_band)) %>%
  mutate(
    dist_from_pen_band = case_when(
      substr(dist_from_pen_band, 1, 1) == "0" ~ 0, 
      substr(dist_from_pen_band, 1, 2) == "25" ~ 250, 
      substr(dist_from_pen_band, 1, 1) == "5" ~ 500, 
      substr(dist_from_pen_band, 1, 1) == "7" ~ 750, 
      substr(dist_from_pen_band, 1, 2) == "10" ~ 1000, 
      substr(dist_from_pen_band, 1, 2) == "12" ~ 1250, 
      substr(dist_from_pen_band, 1, 2) == "15" ~ 1500, 
      substr(dist_from_pen_band, 1, 2) == "17" ~ 1750, 
      substr(dist_from_pen_band, 1, 2) == "20" ~ 2000
    ), 
    data_type = "new", 
    no_sites = 11
  ) %>%
  select(-c(Year_Month, bin_order)) %>%
  rbind(., data.frame(
    dist_from_pen_band = rep_len(seq(0, 2000, 250), 9 * 12),
    Number_of_Birds = 0, 
    Total_Number_of_Fixes = 0, 
    Total_Fixes_Overall = 0, 
    Proportional_Fixes = 0, 
    month = rep(month.name, each = 9), 
    data_type = "new", 
    no_sites = 11
  )) %>%
  distinct(dist_from_pen_band, month, .keep_all = T) %>%
  arrange(month, dist_from_pen_band) %>%
  rename(
    mean_birds = Number_of_Birds, 
    mean_fixes = Total_Number_of_Fixes, 
    mean_monthly_fixes = Total_Fixes_Overall, 
    mean_prop = Proportional_Fixes
  ) %>%
  mutate(
    mean_birds = NA, 
    sites = "All APHA sites", 
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
    total_fixes = sum(unique(.$mean_monthly_fixes)), 
    mean_prop_total_fixes = mean_fixes / total_fixes, 
    min_prop_total_fixes = NA,
    max_prop_total_fixes = NA,
    sd_low_total_fixes = NA,
    sd_high_prop_total_fixes = NA,
    total_birds_released = NA,
    prop_birds_alive = NA
  )

saveRDS(df, "shiny_app/summarised tracking data/all_APHA_sites.rds")
