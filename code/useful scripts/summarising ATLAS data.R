library(tidyverse)
library(sf)

atlas_pen_cen <- st_read("data/ReleasePen/ReleasePen2.shp") %>%
  st_centroid() %>%
  st_coordinates()

rem_sites <- readRDS("data/ATLAS_df.rds") %>%
  mutate(
    month = month.name[month(DateTime)], 
    dist = sqrt(((X - atlas_pen_cen[1, 1])^2) + ((Y - atlas_pen_cen[1, 2])^2))
  ) %>%
  group_by(ID) %>%
  filter(
    minute(DateTime) == minute(DateTime)[1]
  ) %>%
  ungroup()

df <- data.frame(
  site = "ATLAS", 
  month = rep(month.name, each = 9),
  distance_bin = rep_len(seq(0, 2000, 250), length.out = 9*12), 
  count = 0,
  number_fixes = 0, 
  number_birds = 0
)

for(i in 1:nrow(df)) {
  m <- df$month[i]
  d <- df$distance_bin[i]
  
  if(d == 2000) {
    df$count[i] <- nrow(rem_sites[rem_sites$dist > d & 
                                    rem_sites$month == m, ])
  } else {
    df$count[i] <- nrow(rem_sites[rem_sites$dist >= d & 
                                    rem_sites$dist < (d+250) &
                                    rem_sites$month == m, ])
  }
  df$number_fixes[i] <- nrow(rem_sites[rem_sites$month == m, ])
  df$number_birds[i] <- length(unique(rem_sites$ID[rem_sites$month == m]))
}



saveRDS(df, "shiny_app/summarised tracking data/ATLAS data/ATLAS summarised data.rds")
