library(tidyverse)
library(sf)

ct <- read.table("data/Claire Turner PhD/AllFixes.csv", sep = ",", header = T) %>%
  mutate(DateTime = ymd_hms(DateTime)) %>%
  group_by(Tx) %>%
  mutate(difftime = as.numeric(difftime(DateTime, lag(DateTime), units = "mins"))) %>%
  ungroup() %>%
  mutate(dist_from_pen = sqrt(((x - pen_x)^2) + ((y - pen_y)^2)), 
         data = "CT")

pen_cen <- st_read("data/ReleasePen/ReleasePen2.shp") %>%
  st_centroid(st_geometry()) %>%
  st_coordinates()

at <- readRDS("../Pheasant sleep analysis/data/Pheas_filtered_with_tod.rds") %>%
  mutate(Deploydatetime = ymd_hms(Deploydatetime)) %>%
  group_by(ID) %>%
  mutate(difftime = as.numeric(difftime(DateTime, lag(DateTime), units = "mins"))) %>%
  ungroup() %>%
  mutate(dist_from_pen = sqrt((((X - pen_cen[1, 1])^2) + ((Y - pen_cen[1, 2])^2))), 
         data = "AT", 
         site = "NW")


length(unique(at$ID[year(at$Deploydatetime) == 2017]))
length(unique(at$ID[year(at$Deploydatetime) == 2018]))
median(at$difftime[year(at$Deploydatetime) == 2017], na.rm = T)
median(at$difftime[year(at$Deploydatetime) == 2018], na.rm = T)
hist(at$difftime)

sites <- readRDS("data/Data for Exeter - anonymised GPSV2/combined_current_tracks.rds") %>%
  mutate(dist_from_pen = sqrt(((X_coord)^2) + (Y_coord)^2), 
         data = "AP") %>%
  ungroup()


all <- rbind(at %>%
               select(dist_from_pen, DateTime, data, site), 
             ct %>%
               select(dist_from_pen, DateTime, data, site), 
             sites %>%
               select(dist_from_pen, DateTime, data, site)) %>%
  group_by(site) %>%
  mutate(first_day = date(DateTime)[1]) %>%
  ungroup()

length(all$dist_from_pen[all$dist_from_pen > 100 & date(all$DateTime) == all$first_day])
