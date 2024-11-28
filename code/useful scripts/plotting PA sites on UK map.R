library(tidyverse)
library(sf)
library(readxl)

UK <- st_read("data/CoastOutline/UK_Coastline.shp") %>%
  st_transform(., CRS = "EPSG:27700")

df <- read_xlsx("all_PA_sites.xlsx") %>%
  filter(Approx_dist_from_PA == 0) %>%
  mutate(Location = c("Quantocks - Exmoor", 
                      "Aston Rowant - Oxfordshire", 
                      "Mendip wood - Somerset", 
                      "Gormire - North York Moors", 
                      "North Pennine Moors - County Durham", 
                      "Upper Fal Estuary - Cornwall",
                      "Bovingdon Hall Woods - Essex", 
                      "North Pennine Dales Meadows - North Yorkshire", 
                      "Duncton to Bignor Escarpment - Hampshire"))

df_sf <- df %>%
  st_as_sf(., coords = c("Easting", "Northing"), crs = "EPSG:27700")

# Adjust coordinates for overlapping labels
df_sf_coords <- df %>%
  mutate(
    Northing = case_when(
      Location == "North Pennine Dales Meadows - North Yorkshire" ~  Northing + 10000, 
      Location == "Gormire - North York Moors" ~ Northing - 13000, 
      Location == "Quantocks - Exmoor" ~ Northing + 10000, 
      Location == "Mendip wood - Somerset" ~ Northing - 13000,
      TRUE ~ Northing
      )
  ) %>%
  st_as_sf(., coords = c("Easting", "Northing"), crs = "EPSG:27700")
  
# Plot
p1 <- ggplot() + 
  geom_sf(data = UK, fill = "transparent", colour = "lightgrey", linewidth = 3) + 
  geom_sf(data = df_sf, size = 6, colour = "navy") +
  geom_sf_text(data = df_sf_coords, aes(label = Location), 
               hjust = -0.03, vjust = 0, size = 14) + 
  coord_sf(clip = "off") +
  xlab("") + 
  ylab("") + 
  theme_minimal(base_size = 45) + 
  theme(plot.margin = margin(5, 20, 5, 5))
p1

ggsave(p1, filename = "outputs/PA sites/all_PA_sites_plot.png", 
       units = "px", height = 7980, width = 12980)
