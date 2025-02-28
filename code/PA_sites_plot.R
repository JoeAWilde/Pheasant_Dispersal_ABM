library(tidyverse)
library(sf)

df <- readxl::read_xlsx("all_PA_sites.xlsx") %>%
        filter(Approx_dist_from_PA == 0) %>%
        mutate(full_name = case_when(
            Location == Location[1] ~ "Quantocks - Exmoor", 
            Location == Location[4] ~ "Gormire - North York Moors", 
            Location == Location[5] ~ "North Pennine Moors - County Durham",
            TRUE ~ Location
        ))

uk <- st_read("Data/CoastOutline/UK_Coatline.shp") %>%
        st_transform(., crs = 27700)

p1 <- ggplot() + 
    geom_sf(data = uk, fill = "transparent", linewidth = 1, colour = "grey") + 
    geom_point(data = df, aes(x = Easting, y = Northing), 
                size = 5) + 
    ylim(35000, 545000) + 
    xlim(125000, 1000000) + 
    xlab("") + 
    ylab("") + 
    theme_bw(base_size = 30)
p1
ggsave(p1, filename = "../../report_writeup/blank_PA_site_map.png", 
        units = "px", height = 4320, width = 7890)
