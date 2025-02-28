library(tidyverse)
library(terra)
library(tidyterra)
library(ggspatial)
source("code/functions/UKCEH_functions.R")

sites <- c("As", "Bo", "Du", "Ex", "Go", "Me", "Mo", "No", "Up")

for(ss in sites){
    df <- readxl::read_xlsx("all_PA_sites.xlsx") %>%
            filter(grepl(ss, substr(Location, 1, 2)))

    df_st <- st_as_sf(df, coords = c("Easting", "Northing"), crs = 27700)

    crop_buff <- st_buffer(df_st, dist = 500)


    hab <- rast("data/UKCEH-2018-25m_AllUK/gb2018lcm25m.tif") %>%
            crop(., crop_buff) %>%
            .[[1]]

    cols <- UKCEH_colours(hab, short_list = F)
    PAs <- st_read("data/Protected Areas/All_UK_PAs.shp") %>%
            st_crop(., crop_buff)

    p1 <- ggplot() + 
            geom_spatraster(data = as.factor(hab), alpha = 0.5) + 
            geom_sf(data = PAs, fill = "transparent", aes(linetype = "Protected Area"), linewidth = 2) + 
            geom_sf(data = df_st, size = 4, aes(colour = "Release Site")) + 
            scale_fill_manual(name = "Habitat", values = cols$colour, 
                                labels = cols$habitat) + 
            scale_colour_manual(name = "", values = c("Release Site" = "black")) + 
            scale_linetype_manual(name = "", values = c(`Protected Area` = "dashed")) + 
            annotation_scale(location = "bl", plot_unit = "m", text_cex = 1.5) +
            annotation_north_arrow(location = "tr", which_north = "true") + 
            ggtitle(paste0("Example site: ", df$Location[1])) + 
            theme_bw(base_size = 30) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                    plot.title = element_text(size = 15, face = "bold"))
    

    ggsave(p1, filename = paste0("../../report_writeup/", ss, "_site_map.png"), units = "px", 
            height = 4320, width = 4320)
}
