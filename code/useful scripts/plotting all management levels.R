library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(ggnewscale)
library(ggspatial)

source("code/functions/UKCEH_functions.R")

site <- c("As", "Bo", "Du", "Ex", "Go", "Me", "Mo", "No", "Up")
full_site_name <- c("Aston Rowant - Oxfordshire", 
                    "Bovingdon Hall Woods - Essex", 
                    "Duncton to Bignor Escarpment - Hampshire", 
                    "Quantocks - Exmoor", 
                    "Gormire - North York moors", 
                    "Mendip wood - Somerset", 
                    "North Pennine Moors - County Durham", 
                    "North Pennine Dales Meadows - North Yorkshire", 
                    "Upper Fal Estuary - Cornwall")
dist <- 1000

#for(i in 1:length(site)) {
        pen <- st_read(paste0("outputs/script_4/PA sites/", site[i], "500_pen_shapefile.shp"))
        # plot(pen)

        hab <- rast(paste0("outputs/script_5/PA sites/", site[i], "500 cropped habitat raster.tif")) %>%
                crop(., st_buffer(pen, dist = dist))
        # plot(hab)

        mid_df <- data.frame(
                midx = xmin(hab) + ((xmax(hab) - xmin(hab))/2),
                midy = ymin(hab) + ((ymax(hab) - ymin(hab))/2)
        )


        PAs <- st_read("data/Protected Areas/All_UK_PAs.shp") %>%
                st_transform(crs = "EPSG:27700") %>%
                st_crop(., st_buffer(st_as_sf(mid_df, coords = c("midx", "midy"), crs = "EPSG:27700"), dist = dist))
        # plot(PBo)

        dog_buf <- st_difference(st_buffer(st_geometry(pen), dist = 200), pen)
        # plot(dog_buf)

        feed <- st_read(paste0("outputs/script_4/PA sites/", site[i], "500_feeders_shapefile.shp"))
        # plot(feed)

        mana_feed <- st_read(paste0("outputs/script_4/PA sites/", site[i], "500_managed_feeders_shapefile.shp"))
        # plot(mana_feed)

        hedge <- st_read(paste0("outputs/script_5/PA sites/", site[i], "500 cropped hedgerow shapefile.shp")) %>%
                st_crop(., st_buffer(pen, dist = dist))
        # plot(hedge)

        mana_hedge <- st_read(paste0("data/PA_site_hedgerow_management/", site[i], "500_managed_hedge_shapefile.shp")) %>%
                st_crop(., st_buffer(pen, dist = dist))
        # ggplot() + geom_sf(data = mana_hedge)

        cols <- UKCEH_colours(hab, short_list = T)

        p1 <- ggplot() + 
                geom_spatraster(data = as.factor(hab), alpha = 0.3) +
                scale_fill_manual(name = "Habitat type", values = cols$colour, labels = cols$habitat) + 
                new_scale_fill() + 
                geom_sf(data = hedge, aes(colour = "Hedgerows", fill = "Hedgerows")) + 
                geom_sf(data = PAs, aes(colour = "Protected area", fill = "Protected area"), linetype = "dashed", linewidth = 1) + 
                geom_sf(data = pen, aes(colour = "Release pen", fill = "Release pen")) + 
                scale_fill_manual(name = "POIs",
                                values = c("Protected area" = "transparent",
                                        "Release pen" = "white", 
                                        "Feeders" = "red", 
                                        "Hedgerows" = "black"))  +
                scale_colour_manual(name = "POIs",
                                values = c("Protected area" = "purple",
                                        "Release pen" = "black", 
                                        "Feeders" = "red", 
                                        "Hedgerows" = "black"))  +
                scale_y_continuous(name = "", 
                                breaks = NULL, limits = c(ymin(hab), ymax(hab))) +
                scale_x_continuous(name = "", 
                                breaks = NULL, limits = c(xmin(hab), xmax(hab))) + 
                theme_classic(base_size = 30) + 
                ggtitle(paste0(full_site_name[i], " management scenario 1"))
        # p1
        ggsave(p1, filename = paste0("outputs/", site[i], "_level_1_manage.png"), units = "px", height = 7680, width = 7680)

        p2 <- ggplot() + 
                geom_spatraster(data = as.factor(hab), alpha = 0.3) +
                scale_fill_manual(name = "Habitat type", values = cols$colour, labels = cols$habitat) + 
                new_scale_fill() + 
                geom_sf(data = hedge, aes(colour = "Hedgerows", fill = "Hedgerows")) + 
                geom_sf(data = PAs, aes(colour = "Protected area", fill = "Protected area"), linetype = "dashed", linewidth = 1) + 
                geom_sf(data = pen, aes(colour = "Release pen", fill = "Release pen")) + 
                geom_sf(data = feed, aes(colour = "Feeders", fill = "Feeders"), size = 4) + 
                scale_fill_manual(name = "POIs",
                                values = c("Protected area" = "transparent",
                                        "Release pen" = "white", 
                                        "Feeders" = "red", 
                                        "Hedgerows" = "black"))  +
                scale_colour_manual(name = "POIs",
                                values = c("Protected area" = "purple",
                                        "Release pen" = "black", 
                                        "Feeders" = "red", 
                                        "Hedgerows" = "black"))  +
                scale_y_continuous(name = "", 
                                breaks = NULL, limits = c(ymin(hab), ymax(hab))) +
                scale_x_continuous(name = "", 
                                breaks = NULL, limits = c(xmin(hab), xmax(hab))) + 
                theme_classic(base_size = 30) + 
                ggtitle(paste0(full_site_name[i], " management scenario 2"))
        # p2

        ggsave(p2, filename = paste0("outputs/", site[i], "_level_2_manage.png"), units = "px", height = 7680, width = 7680)

        p3 <- ggplot() + 
                geom_spatraster(data = as.factor(hab), alpha = 0.3) +
                scale_fill_manual(name = "Habitat type", values = cols$colour, labels = cols$habitat) + 
                new_scale_fill() + 
                geom_sf(data = hedge, aes(colour = "Hedgerows", fill = "Hedgerows")) + 
                geom_sf(data = PAs, aes(colour = "Protected area", fill = "Protected area"), linetype = "dashed", linewidth = 1) + 
                geom_sf(data = dog_buf, aes(colour = "Dog-in area", fill = "Dog-in area")) + 
                geom_sf(data = pen, aes(colour = "Release pen", fill = "Release pen")) + 
                geom_sf(data = feed, aes(colour = "Feeders", fill = "Feeders"), size = 4) + 
                scale_fill_manual(name = "POIs",
                                values = c("Protected area" = "transparent",
                                        "Release pen" = "white", 
                                        "Feeders" = "red", 
                                        "Hedgerows" = "black", 
                                        "Dog-in area" = "grey"))  +
                scale_colour_manual(name = "POIs",
                                values = c("Protected area" = "purple",
                                        "Release pen" = "black", 
                                        "Feeders" = "red", 
                                        "Hedgerows" = "black", 
                                        "Dog-in area" = "grey"))  +
                scale_y_continuous(name = "", 
                                breaks = NULL, limits = c(ymin(hab), ymax(hab))) +
                scale_x_continuous(name = "", 
                                breaks = NULL, limits = c(xmin(hab), xmax(hab))) + 
                theme_classic(base_size = 30) + 
                ggtitle(paste0(full_site_name[i], " management scenario 3"))

        # p3

        ggsave(p3, filename = paste0("outputs/", site[i], "_level_3_manage.png"), units = "px", height = 7680, width = 7680)

        p4 <- ggplot() + 
                geom_spatraster(data = as.factor(hab), alpha = 0.3) +
                scale_fill_manual(name = "Habitat type", values = cols$colour, labels = cols$habitat) + 
                new_scale_fill() + 
                geom_sf(data = hedge, aes(colour = "Hedgerows", fill = "Hedgerows")) + 
                geom_sf(data = PAs, aes(colour = "Protected area", fill = "Protected area"), linetype = "dashed", linewidth = 1) + 
                geom_sf(data = dog_buf, aes(colour = "Dog-in area", fill = "Dog-in area")) + 
                geom_sf(data = pen, aes(colour = "Release pen", fill = "Release pen")) + 
                geom_sf(data = mana_feed, aes(colour = "Feeders", fill = "Feeders"), size = 4) + 
                scale_fill_manual(name = "POIs",
                                values = c("Protected area" = "transparent",
                                        "Release pen" = "white", 
                                        "Feeders" = "red", 
                                        "Hedgerows" = "black", 
                                        "Dog-in area" = "grey"))  +
                scale_colour_manual(name = "POIs",
                                values = c("Protected area" = "purple",
                                        "Release pen" = "black", 
                                        "Feeders" = "red", 
                                        "Hedgerows" = "black", 
                                        "Dog-in area" = "grey"))  +
                scale_y_continuous(name = "", 
                                breaks = NULL, limits = c(ymin(hab), ymax(hab))) +
                scale_x_continuous(name = "", 
                                breaks = NULL, limits = c(xmin(hab), xmax(hab))) + 
                theme_classic(base_size = 30) + 
                ggtitle(paste0(full_site_name[i], " management scenario 4"))

        # p4

        ggsave(p4, filename = paste0("outputs/", site[i], "_level_4_manage.png"), units = "px", height = 7680, width = 7680)


        p5 <- ggplot() + 
                geom_spatraster(data = as.factor(hab), alpha = 0.3) +
                scale_fill_manual(name = "Habitat type", values = cols$colour, labels = cols$habitat) + 
                new_scale_fill() + 
                geom_sf(data = mana_hedge, aes(colour = "Hedgerows", fill = "Hedgerows")) + 
                geom_sf(data = PAs, aes(colour = "Protected area", fill = "Protected area"), linetype = "dashed", linewidth = 1) + 
                geom_sf(data = dog_buf, aes(colour = "Dog-in area", fill = "Dog-in area")) + 
                geom_sf(data = pen, aes(colour = "Release pen", fill = "Release pen")) + 
                geom_sf(data = mana_feed, aes(colour = "Feeders", fill = "Feeders"), size = 4) + 
                scale_fill_manual(name = "POIs",
                                values = c("Protected area" = "transparent",
                                        "Release pen" = "white", 
                                        "Feeders" = "red", 
                                        "Hedgerows" = "black", 
                                        "Dog-in area" = "grey"))  +
                scale_colour_manual(name = "POIs",
                                values = c("Protected area" = "purple",
                                        "Release pen" = "black", 
                                        "Feeders" = "red", 
                                        "Hedgerows" = "black", 
                                        "Dog-in area" = "grey"))  +
                scale_y_continuous(name = "", 
                                breaks = NULL, limits = c(ymin(hab), ymax(hab))) +
                scale_x_continuous(name = "", 
                                breaks = NULL, limits = c(xmin(hab), xmax(hab))) + 
                theme_classic(base_size = 30) + 
                annotation_scale() + 
                ggtitle(paste0(full_site_name[i], " management scenario 5"))

        # p5

        ggsave(p5, filename = paste0("outputs/", site[i], "_level_5_manage.png"), units = "px", height = 7680, width = 7680)

}