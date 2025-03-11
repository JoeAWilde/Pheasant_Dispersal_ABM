library(sf)
library(terra)
library(tidyverse)

# Create sf object with points
x <- cos(seq(1, 100, 0.1))
y <- sin(seq(1, 100, 0.1))

x_signs <- c(1, 1, -1, -1)
y_signs <- c(1, -1, 1, -1)

x_signs_str <- c("+", "+", "-", "-")
y_signs_str <- c("+", "-", "+", "-")

for(i in 1:length(x_signs)) {
    if(x_signs[i] == 1) { 
        x <- abs(x)
    } else {
        x <- abs(x) * -1
    }

    if(y_signs[i] == 1) {
        y <- abs(y)
    } else {
        y <- abs(y) * -1
    }

    df <- data.frame(x, y) %>%
            mutate(x = x * 1000, 
                y = y * 1000)

    st <- st_as_sf(df, coords = c("x", "y"), crs = 27700)
    #plot(st)


    # Convert points to a single line
    line <- st %>%
        summarise(geometry = st_union(geometry)) %>%  # Merge points
        st_cast("LINESTRING") %>%  # Convert to a linestring
        st_buffer(dist = 0.1)  # Make the line 1000 units thick

    st_write(line, paste0("data/PA_site_hedgerow_management/", 
                                                            x_signs_str[i], 
                                                            y_signs_str[i], 
                                                            "_managed_hedge_shapefile.shp"))

    #plot(line, col = "blue")  # Check the buffered line

    # ----------------------------
    # Define a common raster grid
    # ----------------------------
    res_value <- 25  # Resolution of the raster (10 units per pixel)

    raster_template <- rast(
    nrows = (20000 / res_value),  # Ensure it covers the full 20000 range
    ncols = (20000 / res_value),
    xmin = -10000, xmax = 10000,
    ymin = -10000, ymax = 10000,
    crs = "EPSG:27700"
    )
    values(raster_template) <- NA  # Initialize raster with NA

    # ----------------------------
    # Rasterize the buffered line
    # ----------------------------
    r_line <- rasterize(line, raster_template, field = 1, background = NA, touches = TRUE)
    #plot(r_line)

    writeRaster(r_line, paste0("data/PA_site_hedgerow_management/", 
                                                                x_signs_str[i], 
                                                                y_signs_str[i], 
                                                                "_hedge_raster.tif"), 
                overwrite = T)
}


site_dir_df <- data.frame(
        site = c("As", "Bo", "Du", "Ex", "Go", "Me", "Mo", "No", "Up"), 
        dir = c("++", "-+", "++", "--", "-+", "++", "--", "++", "--")
)

for(i in 1:nrow(site_dir_df)) {
    for(d in c(0, 250, 500, 1000, 2000)) {
        raw_hedges_rast <- rast(paste0("outputs/script_5/PA sites/", site_dir_df$site[i], d, 
                                 " cropped hedges_edges distance raster.tif"))
        mid_x <- xmin(raw_hedges_rast) + ((xmax(raw_hedges_rast) - xmin(raw_hedges_rast)) / 2)
        mid_y <- ymin(raw_hedges_rast) + ((ymax(raw_hedges_rast) - ymin(raw_hedges_rast)) / 2)

        raw_hedge_st <- st_read(paste0("outputs/script_5/PA sites/", site_dir_df$site[i], d, 
                                 " cropped hedgerow shapefile.shp"))

        managed_hedge_st <- st_read(paste0("data/PA_site_hedgerow_management/", 
                                          site_dir_df$dir[i], 
                                          "_managed_hedge_shapefile.shp"))

        line_bbox <- st_bbox(line)  # Get bounding box
        line_mid_x <- (line_bbox$xmin + line_bbox$xmax) / 2
        line_mid_y <- (line_bbox$ymin + line_bbox$ymax) / 2

        x_shift <- mid_x - line_mid_x
        y_shift <- mid_y - line_mid_y

        line_aligned <- st_geometry(line) + c(x_shift, y_shift)
        line_aligned <- st_set_geometry(line, line_aligned)
        line_aligned <- st_set_crs(line_aligned, 27700)

        all_hedges_st <- st_union(line_aligned, raw_hedge_st)

        ggplot() + 
            geom_sf(data = all_hedges_st)

        
    }
}