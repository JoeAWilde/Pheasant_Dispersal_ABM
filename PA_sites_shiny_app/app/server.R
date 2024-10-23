server <- function(input, output) {
  require(tidyverse)
  require(RColorBrewer)
  library(bslib)
  
  df <- readRDS("PA_sites_shiny_app/app/all_PA_site_data.rds") %>%
    mutate(month = factor(month, levels = month.name[c(7:12, 1:6)])) %>%
    arrange(sites, month, dist_from_pen_band) %>%
    mutate(distance_str = if_else(dist_from_pen_band == 2000, "2000+", paste0(dist_from_pen_band, "-",
                                                                              lead(dist_from_pen_band))), 
           fixes_per_site = mean_fixes / no_sites) %>%
    mutate(distance_str = factor(distance_str, levels = c("0-250", "250-500", "500-750",
                                                          "750-1000", "1000-1250",
                                                          "1250-1500","1500-1750",
                                                          "1750-2000", "2000+"))) %>%
    filter(month %in% month.name[c(1:2, 7:12)]) %>%
    group_by(sites, month) %>%
    mutate(band_area = case_when(
      distance_str == "0-250" ~ pi*(250^2), 
      distance_str == "250-500" ~ (pi*(500^2)) - (pi*(250^2)), 
      distance_str == "500-750" ~ (pi*(750^2)) - (pi*(500^2)), 
      distance_str == "750-1000" ~ (pi*(1000^2)) - (pi*(750^2)), 
      distance_str == "1000-1250" ~ (pi*(1250^2)) - (pi*(1000^2)),
      distance_str == "1250-1500" ~ (pi*(1500^2)) - (pi*(1250^2)), 
      distance_str == "1500-1750" ~ (pi*(1750^2)) - (pi*(1500^2)),
      distance_str == "1750-2000" ~ (pi*(2000^2)) - (pi*(1750^2)),
      distance_str == "2000+" ~ (pi*(8846^2)) - (pi*(2000^2))
    )) %>%
    ungroup() %>%
    group_by(sites, month) %>%
    mutate(
      dist_from_PA = case_when(
        sites == "Exmoor 0m away from PA boundary" ~ 0,
        sites == "Exmoor 250m away from PA boundary" ~ 250, 
        sites == "Exmoor 500m away from PA boundary" ~ 500, 
        sites == "Exmoor 1000m away from PA boundary" ~ 1000, 
        sites == "Exmoor 2000m away from PA boundary" ~ 2000
      ), 
      mean_birdhours_PA = mean(mean_birdhours_PA),
      sd_birdhours_PA = mean(sd_birdhours_PA),
      min_birdhours_PA = mean(min_birdhours_PA), 
      max_birdhours_PA = mean(max_birdhours_PA),
      sd_low_PA_birdhours = if_else(mean_birdhours_PA - sd_birdhours_PA < 0, 0, mean_birdhours_PA - sd_birdhours_PA),
      sd_high_PA_birdhours = mean_birdhours_PA + sd_birdhours_PA
    ) %>%
    distinct(mean_birdhours_PA, .keep_all = T)
  
  
  output$PA_birdhours <- renderPlot({
    p1 <- ggplot(data = df) +
      geom_line(aes(x = dist_from_PA, y = mean_birdhours_PA)) + 
      geom_point(aes(x = dist_from_PA, y = mean_birdhours_PA)) +
      geom_errorbar(aes(x = dist_from_PA, ymin=sd_low_PA_birdhours, ymax=sd_high_PA_birdhours)) +
      geom_point(aes(x = dist_from_PA, y = min_birdhours_PA), shape = 1) +
      geom_point(aes(x = dist_from_PA, y = max_birdhours_PA), shape = 1) +
      scale_y_continuous(name = "Birdhours spent in protected area") +
      scale_x_continuous(name = "Distance from protected area boundary (m)",
                         breaks = c(0, 250, 500, 1000, 2000)) +
      scale_fill_manual(name = "Data type", values = brewer.pal(numer_of_sites, "Dark2")) +
      theme_classic(base_size = 20) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      facet_wrap(vars(month))
    p1
  }, height = 689, width = 1550)
  
}
