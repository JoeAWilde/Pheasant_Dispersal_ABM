library(tidyverse)
library(RColorBrewer)

manage <- c("1_no_manage", "2_feed", "3_baseline", "4_extra_feed", "5_all_manage")

for(mm in manage){

  root <- paste0("outputs/script_7/PA sites/", mm, "/summarised data/")

  site_files <- paste0(root, list.files(root)) %>%
                  .[!grepl("tar.gz", .)]

  df <- lapply(site_files, readRDS) %>%
    do.call(rbind, .) %>%
    filter(month %in% month.name[c(1:2, 9:12)]) %>%
    mutate(
      distance_str = if_else(dist_from_pen_band == 2000, "2000+", paste0(dist_from_pen_band, "-",
                                                                        lead(dist_from_pen_band)))
    ) %>%
    mutate(
      distance_str = factor(distance_str, levels = c("0-250", "250-500", "500-750",
                                                    "750-1000", "1000-1250",
                                                    "1250-1500","1500-1750",
                                                    "1750-2000", "2000+")), 
      band_area = case_when(
        distance_str == "0-250" ~ pi*(250^2), 
        distance_str == "250-500" ~ (pi*(500^2)) - (pi*(250^2)), 
        distance_str == "500-750" ~ (pi*(750^2)) - (pi*(500^2)), 
        distance_str == "750-1000" ~ (pi*(1000^2)) - (pi*(750^2)), 
        distance_str == "1000-1250" ~ (pi*(1250^2)) - (pi*(1000^2)),
        distance_str == "1250-1500" ~ (pi*(1500^2)) - (pi*(1250^2)), 
        distance_str == "1500-1750" ~ (pi*(1750^2)) - (pi*(1500^2)),
        distance_str == "1750-2000" ~ (pi*(2000^2)) - (pi*(1750^2)),
        distance_str == "2000+" ~ (pi*(8846^2)) - (pi*(2000^2))
      ), 
      dist_from_PA = case_when(
        substr(site, 3, 99) == "0" ~ 0,
        substr(site, 3, 99) == "250" ~ 250, 
        substr(site, 3, 99) == "500" ~ 500, 
        substr(site, 3, 99) == "1000" ~ 1000, 
        substr(site, 3, 99) == "2000" ~ 2000
      )
    ) %>%
    distinct(site, month, birdhours_in_PA, .keep_all = T) %>%
    mutate(
      sd_low_PA_birdhours = birdhours_in_PA - sd_birdhours_in_PA, 
      sd_low_PA_birdhours = if_else(sd_low_PA_birdhours < 0, 0, sd_low_PA_birdhours), 
      sd_high_PA_birdhours = birdhours_in_PA + sd_birdhours_in_PA, 
      sd_high_PA_birdhours = if_else(sd_high_PA_birdhours < 0, 0, sd_high_PA_birdhours),
      month = factor(month, levels = month.name[c(9:12, 1:2)])
    ) %>%
    arrange(month, dist_from_PA)

  grouped_df <- df %>%
    group_by(month, dist_from_PA) %>%
    mutate(
      birdhours_in_PA = mean(birdhours_in_PA), 
      sd_low_PA_birdhours = mean(sd_low_PA_birdhours), 
      sd_high_PA_birdhours = mean(sd_high_PA_birdhours), 
      low_birdhours_in_PA = mean(low_birdhours_in_PA), 
      high_birdhours_in_PA = mean(high_birdhours_in_PA)
    ) %>%
    ungroup() %>%
    distinct(month, dist_from_PA, .keep_all = T) %>%
    group_by(dist_from_PA, month) %>%
    summarise(birdhours_in_PA = mean(birdhours_in_PA), 
          sd_low_PA_birdhours = mean(sd_low_PA_birdhours),
          sd_high_PA_birdhours = mean(sd_high_PA_birdhours),
          low_birdhours_in_PA = min(low_birdhours_in_PA), 
          high_birdhours_in_PA = max(high_birdhours_in_PA)) %>%
    ungroup() %>%
    mutate(management = mm)

    saveRDS(grouped_df, paste0("outputs/script_8/PA sites/", mm, "_summarised data.rds"))

  p1 <- ggplot(data = grouped_df) +
    geom_line(aes(x = dist_from_PA, y = birdhours_in_PA), linetype = "dashed") + 
    geom_point(aes(x = dist_from_PA, y = birdhours_in_PA, shape = "Mean"), size = 3) +
    geom_errorbar(aes(x = dist_from_PA, ymin= sd_low_PA_birdhours, ymax=sd_high_PA_birdhours, linetype = "±1 SD")) +
    geom_point(aes(x = dist_from_PA, y = low_birdhours_in_PA, shape = "Min"), size = 1.5) +
    geom_point(aes(x = dist_from_PA, y = high_birdhours_in_PA, shape = "Max"), size = 1.5) +
    scale_shape_manual(name = NULL, values = c("Mean" = 19, "Min" = 6, "Max" = 2)) + 
    scale_linetype_manual(name = NULL, values = c("±1 SD" = "solid")) + 
    scale_y_continuous(name = "Birdhours spent in protected area", sec.axis = sec_axis(transform = ~./74.4, name="Percentage of total released population fixes")) +
    scale_x_continuous(name = "Release pen distance from protected area boundary (m)",
                      breaks = c(0, 250, 500, 1000, 2000)) +
    scale_fill_manual(name = "Data type", values = brewer.pal(3, "Dark2")) +
    theme_classic(base_size = 30) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    facet_wrap(vars(month))
  p1

  ggsave(p1, filename = paste0("outputs/script_8/PA sites/", mm, "_all_baseline_pa_birdhours.png"), 
        height = 4320, width = 7890, units = "px")

  grouped_df %>%
    rename(`Distance from PA boundary (m)` = 1, 
          Month = 2, 
          `Mean birdhours spent in PA` = 3, 
          `Mean - SD birdhours spent in PA` = 4, 
          `Mean + SD birdhours spent in PA` = 5, 
          `Minumum birdhours spent in PA` = 6, 
          `Maximum birdhours spent in PA` = 7) %>%
    mutate(`Mean birdhours spent in PA` = round(`Mean birdhours spent in PA`, 2), 
          `Mean - SD birdhours spent in PA` = round(`Mean - SD birdhours spent in PA`, 2), 
          `Mean + SD birdhours spent in PA` = round(`Mean + SD birdhours spent in PA`, 2), 
          `Minumum birdhours spent in PA` = round(`Minumum birdhours spent in PA`, 2), 
          `Maximum birdhours spent in PA` = round(`Maximum birdhours spent in PA`, 2)) %>%
    knitr::kable(., row.names = FALSE) %>%
    kableExtra::kable_classic(full_width = T, , html_font = "Cambria") %>%
    cat(., file = paste0("outputs/script_8/PA sites/", mm, "_birdhours_baseline_table.html"))

}
