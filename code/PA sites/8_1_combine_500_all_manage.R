library(tidyverse)

root <- "outputs/script_8/PA sites/"

files <- paste0(root, list.files(root)) %>%
            .[grepl(".rds", .)]

df <- lapply(files, readRDS) %>%
        do.call(rbind, .) %>%
        filter(dist_from_PA == 500) %>%
        mutate(management_str = case_when(
            management == "1_no_manage" ~ "1", 
            management == "2_feed" ~ "2", 
            management == "3_baseline" ~ "3", 
            management == "4_extra_feed" ~ "4", 
            management == "5_all_manage" ~ "5"
        ))

p1 <- ggplot(data = df) +
    geom_line(aes(x = management_str, y = birdhours_in_PA, group = month), linetype = "dashed") + 
    geom_point(aes(x = management_str, y = birdhours_in_PA, shape = "Mean"), size = 3) +
    geom_errorbar(aes(x = management_str, ymin= sd_low_PA_birdhours, ymax=sd_high_PA_birdhours, linetype = "±1 SD")) +
    geom_point(aes(x = management_str, y = low_birdhours_in_PA, shape = "Min"), size = 1.5) +
    geom_point(aes(x = management_str, y = high_birdhours_in_PA, shape = "Max"), size = 1.5) +
    scale_shape_manual(name = NULL, values = c("Mean" = 19, "Min" = 6, "Max" = 2)) + 
    scale_linetype_manual(name = NULL, values = c("±1 SD" = "solid")) + 
    scale_y_continuous(name = "Birdhours spent in protected area", sec.axis = sec_axis(transform = ~./74.4, name="Percentage of total released population fixes")) +
    scale_x_discrete(name = "Management level") +
    scale_fill_manual(name = "Data type", values = brewer.pal(3, "Dark2")) +
    theme_classic(base_size = 30) +
    facet_wrap(vars(month))
  p1

  ggsave(p1, filename = paste0("outputs/script_8/PA sites/all_managements_500_pa_birdhours.png"), 
        height = 4320, width = 7890, units = "px")

cols <- RColorBrewer::brewer.pal(5, "Dark2")

p2 <- ggplot(data = df) +
    geom_line(aes(x = month, y = birdhours_in_PA, colour = management_str, group = management_str), linetype = "dashed") + 
    geom_point(aes(x = month, y = birdhours_in_PA, shape = "Mean", colour = management_str), size = 3) +
    geom_errorbar(aes(x = month, ymin= sd_low_PA_birdhours, ymax=sd_high_PA_birdhours, linetype = "±1 SD", colour = management_str)) +
    geom_point(aes(x = month, y = low_birdhours_in_PA, shape = "Min", colour = management_str), size = 1.5) +
    geom_point(aes(x = month, y = high_birdhours_in_PA, shape = "Max", colour = management_str), size = 1.5) +
    scale_shape_manual(name = NULL, values = c("Mean" = 19, "Min" = 6, "Max" = 2)) + 
    scale_linetype_manual(name = NULL, values = c("±1 SD" = "solid")) + 
    scale_y_continuous(name = "Birdhours spent in protected area", sec.axis = sec_axis(transform = ~./74.4, name="Percentage of total released population fixes")) +
    scale_x_discrete(name = "Month") +
    scale_colour_manual(name = "Management level", values = cols) +
    theme_classic(base_size = 30) + 
    theme(axis.text.x = element_text(angle = -45, hjust = 0)) + 
    facet_wrap(vars(paste0("Management level ", management_str))) + 
    guides(colour = "none")
p2

  ggsave(p2, filename = paste0("outputs/script_8/PA sites/all_managements_500_pa_birdhours_plot2.png"), 
        height = 4320, width = 7890, units = "px")

df %>%
    select(management_str, month, birdhours_in_PA, sd_low_PA_birdhours, sd_high_PA_birdhours,
            low_birdhours_in_PA, high_birdhours_in_PA, -management, -dist_from_PA) %>%
    rename( 
          `Management level` = 1,
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
    cat(., file = paste0("outputs/script_8/PA sites/all_management_birdhours_table.html"))
