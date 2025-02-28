library(tidyverse)

df <- rbind(
    readRDS("shiny_app/summarised tracking data/3_sites.rds"), 
    readRDS("shiny_app/summarised tracking data/ATLAS_data.rds"), 
    readRDS("shiny_app/summarised tracking data/GWCT_data.rds")
)

summ_df <- df %>%
                group_by(month, dist_from_pen_band) %>%
                mutate(
                    prop_fix = sum(mean_fixes) / sum(mean_monthly_fixes), 
                    prop_fix = if_else(is.na(prop_fix), 0, prop_fix), 
                    prop_alive = sum(mean_birds) / sum(total_birds_released), 
                    prop_fix_scaled = prop_fix * prop_alive
                ) %>%
                ungroup() %>%
                distinct(month, dist_from_pen_band, prop_fix_scaled) %>%
                mutate(distance_str = if_else(dist_from_pen_band == 2000, "2000+", paste0(dist_from_pen_band, "-",
                                                                              lead(dist_from_pen_band)))) %>%
                mutate(distance_str = factor(distance_str, levels = c("0-250", "250-500", "500-750",
                                                                    "750-1000", "1000-1250",
                                                                    "1250-1500","1500-1750",
                                                                    "1750-2000", "2000+"))) %>%
                filter(month %in% month.name[c(1:2, 8:12)]) %>%
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
                ), 
                unst_PEC = prop_fix_scaled / band_area, 
                PEC = unst_PEC / max(unst_PEC)) %>%
                mutate(month = factor(month, levels = month.name[c(7:12, 1:2)]))

tab_df <- summ_df %>%
            select(month, distance_str, PEC) %>%
            rename(Month = month, 
                    `Distance band (m)` = distance_str, 
                    `PEC units` = PEC) %>%
            arrange(Month)

p1 <- ggplot(data = tab_df, aes(x = `Distance band (m)`, y = `PEC units`)) + 
    geom_col() + 
    theme_classic(base_size = 30) + 
    facet_wrap(vars(Month)) + 
    theme(axis.text.x = element_text(angle = 45,vjust = 0.6))
p1

ggsave(p1, filename = "../../report_writeup/PEC_units_plot.png", 
        units = "px", height = 4320, width = 7890)
knitr::kable(tab_df, row.names = FALSE) %>%
            kableExtra::kable_classic(full_width = T, , html_font = "Cambria") %>%
            cat(., file = "../../report_writeup/PEC_units_table.html")
