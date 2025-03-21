library(tidyverse)

#real data ####
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

#sim data ####
sim_df <- readRDS("outputs/script_6/PA sites/3_baseline/summarised_data_from_back_up/simulation_and_all_tracking_summarised_data.rds") %>%
    filter(data_type == "sim" & sites == "Return to 50% KDE woodland at night") %>% 
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


tab_df2 <- sim_df %>%
            select(month, distance_str, PEC) %>%
            rename(Month = month, 
                    `Distance band (m)` = distance_str, 
                    `PEC units` = PEC) %>%
            arrange(Month)

p2 <- ggplot(data = tab_df2, aes(x = `Distance band (m)`, y = `PEC units`)) + 
    geom_col() + 
    theme_classic(base_size = 30) + 
    facet_wrap(vars(Month)) + 
    theme(axis.text.x = element_text(angle = 45,vjust = 0.6))
p2

ggsave(p2, filename = "../../report_writeup/sim_data_PEC_units_plot.png", 
        units = "px", height = 4320, width = 7890)
knitr::kable(tab_df2, row.names = FALSE) %>%
            kableExtra::kable_classic(full_width = T, , html_font = "Cambria") %>%
            cat(., file = "../../report_writeup/sim_data_PEC_units_table.html")


# use all PA sites data available

df <- readRDS("shiny_app/summarised simulation data/inside_feeding.rds") %>%
        rename(
            prop_fix = mean_prop, 
            prop_alive = prop_birds_alive
        ) %>%
        mutate(
            prop_alive = mean_birds / total_birds_released, 
            sd_low_prop_alive = (mean_birds - sd_birds) / total_birds_released,
            sd_high_prop_alive = (mean_birds + sd_birds) / total_birds_released, 
            prop_fix_scaled = prop_fix * prop_alive, 
            sd_low_prop_fix_scaled = (prop_fix - sd_prop) * sd_low_prop_alive, 
            sd_high_prop_fix_scaled = (prop_fix + sd_prop) * sd_high_prop_alive, 
            distance_str = if_else(dist_from_pen_band == 2000, "2000+", 
                                                           paste0(dist_from_pen_band, "-", lead(dist_from_pen_band)))
        ) %>%
        mutate(
            distance_str = factor(distance_str, levels = c("0-250", "250-500", "500-750",
                                                        "750-1000", "1000-1250",
                                                        "1250-1500","1500-1750",
                                                        "1750-2000", "2000+"))
        ) %>%
        filter(
            month %in% month.name[c(1:2, 8:12)]
        ) %>%
        mutate(
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
            unst_PEC = prop_fix_scaled / band_area, 
            sd_low_unst_PEC = sd_low_prop_fix_scaled / band_area, 
            sd_high_unst_PEC = sd_high_prop_fix_scaled / band_area, 
            PEC = unst_PEC / max(unst_PEC), 
            sd_low_PEC = sd_low_unst_PEC / max(sd_low_unst_PEC), 
            sd_high_PEC = sd_high_unst_PEC / max(sd_high_unst_PEC), 
        ) %>%
    mutate(
        month = factor(month, levels = month.name[c(7:12, 1:2)])
    )

tab_df3 <- df %>%
            select(month, distance_str, PEC, sd_low_PEC, sd_high_PEC) %>%
            rename(Month = month, 
                    `Distance band (m)` = distance_str, 
                    `PEC units` = PEC) %>%
            arrange(Month)

p3 <- ggplot(data = tab_df3) + 
    geom_col(aes(x = `Distance band (m)`, y = `PEC units`)) + 
    geom_errorbar(aes(x = `Distance band (m)`, ymin = sd_low_PEC, ymax = sd_high_PEC)) + 
    theme_classic(base_size = 30) + 
    facet_wrap(vars(Month)) + 
    theme(axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1.1))
p3

ggsave(p3, filename = "../../report_writeup/sim_data_PEC_units_plot2.png", 
        units = "px", height = 4320, width = 7890)


#combine tracking and sim data
ts_df <- rbind(tab_df %>% mutate(sd_low_PEC = NA, sd_high_PEC = NA, type = "Tracking data"), 
                tab_df3 %>% mutate(type = "Simulation data"))

p4 <- ggplot(data = ts_df) + 
    geom_col(aes(x = `Distance band (m)`, y = `PEC units`, fill = type), position = "dodge") + 
    geom_errorbar(aes(x = `Distance band (m)`, ymin = sd_low_PEC, ymax = sd_high_PEC, group = type), position = "dodge") + 
    scale_fill_manual(name = "Data type", values = c("orange2", "navy")) + 
    theme_classic(base_size = 30) + 
    facet_wrap(vars(Month)) + 
    theme(axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1.1))
p4

ggsave(p4, filename = "../../report_writeup/sim_data_PEC_units_comparison_plot.png", 
        units = "px", height = 4320, width = 7890)

wide_df <- pivot_wider(ts_df, names_from = "type", values_from = c("PEC units", "sd_low_PEC", "sd_high_PEC")) %>%
                rename(
                    `Tracking data PEC units` = `PEC units_Tracking data`, 
                    `Mean simulation data PEC units` = `PEC units_Simulation data`, 
                    `Mean - 1 SD simulation data PEC units` = `sd_low_PEC_Simulation data`, 
                    `Mean + 1 SD simulation data PEC units` = `sd_high_PEC_Simulation data`
                ) %>%
                select(
                    Month, `Distance band (m)`, `Tracking data PEC units`, `Mean simulation data PEC units`,
                    `Mean - 1 SD simulation data PEC units`, `Mean + 1 SD simulation data PEC units`
                ) %>%
                mutate(
                    `Mean - 1 SD simulation data PEC units` = if_else(`Mean - 1 SD simulation data PEC units` < 0, 0, `Mean - 1 SD simulation data PEC units`)
                )

knitr::kable(wide_df, row.names = FALSE) %>%
            kableExtra::kable_classic(full_width = T, , html_font = "Cambria") %>%
            cat(., file = "../../report_writeup/comparison_sim_track_PEC_table.html")
