library(tidyverse)

#real
gwct <- readRDS("shiny_app/summarised tracking data/GWCT_data.rds") %>%
  rename(sd_low_prop_total_fixes = sd_low_total_fixes)
atlas <- readRDS("shiny_app/summarised tracking data/ATLAS_data.rds")%>%
  rename(sd_low_prop_total_fixes = sd_low_total_fixes)
three_s <- readRDS("shiny_app/summarised tracking data/3_sites.rds")%>%
  rename(sd_low_prop_total_fixes = sd_low_total_fixes)
all_sites <- readRDS("shiny_app/summarised tracking data/all_APHA_sites.rds")%>%
  rename(sd_low_prop_total_fixes = sd_low_total_fixes)

all_data <-rbind(gwct, atlas, all_sites) %>%
  mutate(month = factor(month, levels = month.name[c(7:12, 1:6)])) %>%
  arrange(sites, month, dist_from_pen_band) %>%
  mutate(distance_str = if_else(dist_from_pen_band == 2000, "2000+", paste0(dist_from_pen_band, "-",
                                                                            lead(dist_from_pen_band))), 
         fixes_per_site = mean_fixes / no_sites) %>%
  mutate(distance_str = factor(distance_str, levels = c("0-250", "250-500", "500-750",
                                                        "750-1000", "1000-1250",
                                                        "1250-1500","1500-1750",
                                                        "1750-2000", "2000+"))) %>%
  filter(month %in% month.name[c(1:2, 8:12)]) %>%
  mutate(band_area = case_when(
    distance_str == "0-250" ~ (pi*(250^2))/10000, 
    distance_str == "250-500" ~ ((pi*(500^2)) - (pi*(250^2)))/10000, 
    distance_str == "500-750" ~ ((pi*(750^2)) - (pi*(500^2)))/10000, 
    distance_str == "750-1000" ~ ((pi*(1000^2)) - (pi*(750^2)))/10000, 
    distance_str == "1000-1250" ~ ((pi*(1250^2)) - (pi*(1000^2)))/10000,
    distance_str == "1250-1500" ~ ((pi*(1500^2)) - (pi*(1250^2)))/10000, 
    distance_str == "1500-1750" ~ ((pi*(1750^2)) - (pi*(1500^2)))/10000,
    distance_str == "1750-2000" ~ ((pi*(2000^2)) - (pi*(1750^2)))/10000,
    distance_str == "2000+" ~ ((pi*(8846^2)) - (pi*(2000^2)))/10000
  ))  %>%
  group_by(month, dist_from_pen_band) %>%
  mutate(
    mean_fixes = sum(mean_fixes),
    total_fixes = sum(total_fixes),
    mean_prop_total_fixes = mean_fixes / total_fixes
         ) %>%
  ungroup() %>%
  distinct(month, dist_from_pen_band, .keep_all = T) %>%
  mutate(
    mean_prop = mean_fixes / mean_monthly_fixes, 
    PECunitsUS = mean_prop_total_fixes / band_area, 
    PECunits = PECunitsUS / max(PECunitsUS)
  ) %>%
  select(-c(sites, data_type, no_sites)) %>%
  group_by(month) %>%
  mutate(monthly_PEC_sum = sum(PECunits)) %>%
  ungroup()

p1 <- ggplot(data = all_data) +
  geom_col(aes(x = distance_str, y = PECunits,
               group = sites)) +
  scale_y_continuous(name = "PEC units") +
  scale_x_discrete(name = "Distance from pen (m)") +
  theme_classic(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(vars(month))
p1

ggsave(p1, filename = "outputs/PEC_density.png", units = "px", 
       height = 4320, width = 7890)

p2 <- ggplot(data = all_data %>%
               distinct(month, .keep_all = T)) + 
  geom_line(aes(x = month, y = monthly_PEC_sum, group = 1), linewidth = 2) + 
  geom_point(aes(x = month, y = monthly_PEC_sum), size = 4) + 
  scale_x_discrete(name = "Month") + 
  scale_y_continuous(name = "Sum of PEC units") + 
  theme_classic(base_size = 30)
p2

ggsave(p2, filename = "outputs/monthly_PEC_sum.png", units = "px", 
       height = 4320, width = 7890)


p3 <- ggplot(data = all_data, aes(x = distance_str, y = PECunits,
                                  colour = month, group = month)) + 
  geom_line(linewidth = 2) + 
  geom_point(size = 5) + 
  scale_colour_manual(name = "Month", 
                      values = RColorBrewer::brewer.pal(7, "Spectral")) + 
  scale_x_discrete(name = "Distance from release pen (m)") + 
  scale_y_continuous(name = "PEC units") + 
  theme_classic(base_size = 40) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
p3

ggsave(p3, filename = "outputs/PEC_by_distband.png", units = "px", 
       height = 4320, width = 7890)
# p2 <- ggplot(data = all_sites_mort) +
#   geom_col(aes(x = distance_str, y = exp(PECunitsUS),
#                group = sites)) +
#   scale_y_continuous(name = "PEC units") +
#   scale_x_discrete(name = "Distance from pen (m)") +
#   theme_classic(base_size = 30) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   facet_wrap(vars(month))
# p2



all_track <- rbind(gwct, atlas, three_s, data_with_mort, all_sites) %>%
  group_by(dist_from_pen_band, month) %>%
  mutate(
    mean_birds = sum(mean_birds), 
    mean_fixes = sum(mean_fixes), 
    mean_monthly_fixes = sum(mean_monthly_fixes), 
    mean_prop = mean_fixes / mean_monthly_fixes, 
    no_sites = sum(no_sites), 
    sites = "All tracking data", 
    total_fixes = sum(total_fixes), 
    mean_prop_total_fixes = mean_fixes / total_fixes, 
    total_birds_released = sum(total_birds_released), 
    prop_birds_alive = mean_birds / total_birds_released
  ) %>%
  distinct(dist_from_pen_band, month, .keep_all = T)
saveRDS(all_track, "shiny_app/summarised tracking data/all_tracking_data.rds")


#sim
kde_25 <- readRDS("shiny_app/summarised simulation data/kde_woodland_25_percent_sim.rds") %>%
  mutate(
    sd_low_fixes = mean_fixes - sd_fixes, 
    sd_high_fixes = mean_fixes + sd_fixes, 
    sd_low_prop = mean_prop - sd_prop, 
    sd_high_prop = mean_prop + sd_prop, 
    min_prop_total_fixes = min_fixes / total_fixes, 
    max_prop_total_fixes = max_fixes / total_fixes, 
    sd_low_prop_total_fixes = sd_low_fixes / total_fixes, 
    sd_high_prop_total_fixes = sd_high_fixes / total_fixes
  )


kde_50 <- readRDS("shiny_app/summarised simulation data/woodland_50_KDE.rds") %>%
  mutate(
    sd_low_fixes = mean_fixes - sd_fixes, 
    sd_high_fixes = mean_fixes + sd_fixes, 
    sd_low_prop = mean_prop - sd_prop, 
    sd_high_prop = mean_prop + sd_prop, 
    min_prop_total_fixes = min_fixes / total_fixes, 
    max_prop_total_fixes = max_fixes / total_fixes, 
    sd_low_prop_total_fixes = sd_low_fixes / total_fixes, 
    sd_high_prop_total_fixes = sd_high_fixes / total_fixes
  )


kde_75 <- readRDS("shiny_app/summarised simulation data/woodland_75_KDE.rds")  %>%
  mutate(
    sd_low_fixes = mean_fixes - sd_fixes, 
    sd_high_fixes = mean_fixes + sd_fixes, 
    sd_low_prop = mean_prop - sd_prop, 
    sd_high_prop = mean_prop + sd_prop, 
    min_prop_total_fixes = min_fixes / total_fixes, 
    max_prop_total_fixes = max_fixes / total_fixes, 
    sd_low_prop_total_fixes = sd_low_fixes / total_fixes, 
    sd_high_prop_total_fixes = sd_high_fixes / total_fixes
  ) %>%
  mutate(sites = "Woodland in 75% KDE at night")

inside_feeders <- readRDS("shiny_app/summarised simulation data/inside_feeding.rds")  %>%
  mutate(
    sd_low_fixes = mean_fixes - sd_fixes, 
    sd_high_fixes = mean_fixes + sd_fixes, 
    sd_low_prop = mean_prop - sd_prop, 
    sd_high_prop = mean_prop + sd_prop, 
    min_prop_total_fixes = min_fixes / total_fixes, 
    max_prop_total_fixes = max_fixes / total_fixes, 
    sd_low_prop_total_fixes = sd_low_fixes / total_fixes, 
    sd_high_prop_total_fixes = sd_high_fixes / total_fixes
  ) 


all_data <- rbind(gwct, atlas, three_s, all_sites, all_track, kde_25, kde_50, kde_75, inside_feeders) %>%
  filter(month %in% month.name[c(7:12, 1:2)])

saveRDS(all_data, "shiny_app/app/simulation_and_all_tracking_summarised_data.rds")
