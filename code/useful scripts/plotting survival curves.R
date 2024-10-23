library(tidyverse)

#real
gwct <- readRDS("shiny_app/summarised tracking data/GWCT_data.rds") %>%
  rename(sd_low_prop_total_fixes = sd_low_total_fixes)
atlas <- readRDS("shiny_app/summarised tracking data/ATLAS_data.rds")%>%
  rename(sd_low_prop_total_fixes = sd_low_total_fixes)
three_s <- readRDS("shiny_app/summarised tracking data/3_sites.rds")%>%
  rename(sd_low_prop_total_fixes = sd_low_total_fixes)
# all_sites <- readRDS("shiny_app/summarised tracking data/all_APHA_sites.rds")%>%
#   rename(sd_low_prop_total_fixes = sd_low_total_fixes)

all <- rbind(atlas, gwct, three_s) %>%
  distinct(sites, month, prop_birds_alive, .keep_all = T) %>%
  mutate(
    month = factor(month, levels = month.name[c(7:12, 1:6)])
  ) %>%
  filter(month %in% month.name[c(1:2, 7:12)]) %>%
  select(mean_birds, month, sites, total_birds_released, prop_birds_alive) %>%
  rename(no_birds_alive = mean_birds) %>%
  arrange(month)

saveRDS(all, "mortality_data_per_dataset.rds")

p1 <- ggplot(data = all, aes(x = month, y = prop_birds_alive, colour = sites, group = sites)) + 
  geom_line(linewidth = 2) + 
  geom_point(size = 5) + 
  scale_x_discrete(name = "Month") + 
  scale_y_continuous(name = "Proportion of tracked birds alive", 
                     limits = c(0, 1)) +
  scale_colour_manual(name = "Dataset", 
                      values = RColorBrewer::brewer.pal(3, "Dark2")) + 
  theme_classic(base_size = 30)
p1

ggsave(p1, filename = "outputs/survival_curves_tracking_data.png", 
       units = "px", height = 4320, width = 7890)
