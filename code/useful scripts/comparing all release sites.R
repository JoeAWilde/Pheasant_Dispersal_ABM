library(tidyverse)
library(tidylog)
library(readxl)
library(progress)

sub_df <- readRDS("data/Data for Exeter - anonymised GPSV2/combined_current_tracks.rds") %>%
  filter(
    !(site == "A" & ID == 7 & TimePoint %in% 2341:2343)
  ) %>%
  filter(
    !(site == "A" & ID == 9 & TimePoint %in% 2320:2327)
  ) %>%
  mutate(
    DateTime = ymd_hms(DateTime), 
    month = month.name[month(DateTime)],
    dist_from_pen = sqrt(X_coord^2 + Y_coord^2), 
    sl_ = sqrt(((X_coord - lag(X_coord))^2 + (Y_coord - lag(Y_coord))^2))
  )


summ_sub_df <- data.frame(distance_bins = rep_len(seq(0, 2000, 250), 9 * 12), 
                          Number_of_Birds = 0, 
                          Total_Number_of_Fixes = 0, 
                          Total_Fixes_Overall = 0, 
                          Proportional_Fixes = 0, 
                          month = factor(
                            rep(month.name, each = 9), 
                            levels = month.name[c(7:12, 1:6)])
)

for(i in 1:nrow(summ_sub_df)) {
  if(i == 1)pb <- progress_bar$new(total = nrow(summ_sub_df), format = "[:bar] :percent eta::eta", 
                                   clear = F); pb$tick(0)
  
  if(summ_sub_df$distance_bins[i] == 2000) {
    sub_sub_df <- sub_df %>%
      dplyr::filter(
        month.name[month(DateTime)] == summ_sub_df$month[i] & 
          dist_from_pen >= summ_sub_df$distance_bins[i]
      )
  } else {
    sub_sub_df <- sub_df %>%
      dplyr::filter(
        month.name[month(DateTime)]  == summ_sub_df$month[i] & 
          dist_from_pen >= summ_sub_df$distance_bins[i] &
          dist_from_pen < summ_sub_df$distance_bins[i+1] 
      )
  }
  
  summ_sub_df$Number_of_Birds[i] <- length(unique(sub_sub_df$ID))
  summ_sub_df$Total_Number_of_Fixes[i] <- nrow(sub_sub_df)
  summ_sub_df$Total_Fixes_Overall[i] <- nrow(sub_df %>%
                                                 dplyr::filter(
                                                   month.name[month(DateTime)] == summ_sub_df$month[i]
                                                 ))
  summ_sub_df$Proportional_Fixes[i] <- summ_sub_df$Total_Number_of_Fixes[i] / 
    summ_sub_df$Total_Fixes_Overall[i]
  
  pb$tick()
}

summ_sub_df <- summ_sub_df %>%
  mutate(data_type = "subset", 
         no_sites = 3)

# for(i in list.files("data/all_ranging_data/remaining sites/")) {
#   ith_data <- read.table(paste0("data/all_ranging_data/remaining sites/", i), sep = ",", header = T)
#   
#   if(i == list.files("data/all_ranging_data/remaining sites/")[1]) {
#     rem_sites <- ith_data
#   } else {
#     rem_sites <- rbind(rem_sites, ith_data)
#   }
# }; rm(ith_data)

rem_sites <- read_xlsx("data/all_ranging_data/Monthly Ranging Profile Summary - Complete V1.xlsx")

all_df <- rem_sites %>%
  mutate(
    month = factor(
      month.name[as.integer(substr(Year_Month, 6, 8))], 
      levels = month.name[c(7:12, 1:6)]), 
    distance_bins = if_else(substr(distance_bins, 1, 4) %in% as.character(seq(2000, 9000, 250)), 
                            "2000", distance_bins)) %>%
  group_by(distance_bins, month) %>%
  mutate(Number_of_Birds = sum(Number_of_Birds), 
         Total_Number_of_Fixes = sum(Total_Number_of_Fixes), 
         Total_Fixes_Overall = sum(Total_Fixes_Overall)
  ) %>%
  ungroup() %>%
  mutate(
    distance_bins = case_when(
      substr(distance_bins, 1, 1) == "0" ~ 0, 
      substr(distance_bins, 1, 2) == "25" ~ 250, 
      substr(distance_bins, 1, 1) == "5" ~ 500, 
      substr(distance_bins, 1, 1) == "7" ~ 750, 
      substr(distance_bins, 1, 2) == "10" ~ 1000, 
      substr(distance_bins, 1, 2) == "12" ~ 1250, 
      substr(distance_bins, 1, 2) == "15" ~ 1500, 
      substr(distance_bins, 1, 2) == "17" ~ 1750, 
      substr(distance_bins, 1, 2) == "20" ~ 2000
    ), 
    data_type = "new", 
    no_sites = 11
  ) %>%
  select(-c(Year_Month, bin_order)) %>%
  rbind(., data.frame(
    distance_bins = rep_len(seq(0, 2000, 250), 9 * 12),
    Number_of_Birds = 0, 
    Total_Number_of_Fixes = 0, 
    Total_Fixes_Overall = 0, 
    Proportional_Fixes = 0, 
    month = rep(month.name, each = 9), 
    data_type = "new", 
    no_sites = 11
  )) %>%
  distinct(distance_bins, month, .keep_all = T) %>%
  arrange(month, distance_bins)


df <- rbind(summ_sub_df %>%
              arrange(month, distance_bins),
            all_df) %>%
  mutate(
    distance_str = case_when(
      distance_bins == 2000 ~ "2000m+", 
      distance_bins != 2000 ~ paste0(distance_bins, "-", lead(distance_bins), "m")
    )
  ) %>%
  mutate(
    distance_str = factor(distance_str, 
                          levels = unique(distance_str)
    )
  ) %>%
  filter(!month %in% month.name[3:6])

saveRDS(all_df, "shiny_app/summarised tracking data/APHA data/all_APHA_sites.rds")

# p1 <- ggplot(data = df, aes(x = distance_str, y = Total_Number_of_Fixes / no_sites, group = data_type, fill = data_type)) + 
#   geom_col(position = position_dodge()) + 
#   scale_fill_manual(name = "Data set", 
#                     values = c("firebrick", "navy"), 
#                     labels = c("Other sites", "Sites A, B & D")) + 
#   scale_x_discrete(name = "Distance band") + 
#   scale_y_continuous(name = "Per-site average number of fixes") + 
#   theme_classic(base_size = 30) + 
#   theme(legend.key.width = unit(1, "cm"),
#         axis.text.x=element_text(size=15)) + 
#   facet_wrap(vars(month))
# p1
# 
# ggsave(p1, filename = "data/all_ranging_data/abs_average_fixes_all.png", units = "px", height = 6320, width = 11980)
# 
# p2 <- ggplot(data = df, aes(x = distance_str, y = Proportional_Fixes, group = data_type, fill = data_type)) + 
#   geom_col(position = position_dodge()) + 
#   scale_fill_manual(name = "Data set", 
#                     values = c("firebrick", "navy"), 
#                     labels = c("Other sites", "Sites A, B & D")) + 
#   scale_x_discrete(name = "Distance band") + 
#   scale_y_continuous(name = "Proportion of total monthly fixes") + 
#   theme_classic(base_size = 30) + 
#   theme(legend.key.width = unit(1, "cm"),
#         axis.text.x=element_text(size=15)) + 
#   facet_wrap(vars(month))
# p2
# 
# ggsave(p2, filename = "data/all_ranging_data/prop_fixes_all.png", units = "px", height = 6320, width = 11980)
# 
# 
# p3 <- ggplot(data = df, aes(x = distance_str, y = (Total_Number_of_Fixes / no_sites) / Total_Fixes_Overall,
#                             group = data_type, fill = data_type)) + 
#   geom_col(position = position_dodge()) + 
#   scale_fill_manual(name = "Data set", 
#                     values = c("firebrick", "navy"), 
#                     labels = c("Other sites", "Sites A, B & D")) + 
#   scale_x_discrete(name = "Distance band") + 
#   scale_y_continuous(name = "Proportion of per-site average monthly fixes") + 
#   theme_classic(base_size = 30) + 
#   theme(legend.key.width = unit(1, "cm"),
#         axis.text.x=element_text(size=15)) + 
#   facet_wrap(vars(month))
# p3
# 
# ggsave(p3, filename = "data/all_ranging_data/prop_avg_fixes_all.png", units = "px", height = 6320, width = 11980)
