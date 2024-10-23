library(tidyverse)

ss <- c("Ex")
root <- "shiny_app/summarised simulation data/"
files <- paste0(root, list.files(root)) %>%
  .[grepl(ss, .)]

all_data <- lapply(files, readRDS) %>%
  do.call(rbind , .) %>%
  filter(month %in% month.name[c(7:12, 1:2)]) %>%
  mutate(sites = factor(sites, levels = c("Exmoor 0m away from PA boundary", 
                                          "Exmoor 250m away from PA boundary", 
                                          "Exmoor 500m away from PA boundary", 
                                          "Exmoor 1000m away from PA boundary", 
                                          "Exmoor 2000m away from PA boundary")))

saveRDS(all_data, "PA_sites_shiny_app/app/Ex_all_PA_site_data.rds")
