library(shiny)
library(tidyverse)
library(bslib)

# Data preparation (as before)
df <- readRDS("simulation_and_all_tracking_summarised_data.rds") %>%
  mutate(month = factor(month, levels = month.name[c(7:12, 1:6)])) %>%
  arrange(sites, month, dist_from_pen_band) %>%
  mutate(distance_str = if_else(dist_from_pen_band == 2000, "2000+", paste0(dist_from_pen_band, "-",
                                                                            lead(dist_from_pen_band))), 
         fixes_per_site = mean_fixes / no_sites) %>%
  mutate(distance_str = factor(distance_str, levels = c("0-250", "250-500", "500-750",
                                                        "750-1000", "1000-1250",
                                                        "1250-1500","1500-1750",
                                                        "1750-2000", "2000+"))) %>%
  filter(month %in% month.name[c(1:2, 7:12)])



ui <- page_sidebar(
  
  # App title ----
  title ="Pheasant dispersal comparison",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    # Input: Select the random distribution type ----
    
    checkboxGroupInput(inputId = "sites1",
                       label = "Select simulation data:",
                       choices = sort(unique(as.character(df$sites[df$data_type == "sim"])))), 
    br(),
    checkboxGroupInput(
      inputId = "sites2",
      label = "Select tracking data:",
      choices = sort(unique(as.character(df$sites[df$data_type == "real"]))))
    
    
  ),
  
  
  
  # Main panel for displaying outputs ----
  # Output: A tabset that combines three panels ----
  navset_card_underline(
    title = "Type of y-axis scaling",
    
    nav_panel("Absolute fixes", plotOutput("abs_fix")),
    
    nav_panel("Fixes per site", plotOutput("fix_per_site")),
    
    nav_panel("Proportion of monthly fixes", plotOutput("prop_mon_fix")),
    
    nav_panel("Proportion of total fixes", plotOutput("prop_tot_fix")),
    
    nav_panel("Monthly prop. per m^2", plotOutput("fix_sq_m")),
    
    nav_panel("Monthly prop. per m^2 with mortality", plotOutput("fix_sq_mort")),
    
    nav_panel("Mortality", plotOutput("mort_plot")
    )
  )
)
