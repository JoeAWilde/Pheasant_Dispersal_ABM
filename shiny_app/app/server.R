server <- function(input, output) {
  require(tidyverse)
  require(RColorBrewer)
  library(bslib)
  
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
    ungroup()
    
    sub_df <- reactive({
      subset(df, sites %in% c(input$sites1, input$sites2))
      
    })
    
    numer_of_sites <- length(unique(df$sites))
    
    output$abs_fix <- renderPlot({
      if(length(c(input$sites1, input$sites2)) == 0) {
        
        
        p1 <- ggplot()
        
      } else {
        
        p1 <- ggplot(data = sub_df()) +
          geom_col(aes(x = distance_str, y = mean_fixes, fill = sites,
                       group = sites), position = position_dodge()) +
          geom_errorbar(aes(x = distance_str, ymin=sd_low_fixes, ymax=sd_high_fixes,
                            group = sites),
                        position="dodge") +
          geom_point(aes(x = distance_str, y = min_fixes, group = sites), shape = 1, position = position_dodge(width=0.9)) +
          geom_point(aes(x = distance_str, y = max_fixes, group = sites), shape = 1, position = position_dodge(width=0.9)) +
          scale_y_continuous(name = "Number of fixes") +
          scale_x_discrete(name = "Distance from Pen (m)") +
          scale_fill_manual(name = "Data type", values = brewer.pal(numer_of_sites, "Dark2")) +
          theme_classic(base_size = 20) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          facet_wrap(vars(month))
      }
      p1
    }, height = 689, width = 1550)
    
    output$fix_per_site <- renderPlot({
      if(length(c(input$sites1, input$sites2)) == 0) {
        
        
        p1 <- ggplot()
        
      } else {
        
        p1 <- ggplot(data = sub_df()) +
          geom_col(aes(x = distance_str, y = mean_fixes / no_sites, fill = sites,
                       group = sites), position = position_dodge()) +
          geom_errorbar(aes(x = distance_str, ymin=sd_low_fixes/ no_sites, ymax=sd_high_fixes/ no_sites,
                            group = sites),
                        position="dodge") +
          geom_point(aes(x = distance_str, y = min_fixes/ no_sites, group = sites), shape = 1, position = position_dodge(width=0.9)) +
          geom_point(aes(x = distance_str, y = max_fixes/ no_sites, group = sites), shape = 1, position = position_dodge(width=0.9)) +
          scale_y_continuous(name = "Number of fixes / site") +
          scale_x_discrete(name = "Distance from Pen (m)") +
          scale_fill_manual(name = "Data type", values = brewer.pal(numer_of_sites, "Dark2")) +
          theme_classic(base_size = 20) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          facet_wrap(vars(month))
      }
      p1
    }, height = 689, width = 1550)
    
    
    output$prop_mon_fix <- renderPlot({
      if(length(c(input$sites1, input$sites2)) == 0) {
        
        
        p1 <- ggplot()
        
      } else {
        
        p1 <- ggplot(data = sub_df()) +
          geom_col(aes(x = distance_str, y = mean_prop, fill = sites,
                       group = sites), position = position_dodge()) +
          geom_errorbar(aes(x = distance_str, ymin=sd_low_prop, ymax=sd_high_prop,
                            group = sites),
                        position="dodge") +
          geom_point(aes(x = distance_str, y = min_prop, group = sites), shape = 1, position = position_dodge(width=0.9)) +
          geom_point(aes(x = distance_str, y = max_prop, group = sites), shape = 1, position = position_dodge(width=0.9)) +
          scale_y_continuous(name = "Proportion of monthly fixes", limits = c(0, 1)) +
          scale_x_discrete(name = "Distance from Pen (m)") +
          scale_fill_manual(name = "Data type", values = brewer.pal(numer_of_sites, "Dark2")) +
          theme_classic(base_size = 20) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          facet_wrap(vars(month))
      }
      p1
    }, height = 689, width = 1550)
    
    
    output$prop_tot_fix <- renderPlot({
      if(length(c(input$sites1, input$sites2)) == 0) {
        
        
        p1 <- ggplot()
        
      } else {
        
        p1 <- ggplot(data = sub_df()) +
          geom_col(aes(x = distance_str, y = mean_prop_total_fixes, fill = sites,
                       group = sites), position = position_dodge()) +
          geom_errorbar(aes(x = distance_str, ymin=sd_low_prop_total_fixes, ymax=sd_high_prop_total_fixes,
                            group = sites),
                        position="dodge") +
          geom_point(aes(x = distance_str, y = min_prop_total_fixes, group = sites), 
                     shape = 1, position = position_dodge(width=0.9)) +
          geom_point(aes(x = distance_str, y = max_prop_total_fixes, group = sites), 
                     shape = 1, position = position_dodge(width=0.9)) +
          scale_y_continuous(name = "Proportion of total yearly fixes") +
          scale_x_discrete(name = "Distance from Pen (m)") +
          scale_fill_manual(name = "Data type", values = brewer.pal(numer_of_sites, "Dark2")) +
          theme_classic(base_size = 20) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          facet_wrap(vars(month))
      }
      p1
    }, height = 689, width = 1550)
    
    output$fix_sq_m <- renderPlot({
      if(length(c(input$sites1, input$sites2)) == 0) {
        
        
        p1 <- ggplot()
        
      } else {
        
        p1 <- ggplot(data = sub_df()) +
          geom_col(aes(x = distance_str, y = mean_prop / band_area, fill = sites,
                       group = sites), position = position_dodge()) +
          geom_errorbar(aes(x = distance_str, ymin=sd_low_prop / band_area, 
                            ymax=sd_high_prop / band_area,
                            group = sites),
                        position="dodge") +
          geom_point(aes(x = distance_str, y = min_prop / band_area, group = sites), shape = 1, position = position_dodge(width=0.9)) +
          geom_point(aes(x = distance_str, y = max_prop / band_area, group = sites), shape = 1, position = position_dodge(width=0.9)) +
          scale_y_continuous(name = "Prop. monthly fixes / m^2") +
          scale_x_discrete(name = "Distance from Pen (m)") +
          scale_fill_manual(name = "Data type", values = brewer.pal(numer_of_sites, "Dark2")) +
          theme_classic(base_size = 20) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          facet_wrap(vars(month))
      }
      p1
    }, height = 689, width = 1550)
    
    output$fix_sq_mort <- renderPlot({
      if(length(c(input$sites1, input$sites2)) == 0) {
        
        
        p1 <- ggplot()
        
      } else {
        
        p1 <- ggplot(data = sub_df()) +
          geom_col(aes(x = distance_str, y = (mean_prop / band_area) * prop_birds_alive, fill = sites,
                       group = sites), position = position_dodge()) +
          geom_errorbar(aes(x = distance_str, ymin=(sd_low_prop / band_area) * prop_birds_alive, 
                            ymax=(sd_high_prop / band_area) * prop_birds_alive,
                            group = sites),
                        position="dodge") +
          geom_point(aes(x = distance_str, y = (min_prop / band_area) * prop_birds_alive, group = sites), shape = 1, position = position_dodge(width=0.9)) +
          geom_point(aes(x = distance_str, y = (max_prop / band_area) * prop_birds_alive, group = sites), shape = 1, position = position_dodge(width=0.9)) +
          scale_y_continuous(name = "(Prop. monthly fixes / m^2) * Prop. birds alive") +
          scale_x_discrete(name = "Distance from Pen (m)") +
          scale_fill_manual(name = "Data type", values = brewer.pal(numer_of_sites, "Dark2")) +
          theme_classic(base_size = 20) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          facet_wrap(vars(month))
      }
      p1
    }, height = 689, width = 1550)
    
    output$mort_plot <- renderPlot({
      if(length(c(input$sites1, input$sites2)) == 0) {
        p2 <- ggplot()
      } else {
        p2 <- ggplot(data = sub_df()) +
          geom_line(aes(x = month, y = prop_birds_alive, color = sites, group = sites)) +
          geom_point(aes(x = month, y = prop_birds_alive, color = sites, group = sites),
                     position = position_dodge(width=0.9)) +
          geom_errorbar(aes(x = month, ymin=(mean_birds - sd_birds) / total_birds_released,
                            ymax= (mean_birds + sd_birds) / total_birds_released,
                            group = sites),
                        position="dodge") +
          scale_y_continuous(name = "Number of birds still alive") +
          scale_x_discrete(name = "Month") +
          scale_color_manual(name = "Data type", values = brewer.pal(numer_of_sites, "Set1")) +
          theme_classic(base_size = 16) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      p2
    }, height = 689, width = 1550)
    }
