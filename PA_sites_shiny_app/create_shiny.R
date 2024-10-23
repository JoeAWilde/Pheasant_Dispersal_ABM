library(shiny)
library(bslib)

app_path <- ifelse(dir.exists("C:/Users/jw1152/"),
                   "C:/Users/jw1152/OneDrive - University of Exeter/Luke Pheasant modelling/Joe/main_model/shiny_app/app/", 
                   "C:/Users/Owner/OneDrive - University of Exeter/Luke Pheasant modelling/Joe/main_model/shiny_app/app/")

shinyAppDir(app_path)

rsconnect::deployApp(
  appDir = app_path,
  appName = "pheasant_dispersal",
  appTitle = "Pheasant Dispersal")
