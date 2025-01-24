#Script to run regressions to find the distributions of step lengths and turning angles to use in the simulation

#load required libraries
library(tidyverse)
library(progress)
library(brms)
library(cmdstanr)
library(terra)
library(bayesplot)

# Functions ####
#create ugly function that bounds turning angle to -pi to pi
angle_bounds_neg <- function(ta) {
  tabounded <- ifelse(ta > pi, 
                      ((2*pi) - ta)*-1, 
                      ifelse(ta < -pi, 
                             ((-2*pi) - ta)*-1, 
                             ta))
  return(tabounded)
}

angle_bounds_pos <- function(ta) {
  tabounded <- ifelse(ta < 0, 
                      (2*pi) + ta, 
                      ta)
  return(tabounded)
}

# ATLAS data ####

## load in the habitat data ####
hab <- rast("outputs/script_1/ATLAS outputs/cropped release pen habitat raster.tif")

## load in the tracking data, subsample and calculate step length and turning angle ####

atlas <- read.table("data/ATLAS data/GPS data/Pheas_filtered.csv", sep = ",", header = T) %>%
  select(-dist) %>%
  mutate(DateTime = ymd_hms(DateTime), 
         Deploydatetime = ymd_hms(Deploydatetime)) %>%
  filter(year(Deploydatetime) == "2018") %>%
  group_by(ID) %>%
  filter(minute(DateTime) == minute(DateTime[1])) %>%
  mutate(
    sl = sqrt((X - lag(X))^2 + ((Y - lag(Y))^2)), 
    ta = atan2(Y - lag(Y), X - lag(X)), 
    difftime = as.numeric(difftime(DateTime, lag(DateTime), units = "days")),
    sl = if_else(difftime > 0.05, NA, sl), 
    sl = if_else(sl > 1000, NA, sl), 
    SinceRel = as.numeric(difftime(DateTime, DateTime[1], units = "days"))
  )%>%
  ungroup() %>%
  mutate(hab = terra::extract(x = hab, y = as.matrix(cbind(.$X, .$Y)), cells = F)[ , 1], 
         site = "NORTHWYKE")


# APHA data ####

for(ss in c("A", "B", "D")) {
  
  ## load in the habitat data ####
  hab <- rast(paste0("outputs/script_1/APHA outputs/site ",
                     ss, "/site ", ss, " cropped release pen habitat raster.tif"))
  
  ## load in the tracking data, subsample and calculate step length and turning angle ####
  apha_ss <- readRDS(paste0("data/APHA data (anonymised)/GPS data/combined_current_tracks.rds")) %>%
    filter(site == ss) %>% 
    rename(X = X_coord, 
           Y = Y_coord) %>%
    group_by(ID) %>%
    mutate(
      sl = sqrt((X - lag(X))^2 + ((Y - lag(Y))^2)), 
      ta = atan2(Y - lag(Y), X - lag(X)), 
      difftime = as.numeric(difftime(DateTime, lag(DateTime), units = "days")),
      sl = if_else(difftime > 0.05, NA, sl), 
      sl = if_else(sl > 1000, NA, sl), 
      SinceRel = as.numeric(difftime(DateTime, DateTime[1], units = "days"))
    ) %>%
    ungroup() %>%
    mutate(hab = terra::extract(x = hab, y = as.matrix(cbind(.$X, .$Y)), cells = F)[ , 1],
           ID = paste0(ID, site))
  
  ## remove anomalous points and rebind into one df ####
  if(ss == "A") {
    rm_index <- c(
      which(apha_ss$ID == "7A" & apha_ss$TimePoint %in% 2341:2343), 
      which(apha_ss$ID == "9A" & apha_ss$TimePoint %in% 2320:2327)
    )
    apha_ss <- apha_ss[-rm_index, ]
    apha <- apha_ss
  } else {
    apha <- rbind(apha, apha_ss)
  }
};rm(apha_ss)


# Combined data ####

##combine the data frames with mathcing column names ####
df <- rbind(
  atlas %>%
    select(names(.)[which(names(.) %in% names(apha))]), 
  apha %>%
    select(names(.)[which(names(.) %in% names(atlas))])
)


## substitute sl==0 for a very small constant ####
df$sl <- ifelse(df$sl == 0, 0.00000001, df$sl)

## ensure habitat is a factor ####
df$hab <- factor(df$hab)


# Step length regression ####

## set priors ####
priors <- prior(normal(0, 1), class = b) + prior(exponential(1), class = sd)

## run the regression ####
m1 <- brm(
  formula = bf("sl ~ hab + SinceRel + (1|ID) + (1|site)"),
  data = df,
  family = "gamma",
  iter = 3000,
  warmup = floor(3000/2),
  chains = 4,
  cores = 4,
  # backend = "cmdstan",
  prior = priors
); saveRDS(m1, "outputs/script_2/sl_regress_rp.rds")

# Turning angle regression ####

## bind turning angle between 0 and 2pi ####
df$ta_pos <- angle_bounds_pos(df$ta)

## run the regression ####
m2 <- brm(
  formula = bf("ta ~ hab + SinceRel + (1|ID) + (1|site)"), 
  data = df, 
  family = von_mises(),
  iter = 3000, 
  warmup = (3000/2), 
  chains = 4, 
  cores = 4#, 
  # backend = "cmdstan"
); saveRDS(m1, "outputs/script_2/ta_regress_rp.rds")

