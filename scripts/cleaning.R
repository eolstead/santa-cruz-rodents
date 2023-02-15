# Clean Raw Microsite and Capture Data
# EGO and EKB
# Feb 2023

# PACKAGES and DATA ####

library(tidyverse)

# Data
microsite <- read_csv("data/microsite_raw.csv")
capture <- read_csv("data/capture_raw.csv")

# Data Exploration

capture %>% 
  distinct(Species)
