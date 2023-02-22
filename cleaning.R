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

microsite %>% 
  distinct(`Type of Vegetation`) %>% 
  print(n=30)


# Clean Vegetation Data

# create introduced column in microsite
microsite <- microsite %>%
  mutate(Introduced = case_when(`Type of Vegetation` == "Bermuda grass" ~ 'Y', 
                                `Type of Vegetation` == "Salt cedar" ~ 'Y', 
                                `Type of Vegetation` == "Buffelgrass" ~ 'Y',
                                `Type of Vegetation` == "Johnson grass" ~ 'Y',
                                `Type of Vegetation` == "Johnson grass, bermuda grass" ~ 'Y',
                                `Type of Vegetation` == "Tall flatsedge / Bermuda mix" ~ 'Y',
                                `Type of Vegetation` == "Salt cedar / Typha" ~ 'Y',
                                `Type of Vegetation` == "Cheeseweed burrobush" ~ 'Y',
                                `Type of Vegetation` == "Desert broom/salt cedar" ~ 'Y',
                                `Type of Vegetation` == "Cheeseweed burrobush/Johnson grass" ~ 'Y',
                                `Type of Vegetation` == "Desert broom/Bermuda grass" ~ 'Y',
                                `Type of Vegetation` == "Johnson grass, desert broom" ~ 'Y',
                                `Type of Vegetation` == "Johnson grass / bermuda grass" ~ 'Y',
                                `Type of Vegetation` == "Bermuda grass / Johnson grass" ~ 'Y',
                                `Type of Vegetation` == "Unidentified aster/Bermuda grass" ~ 'Y',
                                TRUE ~ 'N')
          )

# create native column in microsite
microsite <- microsite %>%
  mutate(Native = case_when(`Type of Vegetation` == "Cheese bush" ~ 'Y', 
                            `Type of Vegetation` == "Cockleburr" ~ 'Y', 
                            `Type of Vegetation` == "Cheesebush" ~ 'Y',
                            `Type of Vegetation` == "Typha" ~ 'Y',
                            `Type of Vegetation` == "Smartweed/Typha mix" ~ 'Y',
                            `Type of Vegetation` == "Tall flatsedge / Bermuda mix" ~ 'Y',
                            `Type of Vegetation` == "Desert broom" ~ 'Y',
                            `Type of Vegetation` == "Salt cedar / Typha" ~ 'Y',
                            `Type of Vegetation` == "Arrowweed" ~ 'Y',
                            `Type of Vegetation` == "Mesquite" ~ 'Y',
                            `Type of Vegetation` == "Cheeseweed burrobush" ~ 'Y',
                            `Type of Vegetation` == "Desert broom/salt cedar" ~ 'Y',
                            `Type of Vegetation` == "Cheeseweed burrobush/Johnson grass" ~ 'Y',
                            `Type of Vegetation` == "Desert broom/Bermuda grass" ~ 'Y',
                            `Type of Vegetation` == "Johnson grass, desert broom" ~ 'Y',
                            TRUE ~ 'N')
  )                              

# create unkown column in microsite
microsite <- microsite %>%
  mutate(Unknown = case_when(`Type of Vegetation` == "Unknown grass" ~ 'Y', 
                             `Type of Vegetation` == "Unidentified grass" ~ 'Y', 
                             `Type of Vegetation` == "Unidentified Grama grass" ~ 'Y',
                             TRUE ~ 'N')
  )

