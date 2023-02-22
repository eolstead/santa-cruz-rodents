# Clean Raw Microsite and Capture Data
# EGO and EKB
# Feb 2023

# PACKAGES and DATA ####

library(tidyverse)
install.packages("rstatix")
library(rstatix)

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


grass <- c("Bermuda grass", "Unknown grass", "Buffelgrass", "Unidentified grass", "Johnson grass", "Unidentified Grama grass", "Johnson grass, bermuda grass", "Bermuda grass / Johnson grass", "Johnson grass / bermuda grass" )
shrubs <- c("Arrowweed", "Cheese bush", "Cheesebush", "Desert broom", "Mesquite", "Salt cedar", "Desert broom/salt cedar")
forbs <- c("Cockleburr", "Cheeseweed burrobush")
sedge_typha <- c("Umbrella flatsedge", "Typha")
mixed <- c("Tall flatsedge / Bermuda mix", "Salt cedar / Typha", "Cheeseweed burrobush/Johnson grass", "Desert broom/Bermuda grass", "Johnson grass, desert broom", "unidentified aster/Bermuda grass", "Smartweed/Typha mix")

microsite <- microsite %>% 
  mutate(Grouped_Veg = case_when(`Type of Vegetation` %in% grass ~ 'grass', 
                                 `Type of Vegetation` %in% shrubs ~ 'shrubs',
                                 `Type of Vegetation` %in% forbs ~ 'forb',
                                 `Type of Vegetation` %in% sedge_typha ~ 'sedge_typha',
                                 TRUE ~ 'mixed'))

joined_data <- full_join(capture, microsite, by=c("Trap ID" = "Trap Location", "Site" = "Site")) %>% 
  select(-`Status (R/N)`:-Handler) %>% 
  filter(Species != "SIOC?", Species != "DIME?", Species != "DI")

contingency_table <- table(joined_data$Species, joined_data$Grouped_Veg)
contingency_table  

chi_square_model <-  chisq.test(contingency_table, simulate.p.value = TRUE)
chi_square_model

pairwise_chisq_gof_test(test)


dimnames(contingency_table) <- list(
  Species = c("CHPE", "DIME", "NEAB", "PEER", "REME", "SIOC"),
  Grouped_Veg = c("forb", "grass", "mixed", "sedge_typha", "shrubs")
)

chisq_test(contingency_table)
test <- contingency_table+1
