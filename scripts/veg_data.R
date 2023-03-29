# Clean Raw Microsite and Capture Data
# EGO and EKB
# Feb 2023

# PACKAGES and DATA ####

# Packages
install.packages("chisq.posthoc.test")
install.packages("nnet")
install.packages("stargazer")

library(tidyverse)
library(chisq.posthoc.test)
library(nnet)
library(stargazer)

# Data
microsite <- read_csv("data/microsite_raw.csv")
capture <- read_csv("data/capture_raw.csv")

# Data Exploration

capture %>% 
  distinct(Species)

microsite %>% 
  distinct(`Type of Vegetation`) %>% 
  print(n=30)


# DATA WRANGLING ####

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
                            `Type of Vegetation` == "Unidentified Grama grass" ~ 'Y',
                            `Type of Vegetation` %in% c("Unknown grass", "Unidentified grass") ~ NA_character_,
                            TRUE ~ 'N')
  )                              

# Create groupings for vegetation data

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
  filter(Species != "SIOC?", Species != "DIME?", Species != "DI") # this filter function removes NAs!
# write_csv(joined_data, "data/joined_data.csv")

# make a contingency table
contingency_table <- table(joined_data$Species, joined_data$Grouped_Veg)

dimnames <- list(
  Species = c("CHPE", "DIME", "NEAB", "PEER", "REME", "SIOC"),
  Grouped_Veg = c("forb", "grass", "mixed", "sedge_typha", "shrubs")
)

contingency_table
props <- as.data.frame(prop.table(contingency_table))
colnames(props) <- c("Species", "Vegetation", "Freq")

ggplot(props, aes(x = Species, y = Freq, fill = Vegetation)) +
  geom_col() +
  theme_classic()

ggsave("output/frequency_of_species_and_vegetation.png", width = 6, height = 4)

# ANALYSIS ####

# Run global chi-square test
Xsq_global <- chisq.test(contingency_table)
Xsq_global

# Run post-hoc test
Xsq_posthoc <- chisq.posthoc.test(contingency_table)
Xsq_posthoc

# Multinomial regression
# working from this pdf: https://www.princeton.edu/~otorres/LogitR101.pdf
reg_model <- multinom(Species ~ Grouped_Veg + `Percent Veg Cover` + Native, data = joined_data)
reg_model

stargazer(reg_model, type = "html", out = "output/regression_model_output.htm")

# relative risk ratios (?)
reg_model_rrr <- exp(coef(reg_model))
reg_model_rrr

stargazer(reg_model_rrr, type = "html", coef = list(reg_model_rrr), p.auto = FALSE, out = "output/reg_model_rrr.htm")

# same thing with only cover and native/non-native
reg_model_noveg <- multinom(Species ~ `Percent Veg Cover` + Native, data = joined_data)
reg_model_noveg

stargazer(reg_model_noveg, type = "html", out = "output/reg_model_noveg_output.htm")

# relative risk ratios (?)
reg_model_noveg_rrr <- exp(coef(reg_model_noveg))
reg_model_noveg_rrr

stargazer(reg_model_noveg_rrr, type = "html", coef = list(reg_model_noveg_rrr), p.auto = FALSE, out = "output/reg_model_noveg_rrr.htm")

