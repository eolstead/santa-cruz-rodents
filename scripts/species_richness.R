# Species Richness
# EGO EKB
# Feb 2023

# Data and Packages
library(tidyverse)

capture <- read_csv("data/capture_raw.csv")

# Calculate Species Richness

# removed unclear species
sp_richness <- capture %>% 
  filter(Species != "SIOC?", Species != "DIME?", Species != "DI")

# Overall Species Richness

# create list of species overall
overall_sp_list <- sp_richness %>% 
  distinct(Species)
# count species overall
overall_sp_count <- count(overall_sp_list)

# Site-Specific Species Richness

# create list of species found per site
site_sp_list <- sp_richness %>% 
  group_by(Site) %>% 
  distinct(Species) 
# count number of species per site
site_sp_count <- site_sp_list %>% 
  group_by(Site) %>% 
  summarise(n_species = n())

# Plot Site-Specific Richness

ggplot(site_sp_count, aes(x = Site, y = n_species, fill = Site)) +
  geom_col() +
  ylab("Species Richness") +
  theme_bw() 
ggsave("output/site_sp_richness.png", width = 4, height = 4)


# Abundance Calculation Without Recaptures

# removed ? and NA
fixed_recaptures <- capture %>% 
  mutate(`Status (R/N)` = stringr::str_remove(`Status (R/N)`, "\\?"),
         `Status (R/N)` = replace_na(`Status (R/N)`,"N"))

fixed_abundance <- filter(fixed_recaptures, `Status (R/N)` != "R",
       Species != "SIOC?", Species != "DIME?", Species != "DI") %>% 
  group_by(Site, Species) %>% 
  count()

fixed_abund_site <- xtabs(n~Site, fixed_abundance)

ggplot(fixed_abundance, aes(x = Site, y = n, fill = Species)) +
  geom_bar(stat= 'identity', position='dodge') +
  ylab("Species Abundance") +
  theme_bw()

ggsave("output/species_abundance_per_site.png", width = 6, height = 4)

fixed_recap_abund <- fixed_recaptures %>% 
  filter(`Status (R/N)` == "R", Species != "SIOC?", Species != "DIME?", Species != "DI") %>% 
group_by(Site, Species) %>% 
  summarize(n_recap=n())

recap_rate <- full_join(fixed_abundance, fixed_recap_abund) %>% 
  mutate(recap_rate = n_recap/n)

shannon_index <- fixed_abundance %>% 
  group_by(Site) %>% 
  mutate(Site_Total=sum(n),
         Proportion=n/Site_Total,
         lnProp=log(Proportion),
         Prop_x_lnProp = Proportion*lnProp) %>% 
  summarise(ShannonIndex=-sum(Prop_x_lnProp))

recapture_rate <- fixed_recaptures %>% 
  group_by(Species, Site)
