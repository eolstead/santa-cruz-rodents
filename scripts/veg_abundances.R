# Vegetation Abundances
# March 2023

# Data and Packages
library(tidyverse)

joined_data <- read_csv("data/joined_data.csv")
microsite <- read_csv("data/microsite_raw.csv")

# VEGETATION AT CAPTURE SITES ####

# Calculate Species Richness

vegCat_list_cap <- joined_data %>% 
  distinct(Grouped_Veg)

# create list of species found per site
site_veg_list_cap <- joined_data %>% 
  group_by(Site) %>% 
  distinct(Grouped_Veg) 
# count number of species per site
site_veg_count_cap <- site_veg_list_cap %>% 
  group_by(Site) %>% 
  summarise(n_Grouped_Veg = n())

# Calculate Vegetation Abundance at Capture Sites

veg_abundance_cap <- joined_data %>% 
  group_by(Site,Grouped_Veg) %>% 
  count()

# Plot Abundance at Capture Sites

ggplot(veg_abundance_cap, aes(x = Site, y = n, fill = Grouped_Veg)) +
  geom_bar(stat= 'identity', position='dodge') +
  ylab("Vegetation Type per Trap Site") + ggtitle("Vegetation Types at Capture Trap Locations per Site") +
  theme_bw()


# VEGETATION AT ALL SITES ####

# Calculate Species Richness

vegCat_list <- microsite %>% 
  distinct(Grouped_Veg)

# create list of species found per site
site_veg_list <- microsite %>% 
  group_by(Site) %>% 
  distinct(Grouped_Veg) 
# count number of species per site
site_veg_count <- site_veg_list %>% 
  group_by(Site) %>% 
  summarise(n_Grouped_Veg = n())

# Calculate Vegetation Abundance at ALL Sites

veg_abundance <- microsite %>% 
  group_by(Site,Grouped_Veg) %>% 
  count()

# Plot Abundance at ALL Sites

ggplot(veg_abundance, aes(x = Site, y = n, fill = Grouped_Veg)) +
  geom_bar(stat= 'identity', position='dodge') +
  ylab("Vegetation Type per Trap Location") + ggtitle("Vegetation Types at Trap Locations per Site") +
  theme_bw()


# Shannon Index for All Sites and Capture Sites
veg_abundance %>% 
  group_by(Site) %>% 
  mutate(Site_Total=sum(n),
         Proportion=n/Site_Total,
         lnProp=log(Proportion),
         Prop_x_lnProp = Proportion*lnProp) %>% 
  summarise(ShannonIndex=-sum(Prop_x_lnProp))

veg_abundance_cap %>% 
  group_by(Site) %>% 
  mutate(Site_Total=sum(n),
         Proportion=n/Site_Total,
         lnProp=log(Proportion),
         Prop_x_lnProp = Proportion*lnProp) %>% 
  summarise(ShannonIndex=-sum(Prop_x_lnProp))
