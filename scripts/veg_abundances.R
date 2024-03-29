# Vegetation Abundances
# March 2023

# Data and Packages
library(tidyverse)

joined_data <- read_csv("data/joined_data.csv")
microsite <- read_csv("data/microsite_grouped_veg.csv")

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
  count() %>% 
  rename(n_caps = n)

# Plot Abundance at Capture Sites

ggplot(veg_abundance_cap, aes(x = Site, y = n_caps, fill = Grouped_Veg)) +
  geom_bar(stat= 'identity', position='dodge') +
  ylab("Vegetation Type per Capture Site") +
  theme_bw() + scale_fill_discrete(name = "Vegetation Type", labels = c("Forb", "Grass", "Mixed", "Sedge/Typha", "Shrubs"))

ggsave("output/vegetation_type_per_capture_site.png", width = 6, height = 4)

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
  count() %>% 
  mutate(n_trapnights = n * 3)

# Plot Abundance at ALL Sites

ggplot(veg_abundance, aes(x = Site, y = n, fill = Grouped_Veg)) +
  geom_bar(stat= 'identity', position='dodge') +
  ylab("Vegetation Type per Trap Location") +
  theme_bw() + scale_fill_discrete(name = "Vegetation Type", labels = c("Forb", "Grass", "Mixed", "Sedge/Typha", "Shrubs"))

ggsave("output/vegetation_type_per_trap_site.png", width = 6, height = 4)


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


# Proportional Veg Data

veg_abundance_cap2 <- full_join(veg_abundance_cap, veg_abundance) %>% 
  mutate(prop_trapnights_cap = n_caps/n_trapnights)

ggplot(veg_abundance_cap2, aes(x = Site, y = prop_trapnights_cap, fill = Grouped_Veg)) +
  geom_col(position = "dodge") + ylab("Normalized Vegetation Type per Capture Site") +
  theme_bw() + scale_fill_discrete(name = "Vegetation Type", labels = c("Forb", "Grass", "Mixed", "Sedge/Typha", "Shrubs"))

ggsave("output/normalized_vegetation_type_per_capture_site.png", width = 6, height = 4)
