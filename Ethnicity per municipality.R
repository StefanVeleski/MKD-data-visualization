# Loading libraries

library(raster) 
library(tidyverse)
library(sf)
library(geodata)
library(ggthemes)
library(plotly)
library(scales)
library(readr)

# Loading datasets

ethnicity_2021 <- read_csv("Datasets/Ethnicity data/Ethnicity per municipality.csv")
census_2021 <- read_csv("Datasets/Population data/census_2021.csv") 
census_2002 <- read_csv("Datasets/Population data/census_2002.csv")

# Getting the province shape file for Macedonia

macedonia <- getData('GADM', country='MKD', level=1) %>%
  st_as_sf() # Converting it into an sf object

# Adding census_2022 dataset sf object and making sure each column is in the correct data type

macedonia <- bind_cols(macedonia, census_2002) 

macedonia$Municipality <- as.factor(macedonia$Municipality)
macedonia$`Total resident population` <- as.numeric(macedonia$`Total resident population`)

# Data wrangling, removing unnecessary boundaries, and adjusting boundaries according to 2013 municipal reform

macedonia_new <- macedonia %>%
  filter(!Municipality %in% c("Lake Ohrid")) %>% # Remove Lake Ohrid boundaries
  mutate(Municipality = fct_collapse(Municipality, Kichevo = c("Drugovo", "Zajas", "Kichevo","Oslomej","Vraneshtica"))) %>% # Merge Drugovo, Zajas, Oslomej and Vraneshtica into Kichevo
  group_by(Municipality) %>%
  summarise(`Total resident population` = sum(`Total resident population`))

macedonia_new$`Total resident population 2021` <- census_2021$`Total resident population`

# Adding a variable with the percentage change between the two censuses

macedonia_new$Change <- (macedonia_new$`Total resident population 2021`-macedonia_new$`Total resident population`)/macedonia_new$`Total resident population`

macedonia_new$Change <- round(macedonia_new$Change, 4) # Round to four decimal places

# Joining the two datasets

ethnicity_data <- left_join(ethnicity_2021, macedonia_new, by = "Municipality")

# Removing unnecessary columns

ethnicity_data <- ethnicity_data %>%
  select(Ethnicity, Total, Municipality, geometry, `Total resident population 2021`)

# Removing scientific notation

options(scipen=999)

# Creating a column with the proportion of the given ethnicity of the entire population in the municipality

ethnicity_data$Percentage <- ethnicity_data$Total/ethnicity_data$`Total resident population 2021`

# Rounding it to four decimal places
ethnicity_data$Percentage <- round(ethnicity_data$Percentage, 4)

# Changing the Admin factor to "Census boycotters". Most of these are ethnic Macedonians. 
ethnicity_data$Ethnicity <- recode_factor(ethnicity_data$Ethnicity, Admin = "No data (undercount)")

ethnicity_data <- ethnicity_data %>% 
  filter(!Ethnicity %in% c("Total", "Undeclared", "Unknown")) %>% # Remove unnecessary factor levels
  mutate(across (Ethnicity, factor, levels = c("Macedonians", "Albanians", "Turks", "Roma", "Serbs", "Bosniaks", "Vlachs", "Other", "No data (undercount)"))) # Arrange factor levels according to their size

p <- ggplot(ethnicity_data) +
  geom_sf(aes(text = paste0("Municipality: ", Municipality, "\nTotal population: ", Total), fill = Percentage, geometry = geometry)) + # Adding hover text and choosing a variable to determine the color
  scale_fill_distiller(type = "seq", palette = "YlOrBr", direction = 1,labels = percent) + # Set color scheme 
  labs(title = "The ethnic makeup of Macedonia according to the 2021 census") + # Add title
  theme(axis.text.x = element_blank(), # Remove unnecessary elements of plot
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  facet_wrap(~Ethnicity) + # Creating one plot per factor level
  theme(strip.text.x = element_text(size = 14)) + # Facet title font increase
  theme(plot.title = element_text(size=18)) # Plot title font increase

p

ggsave(file = "Plots/The ethnic makeup of Macedonia according to the 2021 census.jpg", dpi = 300)


ggplotly(p) # Creating an interactive plot